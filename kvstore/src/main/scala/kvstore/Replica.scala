package kvstore

import java.util.concurrent.Executors

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.util.Timeout
import kvstore.Arbiter._
import kvstore.PersistPerformer.PersistPerformed
import akka.pattern.ask

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object Replica {

    sealed trait Operation {

        def key: String

        def id: Long
    }

    case class Insert(key: String, value: String, id: Long) extends Operation

    case class Remove(key: String, id: Long) extends Operation

    case class Get(key: String, id: Long) extends Operation

    sealed trait OperationReply

    case class OperationAck(id: Long) extends OperationReply

    case class OperationFailed(id: Long) extends OperationReply

    case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

    def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {

    import Persistence._
    import Replica._
    import Replicator._

    /*
     * The contents of this actor is just a suggestion, you can implement it in any way you like.
     */

    @throws[Exception](classOf[Exception])
    override def preStart(): Unit = arbiter ! Join

    var kv = Map.empty[String, String]
    // a map from secondary replicas to replicators
    var secondaries = Map.empty[ActorRef, ActorRef]
    // the current set of replicators
    var replicators = Set.empty[ActorRef]

    val persiter = context.actorOf(persistenceProps)

    def receive = {

        case JoinedPrimary => context.become(leader)
        case JoinedSecondary => context.become(replica(0L))
    }

    implicit val ctx = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

    /* TODO Behavior for  the leader role. */
    val leader: Receive = {
        case Get(key, id) => sender ! GetResult(key, kv.get(key), id)
        case Insert(key, value, id) =>

            kv += key -> value

            val origin = sender()
            val persistPerformer = context.actorOf(PersistPerformer.props(origin, persiter))

            implicit val askTimeout = Timeout(1.second)
            val eventualPersist = persistPerformer ? Persist(key, Some(value), id)

            eventualPersist.onFailure {
                case s => println(s"Persist failure $s")
            }

            val eventualReplications = replicators.toList.map { replicator =>
                val f = replicator ? Replicate(key, Some(value), id)

                f.onFailure {
                    case s => println(s"Replications failure: (key $key) $s")
                }

                f
            }

            val f = Future.sequence(eventualReplications :+ eventualPersist)

            f.onComplete {
                case Success(s) =>
                    origin ! OperationAck(id)
                case Failure(t) =>
                    origin ! OperationFailed(id)
            }


        case Remove(key, id) =>
            kv -= key

            val origin = sender()
            val persistPerformer = context.actorOf(PersistPerformer.props(origin, persiter))

            implicit val askTimeout = Timeout(1.second)
            val eventualPersist = (persistPerformer ? Persist(key, None, id)).mapTo[PersistPerformed]

            val eventualReplications = replicators.map { replicator =>
                replicator ? Replicate(key, None, id)
            }.toList

            val f = Future.sequence(eventualReplications :+ eventualPersist)

            f.onComplete {
                case Success(s) =>
                    origin ! OperationAck(id)
                case Failure(t) =>
                    origin ! OperationFailed(id)
            }

        case Replicas(replicas) =>

            val newSecondaries  = replicas.filterNot(_ == self)

            val removed = secondaries.keySet -- replicas.filterNot(_ == self)

            removed.foreach { rmvd =>
                val removedReplicator = secondaries(rmvd)

                removedReplicator ! Stop

                secondaries -= rmvd
                replicators -= removedReplicator
            }

            newSecondaries.foreach { replica =>

                val replicator =  context.actorOf(Replicator.props(replica))
                replicators += replicator
                secondaries += replica -> replicator

                kv.foreach {
                    case (key, value) => replicator ! Replicate(key, Some(value), -1000)
                }
            }

    }

    /* TODO Behavior for the replica role. */
    def replica(expectedSeq: Long): Receive = {

        case Get(key, id) => sender ! GetResult(key, kv.get(key), id)
        case s @ Snapshot(key, Some(value), seq) if seq == expectedSeq =>
            context.become(replica(seq + 1))
            kv += key -> value

            val persistPerformer = context.actorOf(PersistPerformer.props(sender(), persiter))

            persistPerformer ! Persist(key, s.valueOption, seq)

            
        case s @ Snapshot(key, None, seq) if seq == expectedSeq =>

            context.become(replica(seq + 1))
            kv -= key

            val persistPerformer = context.actorOf(PersistPerformer.props(sender(), persiter))

            persistPerformer ! Persist(key, s.valueOption, seq)

        case PersistPerformer.PersistPerformed(snapshotRequester, key, seq) =>
            snapshotRequester ! SnapshotAck(key, seq)
            
            
        case Snapshot(key, _, seq) if seq < expectedSeq =>
            sender() ! SnapshotAck(key, seq)

    }
}

private object PersistPerformer {

    def props(requester: ActorRef, persister: ActorRef): Props = {
        Props(new PersistPerformer(requester, persister))
    }

    case class PersistPerformed(requester: ActorRef, key: String, id: Long)
}

private class PersistPerformer(requester: ActorRef, persister: ActorRef) extends Actor {

    import context.dispatcher

    def receive: Receive = {
        case p: Persistence.Persist =>
            persister ! p

            context.become(waitingForConfirmation(sender(), context.system.scheduler.schedule(100.millis, 100.millis, persister, p)))
    }

    def waitingForConfirmation(origin: ActorRef, scheduling: Cancellable): Receive = {

        case Persistence.Persisted(key, id) =>
            origin ! PersistPerformed(requester, key, id)
            scheduling.cancel()
            context.stop(self)

    }
}

