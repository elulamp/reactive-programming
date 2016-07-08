package kvstore

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import kvstore.Replicator.SnapshotAck

import scala.concurrent.duration._

object Replicator {

    case class Replicate(key: String, valueOption: Option[String], id: Long)

    case class Replicated(key: String, id: Long)

    case class Snapshot(key: String, valueOption: Option[String], seq: Long)

    case class SnapshotAck(key: String, seq: Long)

    case class Stop()

    def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {

    import Replicator._

    /*
     * The contents of this actor is just a suggestion, you can implement it in any way you like.
     */

    // map from sequence number to pair of sender and request
    var acks = Map.empty[Long, (ActorRef, Replicate)]
    // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
    var pending = Vector.empty[Snapshot]

    var _seqCounter = 0L

    def nextSeq = {

        val ret = _seqCounter
        _seqCounter += 1
        ret
    }


    /* TODO Behavior for the Replicator. */
    def receive: Receive = {

        case r@Replicate(key, valueOption, id) =>

            val seq = nextSeq

            acks += seq ->(sender(), r)

            val snapshotPerformer = context.actorOf(SnapshotPerformer.props(replica))

            snapshotPerformer ! SnapshotPerformer.PerformSnapshot(key, valueOption, seq)

        case SnapshotPerformer.SnapshotPerformed(key, seq) =>

            val (requester, Replicate(_, _, id)) = acks(seq)

            acks -= seq

            requester ! Replicated(key, id)

        case Stop =>
            acks.foreach {
                case (seq, (requester, Replicate(key, _, id))) =>
                    requester ! Replicated(key, id)
            }
            context.stop(self)
    }


}

private object SnapshotPerformer {

    case class PerformSnapshot(key: String, valueOption: Option[String], seq: Long)

    case class SnapshotPerformed(key: String, seq: Long)

    def props(replica: ActorRef): Props = {

        Props(new SnapshotPerformer(replica))
    }
}

private class SnapshotPerformer(replica: ActorRef) extends Actor {

    import SnapshotPerformer._
    import context.dispatcher

    override def receive: Actor.Receive = {

        case PerformSnapshot(key, valueOption, seq) => {

            val s = Replicator.Snapshot(key, valueOption, seq)

            replica ! s

            context.become(waitingForConfirmation(sender(), context.system.scheduler.schedule(100.millis, 100.millis, replica, s)))
        }
    }

    def waitingForConfirmation(origin: ActorRef, scheduling: Cancellable): Receive = {

        case SnapshotAck(key, seq) =>
            origin ! SnapshotPerformed(key, seq)
            scheduling.cancel()
            context.stop(self)
    }
}