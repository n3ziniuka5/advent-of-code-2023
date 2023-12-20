package aoc

import aoc.Common.timed
import aoc.Day20.ModuleState.{Broadcaster, Conjuction, FlipFlop}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day20:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day20.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    enum Pulse:
        case High, Low

    sealed trait ModuleState
    object ModuleState:
        case object Broadcaster                           extends ModuleState
        case class FlipFlop(on: Boolean)                  extends ModuleState
        case class Conjuction(inputs: Map[String, Pulse]) extends ModuleState

    def part1(lines: List[String]): Long =
        val (connections, state) = parse(lines)
        solve(1000, state, connections, 0, 0)

    def part2(lines: List[String]): Long =
        0

    def parse(lines: List[String]): (Map[String, List[String]], Map[String, ModuleState]) =
        val connections = lines.map { case s"$from -> $to" =>
            from.dropWhile(!_.isLetter).trim -> to.split(", ").toList
        }.toMap

        val state = lines.flatMap { l =>
            if (l.startsWith("&"))
                val key    = l.dropWhile(!_.isLetter).takeWhile(_ != ' ')
                val inputs = connections.filter(_._2.contains(key)).view.mapValues(_ => Pulse.Low).toMap

                Some(key -> Conjuction(inputs))
            else if (l.startsWith("%")) Some(l.dropWhile(!_.isLetter).takeWhile(_ != ' ') -> FlipFlop(false))
            else if (l.startsWith("broadcaster")) Some(l.dropWhile(!_.isLetter).takeWhile(_ != ' ') -> Broadcaster)
            else None
        }.toMap

        /*val withConjunctionInputs = state.map { case (name, state) =>
            state match
                case Conjuction(inputs) =>
                    val newInputs = inputs.map((name, _))
                    name -> Conjuction(newInputs)
                case _ => name -> state
        }*/

        // println(connections)
        // println(state)
        // println()

        (connections, state)

    @tailrec
    def solve(
        n: Long,
        state: Map[String, ModuleState],
        connections: Map[String, List[String]],
        lowPulses: Long,
        highPulses: Long
    ): Long =
        if (n == 0)
            println(s"low pulses $lowPulses and high pulses $highPulses")
            lowPulses * highPulses
        else
            val (newState, addedLow, addedHigh) =
                // println("PRESSING BUTTON")
                pressButton(Queue(("button", "broadcaster", Pulse.Low)), state, connections, 0, 0)
            solve(n - 1, newState, connections, lowPulses + addedLow, highPulses + addedHigh)

    @tailrec
    def pressButton(
        queue: Queue[(String, String, Pulse)],
        state: Map[String, ModuleState],
        connections: Map[String, List[String]],
        lowPulses: Long,
        highPulses: Long
    ): (Map[String, ModuleState], Long, Long) =
        def addPulse(pulse: Pulse): (Long, Long) =
            pulse match
                case Pulse.Low  => (lowPulses + 1, highPulses)
                case Pulse.High => (lowPulses, highPulses + 1)

        queue.dequeueOption match
            case None                            => (state, lowPulses, highPulses)
            case Some(((from, to, pulse), rest)) =>
                // println(s"from \"$from\" $pulse \"$to\" ")

                state.get(to) match
                    case Some(ModuleState.Broadcaster) =>
                        val newQueue          = rest.enqueueAll(connections(to).map(nextTo => (to, nextTo, pulse)))
                        val (newLow, newHigh) = addPulse(pulse)
                        pressButton(newQueue, state, connections, newLow, newHigh)

                    case Some(FlipFlop(on)) =>
                        pulse match
                            case Pulse.High => pressButton(rest, state, connections, lowPulses, highPulses + 1)
                            case Pulse.Low =>
                                val newPulse = if on then Pulse.Low else Pulse.High
                                val newQueue = rest.enqueueAll(connections(to).map(nextTo => (to, nextTo, newPulse)))
                                pressButton(
                                  newQueue,
                                  state + (to -> FlipFlop(!on)),
                                  connections,
                                  lowPulses + 1,
                                  highPulses
                                )

                    case Some(Conjuction(inputs)) =>
                        val (newLow, newHigh) = addPulse(pulse)
                        val newInputs         = inputs + (from -> pulse)
                        val newQueue =
                            if newInputs.forall(_._2 == Pulse.High) then
                                rest.enqueueAll(connections(to).map(nextTo => (to, nextTo, Pulse.Low)))
                            else rest.enqueueAll(connections(to).map(nextTo => (to, nextTo, Pulse.High)))

                        pressButton(newQueue, state + (to -> Conjuction(newInputs)), connections, newLow, newHigh)

                    case None =>
                        val (newLow, newHigh) = addPulse(pulse)
                        pressButton(rest, state, connections, newLow, newHigh)
