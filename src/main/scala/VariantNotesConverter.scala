import Variant.Chrom
import cats.data.Chain
import de.sciss.midi._

import scala.collection.{Seq, mutable}
import scala.util.Random

object VariantNotesConverter {
  private implicit val rate: TickRate = TickRate.tempo(bpm = 120, tpq = 1024)
  private val maxVelocity = 127
  private val pianoPitchRange = 21 to 108
  private val startNote = Random.nextInt(12) + pianoPitchRange.start
  private val minorScaleIntervals = Seq(2, 1, 2, 2, 1, 3, 1)
  private val majorScaleIntervals = Seq(2, 2, 1, 2, 2, 2, 1)
  private val scaleIntervals = minorScaleIntervals
  private val scalePitches: Seq[Int] = {
    val builder: mutable.Buffer[Int] = mutable.Buffer[Int]()
    var currentNote = startNote
    var idx = 0
    while (currentNote <= pianoPitchRange.end) {
      builder.append(currentNote)
      currentNote += scaleIntervals(idx)
      idx = (idx + 1) % scaleIntervals.size
    }
    builder
  }

  private val scalePitchesToIdx: Map[Int, Int] = scalePitches.zipWithIndex.toMap

  private val baseGroupsCache: mutable.Map[Int, Seq[String]] = mutable.Map[Int, Seq[String]]()

  private lazy val baseGroups = baseGroupsOfLength(3) ++ baseGroupsOfLength(2) ++ baseGroupsOfLength(1)
  private lazy val baseGroupToPitch = baseGroups.reverse.zipWithIndex.map {
    case (group, idx) if idx % 2 == 0 =>
      group -> (scalePitches(idx / 4))
    case (group, idx) =>
      group -> (scalePitches(scalePitches.length - idx / 4 - 1))
  }.toMap


  def baseGroupsOfLength(n: Int): Seq[String] = n match {
    case 1 => Seq("C", "G", "A", "T")
    case _ =>
      for {
        prev <- baseGroupsCache.getOrElseUpdate(n-1, baseGroupsOfLength(n-1))
        addition <- baseGroupsCache.getOrElseUpdate(1, baseGroupsOfLength(1))
      } yield {
        s"$prev$addition"
      }
  }

  def refAltToPitch(refAlt: String): Int = {
   refAlt.grouped(3)
      .map(baseGroupToPitch)
      .toSeq
      .groupBy(identity)
      .maxBy(_._2.length)
      ._1
  }

  private def qualityToVelocity(qual: Double): Int = {
    (maxVelocity * math.min(1D, qual / 10000)).toInt
  }

  private def positionToTime(pos: Long): Int = {
    (pos / 500000).toInt
  }

  def variantToNotes(variant: Variant): (Option[Note], Option[Note]) = {
    val refNote = variant.ref.map(ref => Note(
      channel = 0,
      pitch = refAltToPitch(ref),
      velocity = qualityToVelocity(variant.quality),
      time = positionToTime(variant.pos),
      length = 1,
      title = variant.chr))

    val altNote = variant.alt.map(alt => Note(
      channel = 1,
      pitch = refAltToPitch(alt),
      velocity = qualityToVelocity(variant.quality),
      time = positionToTime(variant.pos),
      length = 1,
      title = variant.chr))

    (refNote, altNote)
  }

  def addNoteToChannel(note: Note, chain: Chain[Note]): Chain[Note] = {
    chain.uncons match {
      case Some((head, tail)) if head.title == note.title && head.time == note.time && head.channel == note.channel =>
        tail.prepend(combine(head, note))
      case _ => chain.prepend(note)
    }
  }

  def noteToEvents(note: Note): Seq[Event] = {
    val noteLength = math.min(note.length, 16)
    Seq(
      Event(
        tick = (note.time * 0.25 * rate.value).toLong,
        message = NoteOn(
          channel = note.channel,
          pitch = note.pitch,
          velocity = note.velocity
        )
      ),
      Event(
        tick = ((note.time + noteLength) * 0.25 * rate.value).toLong,
        message = NoteOff(
          channel = note.channel,
          pitch = note.pitch,
          velocity = note.velocity
        )
      )
    )
  }

  def convertToScore(variants: Iterator[Variant]): Map[Chrom, Sequence] = {

    val (channel1, channel2): (Chain[Note], Chain[Note]) = variants.foldLeft((Chain.empty[Note], Chain.empty[Note])) {
      case ((leftChannel, rightChannel), variant) =>
        val (leftChannelNote, rightChannelNote) = variantToNotes(variant)

        (leftChannelNote.map(addNoteToChannel(_, leftChannel)).getOrElse(leftChannel),
          rightChannelNote.map(addNoteToChannel(_, rightChannel)).getOrElse(rightChannel))
    }

    (channel1 ++ channel2)
      .toList
      .groupBy(_.title)
      .mapValues(_.sortBy(_.time).flatMap(noteToEvents))
      .mapValues(events => Track(events.toVector))
      .mapValues(track => Sequence(Vector(track)))
  }


  def combinePitches(pitch1: Int, pitch2: Int): Int = {
    val pitch1Idx = scalePitchesToIdx(pitch1)
    val pitch2Idx = scalePitchesToIdx(pitch2)
    scalePitches((pitch1Idx + pitch2Idx) / 2)
  }

  def combine(note1: Note, note2: Note): Note = {
    Note(
      channel = note1.channel,
      pitch = combinePitches(note1.pitch, note2.pitch),
      velocity = math.min(note1.velocity + note2.velocity, maxVelocity),
      time = note1.time,
      length = note1.length + note2.length,
      title = note1.title
    )
  }
}

case class Note(
                 channel: Int,
                 pitch: Int,
                 velocity: Int,
                 time: Int,
                 length: Int,
                 title: String
               )

object Note {

}
