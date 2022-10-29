import Note.maxVelocity
import Variant.Chrom
import cats.data.Chain
import de.sciss.midi.{Event, NoteOff, NoteOn, Sequence, TickRate, Track}

object VariantNotesConverter {
  private implicit val rate: TickRate = TickRate.tempo(bpm = 120, tpq = 1024)
  private val pianoPitchRange = 21 to 108

  def refAltToPitch(refAlt: String): Int = {
    refAlt.zipWithIndex.foldLeft((pianoPitchRange.min + pianoPitchRange.max) / 2) {
      case (pitch, (base, idx)) => {
        val delta = base match {
          case 'C' => 1
          case 'G' => -1
          case 'A' => 2
          case 'T' => -2
        }
        ((pitch + (delta * idx)) % (pianoPitchRange.max - pianoPitchRange.min)) + pianoPitchRange.min
      }
    }
  }

  private def qualityToVelocity(qual: Double): Int = {
    (maxVelocity * math.min(1D, qual / 10000)).toInt
  }

  private def positionToTime(pos: Long): Int = {
    (pos / 1000000).toInt
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
        tail.prepend(Note.combine(head, note))
      case _ => chain.prepend(note)
    }
  }

  def noteToEvents(note: Note): Seq[Event] = {
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
        tick = ((note.time + note.length) * 0.25 * rate.value).toLong,
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
      .mapValues(_.flatMap(noteToEvents))
      .mapValues(events => Track(events.toVector))
      .mapValues(track => Sequence(Vector(track)))
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
  val maxVelocity = 127

  def combinePitches(pitch1: Int, pitch2: Int): Int = {
    (pitch1 + pitch2) / 2
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
