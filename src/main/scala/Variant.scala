import Variant.Chrom

import scala.util.Try

case class Variant(chr: Chrom, pos: Long, ref: Option[String], alt: Option[String], quality: Double)

object Variant {
  type Chrom = String
  private val supportedChromosomes = ((1 to 22) ++ Seq("X, Y, M")).map(c => s"chr$c").toSet

  private def parseRefAlt(refAlt: String): Option[String] = {
    Some(refAlt)
      .map(_.filterNot(_ == 'N'))
      .filterNot(_ == ".")
      .filterNot(_.isEmpty)
  }

  def parse(line: String): scala.Seq[Variant] = Try {
    val Array(chr, pos, _, ref, alt, qual, filter, _*) = line.split("\t")
    filter match {
      case "PASS" if supportedChromosomes.contains(chr) => {
        if (alt.contains(",")) {
          alt.split(",").map(singleAlt => {
            Variant(
              chr,
              pos.toLong,
              parseRefAlt(ref),
              parseRefAlt(singleAlt),
              qual.toDouble)
          }).toSeq
        } else {
          Seq(Variant(
            chr,
            pos.toLong,
            parseRefAlt(ref),
            parseRefAlt(alt),
            qual.toDouble))
        }
      }
      case _ => Seq.empty
    }
  }.toOption.getOrElse(Seq.empty)
}
