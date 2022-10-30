import scala.io.{BufferedSource, Source}

object GenomePlayer {

  def parseVcf(vcfFile: BufferedSource): Iterator[Variant] = {
    val vcfLines = vcfFile.getLines()
    vcfLines
      .filterNot(_.startsWith("#"))
      .flatMap(Variant.parse)
  }

  def main(args: Array[String]): Unit = {
    val vcfFile = Source.fromFile("gfx0237502_401-freebayes.final.vcf")
    try {
      val variants = parseVcf(vcfFile)
//      println(variants.toList.flatMap(_.ref).map(_.length).max)
//      println(variants.toList.flatMap(_.alt).map(_.length).max)
      val scoreByChrom = VariantNotesConverter.convertToScore(variants)
      scoreByChrom.foreach {
        case (chrom, seq) => seq.write(s"$chrom.midi")
      }
      println("done")
    } finally {
      vcfFile.close()
    }
  }
}
