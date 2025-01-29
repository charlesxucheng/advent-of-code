package aoc
package aoc2024.day19

import aoc2024.day19.Towels.checkMatches
import common.Utils.loadData
import scala.collection.mutable.Map

object Towels {

//  val patterns: Seq[String] =
//    "r, wr, b, g, bwu, rb, gb, br".split(",").map(_.trim).toSeq

  val patterns: Seq[String] =
    "uurr, uugbw, rg, wugbbb, uru, ububw, uu, uwr, rgrgb, rurru, bbub, rww, urggbbb, rbur, grur, grw, guru, rgu, bwbw, ru, grrbbur, urr, bwbbbg, brrbr, wgw, rurbrbrr, wuu, wggw, wuuuru, wrg, ugww, gggrrg, gwruru, rrw, rbb, wgwrw, wug, bwurwbu, uurw, gbb, gbrbg, gwubwbrr, rbggr, bwgwg, uwg, guwbbw, rguwb, bgbu, grr, ubw, ggwbu, bburgr, u, urgb, rrb, wwbrru, ruruw, gr, uruwwur, brw, gwgu, rgbg, gug, rrbb, uuubub, rwuu, bru, uruuwbr, gubuuw, ruwu, rurbb, bu, bwbrwggr, wbw, ubwuw, buwugb, bur, uwwu, urb, rbrb, gu, wguwr, urrgw, ur, uwgbg, bbbugr, wuww, uggr, bbgwubb, uwgr, bubgb, bubu, bbr, rwrrb, gbuu, bgwbg, rrru, wgwrr, bug, bubrwb, burubu, bbwr, gwgg, uggubg, rgrub, ggbrrgu, wrbgru, gwr, uwgu, wuwub, gg, rrrbwrr, guw, gruu, rwrw, rbbgrb, rbugbr, buugbu, ggrrbuww, wgru, uur, wuwwr, bb, bgbg, gwurrwu, rbu, gwurg, rrrugwub, rgr, rru, wrgbgr, grg, rbrwrg, wrwgwwb, bugw, rwr, ugug, rwgg, ruw, ubg, brbw, bbrgrb, uww, uwwur, uubbr, wbrw, ggu, gbw, w, bggu, wwbrb, rbg, gbgubgu, bbbu, bgbur, uurur, uwuwu, uuwbw, wwgruub, wgwwwwr, buwr, gbuwg, wur, gbgrwb, buu, gbr, ggw, bwbbgbww, bruug, rruruu, bwgw, gww, ruwugggr, burb, brug, rbwurug, uuggwru, bbu, gbrgbuu, ruggwbw, wgur, gggru, rur, wuwgr, wwbwubr, guurwu, rwrrbub, wr, rugbgu, uugwwb, brr, gbwub, bub, rwrubw, uguwbggb, bruubbbw, wbrgu, ubb, buugr, bbwuww, wwugrw, wwb, bwgrwuw, wgg, bgwu, rguubw, uwrguw, uuwgu, ruwwb, ggg, urwub, rwu, bubuu, wubb, uwug, bg, gggbubg, rwrrwgu, wbr, brbu, bbuu, uruwur, uwwwrwbr, bwg, uub, grbrgw, bbug, wrgggrwg, gggurb, wgb, bbbbrbww, ggwb, rgw, bubuwgu, uwu, ubrurbu, grb, wrrw, wugw, brgwrugr, wrw, wgu, grgr, uuurw, ugrgrwr, www, bbg, ugg, gbu, uwrg, wbu, brb, rgb, rbrgbwu, ggb, wbwgr, grguwurr, brbrbr, ugw, gburg, gbbw, rrrbr, wwgg, rrr, bgu, wugbwgw, ruwuw, wwu, guug, gggub, gwbgw, wbwru, bwgru, rrugrw, uwuwrw, wbb, rr, gbuw, uwww, brg, urw, bgwb, wggggg, uwbbb, rwrr, rgwr, ugr, bgbggw, ubub, wrbggub, rrubr, ggrwr, wwur, wwuug, bbw, bbur, ruuwwrbr, ugrw, bwb, rrbwgw, bgbggwg, ubbw, uubwrg, rrwu, gbwb, bubg, brbggb, guugrb, wuwwru, rwrg, rgbrgr, uuw, ubrgwr, wwbrw, grbgg, urgg, bbrwu, rw, ubwwr, uugwg, ruubuw, wrrwrwuw, rbub, urg, buw, buuu, rbbwrr, rggwrw, urgugr, bwr, guu, wwuwug, rurb, bgb, rgbb, bgr, wuub, ubuwurw, buuwr, ggwuwgwb, gbug, ug, ruuru, rrg, rwuubww, wbg, buwb, ugrugb, gwb, wub, ggrbr, guwr, wwrwr, wgrw, bgw, ww, wuubgru, uw, bbwuurg, wuuwub, wrbuu, gw, bgrrgbb, wrbbgwwb, b, ruww, rbruu, bgg, uguww, wb, wgr, rubub, rbru, rrgb, ugwrww, gbg, wrgr, rbw, ugbuwww, gru, ruu, wugr, wgwg, wu, gwgbbug, wrbwrrgb, ugb, rwg, bgwburb, ggr, brggg, ruuuu, ugu, grbwu, ub, uwb, ubu, burwwrgr, wwg, gbwgu, wru, ggbwu, gwgubb, bbbww, rub, ubbbg, gwwb, rwugu, urggwbr, gwub, ubr, wrgu, gub, rb, wrb, rug, wuw, rubgrrg, bbbw, gbgrg, wbrggw, urbb, rbr, buuurg, uggu, bgrr, wrr, bubrg, urwu, wbbr, bww, gbbuug, rgg, bgbw, gb, uuwu, uuu, br, wgbg, rrbuurb, bwu, r, gur, rwug, uuwru, bbb, gugugw"
      .split(",")
      .map(_.trim)

  private val countByDesign = Map.empty[String, Long]

  def checkMatches(
      input: Seq[String]
  ): Seq[Long] = {

    // Use a mutable Map to carry saved count by Designs across recursive calls
    val countByDesign = Map.empty[String, Long]

    input.map(countMatches)
  }

  def countMatches(
      design: String
  ): Long = {
    countByDesign.getOrElseUpdate(
      design, {
        val remainingDesigns = patterns
          .filter(design.startsWith)
          .map(pattern => design.drop(pattern.length))

        remainingDesigns.map(d => if (d.isEmpty) 1 else countMatches(d)).sum
      }
    )
  }
}

@main
def main(): Unit = {

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(scribe.Level.Info))
    .replace()

//  val filename = "test.txt"
  val filename = "aoc2024-day19-input.txt"
  val input = loadData(filename)(s => s.flatMap(Seq(_)).toSeq)

  scribe.debug(input.toString)

  val result = checkMatches(input)

  scribe.info(
    s"The number of inputs that can be matched is ${result.map(_ > 0).count(identity)}"
  )
  scribe.info(s"The number of ways to arrange the towels are ${result.sum}")

}
