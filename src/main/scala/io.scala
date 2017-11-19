import better.files._
import java.io.{File => JFile}

object Data{
  def readSplittedData(path: String): Set[Int] = {
    val f = File(path)
    f.contentAsString
      // 末尾の改行文字を削除
      .reverse.tail.reverse
      .split(" ")
      .map{i => i.toInt}
      .toSet
  }
}
