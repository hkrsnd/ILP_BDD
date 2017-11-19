import scala.io.Source

object IO {
  /*
   data file format
   first line: attr_name_1, ..., attr_name_n, positive_class_name
   last lines: attr_value, ..., attr_value, class
   
   TODO?: select class column by first line
   */
  def importData(filepath: String): List[RelationalData] = {
    val source = Source.fromFile(filepath)
    val lines = source.getLines.toList
    val firstline = lines.head
    val lastlines = lines.tail

    val attr_names = firstline.split(",").toList
    val attr_values_list = lastlines.map{line => line.split(",").toList}

    val attr_num = attr_names.length

    attr_values_list.map{attr_values =>
      var attr_name_to_value_map: Map[String,Const] = Map.empty
      (0 to attr_num-1).map{i =>
        attr_name_to_value_map += (attr_names(i) -> Const(attr_values(i)))
      }
    RelationalData(attr_values.last, attr_name_to_value_map)
    }
  }

  def importPossibleValues(filepath: String): Map[String, Set[Const]] = {
    val source = Source.fromFile(filepath)
    val lines = source.getLines.toList

    var result = Map[String,Set[Const]]()
    lines.foreach{line =>
      val attr_name = line.split(";").head
      val values = line.split(";").last.split(",").map{Const(_)}.toSet
      result += (attr_name -> values)
    }
    result
  }
}
