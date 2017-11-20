import scala.io.Source

object IO {
  /*
   data file format
   first line: attr_name_1, ..., attr_name_n, positive_class_name
   last lines: attr_value, ..., attr_value, class
   */
  def importData(filepath: String): List[RelationalData] = {
    val source = Source.fromFile(filepath)
    val lines = source.getLines.toList

    val firstline = lines.head
    val secondline = lines.tail.head
    val lastlines = lines.tail.tail

    val attr_names = firstline.split(",").toList
    val class_col_num = secondline.toInt - 1

    val attr_values_list = lastlines.map{line => line.split(",").toList}
    val attr_num = attr_values_list(0).length - 1


    attr_values_list.map{attr_values =>
      var attr_name_to_value_map: Map[String,Const] = Map.empty
        (0 to attr_num-1).map{i =>
          if(i != class_col_num)
            attr_name_to_value_map += (attr_names(i) -> Const(attr_values(i)))
        }
      RelationalData(attr_values(class_col_num), attr_name_to_value_map)
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
