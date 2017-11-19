import scala.io.Source

object IO {
  /*
   data file format
   first line: attr_name_1, ..., attr_name_n, positive_class_name
   last lines: attr_value, ..., attr_value, class
   
   TODO?: select class column by first line
   */
  def importData(filepath: String) = {
    val source = Source.fromFile(filepath)
    val lines = source.getLines.toList
    val firstline = lines.head
    val lastlines = lines.tail

    val attr_names = firstline.split(",").toList
    val attr_values_list = lastlines.map{line => line.split(",").toList}

    val attr_num = attr_names.length

    attr_values_list.map{attr_values =>
      var attr_name_to_value_map: Map[String,String] = Map.empty
      (0 to attr_num-2).map{i =>
        attr_name_to_value_map += (attr_names(i) -> attr_values(i))
      }
    RelationalData(attr_values.last, attr_name_to_value_map)
    }
  }
}
