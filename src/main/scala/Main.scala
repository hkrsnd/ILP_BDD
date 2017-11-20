import net.sf.javabdd.BDD
import net.sf.javabdd.BDDFactory

object Main {
  def buildBDD(datafile_path: String, valuefile_path: String, positive_symbol: String, body_length: Int) = {
    val datas = IO.importData(datafile_path)
    print(datas.length)
    println(" samples")

    val possible_values = IO.importPossibleValues(valuefile_path)

    val clauses = PredicateLogic.generateCountedDefiniteClauses(3,possible_values,"positive").toList.sortWith{(x,y) => x.body.length < y.body.length }
    print(clauses.length)
    println(" possible clauses")

    println("Building BDD...")
    BDDMain.buildBDDFromRelationalData(datas,possible_values,clauses,positive_symbol,body_length)
  }
}

object Test {
  val mammal_data_path = "resource/data/mammal.data"
  val mammal_value_path = "resource/data/mammal.values"

  val mushroom_data_path = "resource/data/agaricus-lepiota.data"
  val mushroom_value_path = "resource/data/agaricus-lepiota.value"

  def mammal_test() = {
    val datas = IO.importData(mammal_data_path)
    val possible_values = IO.importPossibleValues(mammal_value_path)
    Main.buildBDD(mammal_data_path,mammal_value_path,"positive",2)
  }

  def mushroom_test() = {
    val datas = IO.importData(mushroom_data_path)
    val possible_values = IO.importPossibleValues(mushroom_value_path)
    Main.buildBDD(mushroom_data_path,mushroom_value_path,"positive",2)
  }
}
