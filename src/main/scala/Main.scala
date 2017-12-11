import net.sf.javabdd.BDD
import net.sf.javabdd.BDDFactory

object Main {
  def buildBDD(datafile_path: String, valuefile_path: String, positive_symbol: String, body_length: Int) = {
    val datas = IO.importData(datafile_path)
    print(datas.length)
    println(" samples")

    val possible_values = IO.importPossibleValues(valuefile_path)

    val all_clauses = PredicateLogic.generateCountedDefiniteClauses(body_length,possible_values,positive_symbol).toList//sortWith{(x,y) => x.body.length < y.body.length }
    val clauses = PredicateLogic.removeDontCareClauses(all_clauses,datas).toList
      .sortWith{(x,y) => x.body.length < y.body.length }
      (0 to clauses.length-1).map{i =>
        print(i)
        print(": ")
        println(clauses(i))}
 
    print(clauses.length)
    println(" possible clauses")

    println("Building BDD...")
    BDDMain.buildBDDFromRelationalData(datas,possible_values,clauses,positive_symbol,body_length)
  }

  def generateClauses(datafile_path: String,valuefile_path: String, positive_symbol: String, body_length: Int) = {
    val datas = IO.importData(datafile_path)

    val possible_values = IO.importPossibleValues(valuefile_path)

    val all_clauses = PredicateLogic.generateCountedDefiniteClauses(body_length,possible_values,positive_symbol).toList//sortWith{(x,y) => x.body.length < y.body.length }
    val clauses = PredicateLogic.removeDontCareClauses(all_clauses,datas).toList
      .sortWith{(x,y) => x.body.length < y.body.length }

    clauses
  }
}

object Test extends BDDAlgo{
  val mammal_data_path = "resource/data/mammal.data"
  val mammal_value_path = "resource/data/mammal.value"

  val mammal_mini_data_path = "resource/data/mammal_mini.data"
  val mammal_mini_value_path = "resource/data/mammal_mini.value"

  val tic_data_path = "resource/data/tic-tac-toe.data"
  val tic_value_path = "resource/data/tic-tac-toe.value"

  val mushroom_data_path = "resource/data/agaricus-lepiota.data"
  val mushroom_mini_data_path = "resource/data/agaricus-lepiota-mini.data"
  val mushroom_value_path = "resource/data/agaricus-lepiota.value"

  val soybean_small_data_path = "resource/data/soybean-small.data"
  val soybean_small_value_path = "resource/data/soybean-small.value"

  val soybean_large_data_path = "resource/data/soybean-large.data"
  val soybean_large_value_path = "resource/data/soybean-small.value"

  def mammal_test(body_length: Int) = {
    val start = System.currentTimeMillis
    val result = Main.buildBDD(mammal_data_path,mammal_value_path,"positive",body_length)
    println((System.currentTimeMillis - start) + "msec")
    print(result.nodeCount)
    println(" nodes")
    print(result.satCount)
    println(" assignments")
    val clauses = Main.generateClauses(mammal_data_path,mammal_value_path,"positive",body_length)
    val weights = clauses.map{c => c.body.size}.toArray
    val bools = minimumWeight(result,weights)

    (0 to bools.size-1).foreach{i =>
      if(bools(i) == 1)
        println(clauses(i))
    }
  }

  def mammal_mini_test() = {
    val start = System.currentTimeMillis
    val result = Main.buildBDD(mammal_mini_data_path,mammal_mini_value_path,"positive",2)
    println((System.currentTimeMillis - start) + "msec")
    print(result.nodeCount)
    println(" nodes")
    print(result.satCount)
    println(" assignments")
    val weights = Array(0,1,1,1,1,2,2,2,2)
    minimumWeight(result,weights)
  }

  def tic_tac_toe_test(body_length: Int) = {
    val start = System.currentTimeMillis
    val result = Main.buildBDD(tic_data_path,tic_value_path,"positive",body_length)
    println((System.currentTimeMillis - start) + "msec")
    print(result.nodeCount)
    println(" nodes")
    result
  }

  def mushroom_test(body_length: Int) = {
    val start = System.currentTimeMillis
    val result = Main.buildBDD(mushroom_data_path,mushroom_value_path,"e",body_length)
    println((System.currentTimeMillis - start) + "msec")
    result
  }

  def mushroom_mini_test(body_length: Int) = {
    val start = System.currentTimeMillis
    val result = Main.buildBDD(mushroom_mini_data_path,mushroom_value_path,"e",body_length)
    println((System.currentTimeMillis - start) + "msec")
    result
  }

  def soy_small_test(body_length: Int) = {
    val start = System.currentTimeMillis
    val result = Main.buildBDD(soybean_small_data_path,soybean_small_value_path,"D1",body_length)
    println((System.currentTimeMillis - start) + "msec")
    print(result.nodeCount)
    println(" nodes")
    result

    val clauses = Main.generateClauses(soybean_small_data_path,soybean_small_value_path,"D1",body_length)
    val weights = clauses.map{c => c.body.size}.toArray
    val bools = minimumWeight(result,weights)

    (0 to bools.size-1).foreach{i =>
      if(bools(i) == 1)
        println(clauses(i))
    }
  }

  def soy_learge_test(body_length: Int) = {
    val start = System.currentTimeMillis
    val result = Main.buildBDD(soybean_large_data_path,soybean_large_value_path,"D1",body_length)
    println((System.currentTimeMillis - start) + "msec")
    print(result.nodeCount)
    println(" nodes")
    result
  }

  def testAllSat = {
   val result = Main.buildBDD(mammal_mini_data_path,mammal_mini_value_path,"positive",2)
    result
  }
}
