import net.sf.javabdd.BDD
import net.sf.javabdd.BDDFactory

object Main {
  def buildBDD(b: BDDFactory, datafile_path: String, valuefile_path: String, positive_symbol: String, body_length: Int) = {
    val datas = IO.importData(datafile_path)
    println(datas)
    print(datas.length)
    println(" samples")

    val possible_values = IO.importPossibleValues(valuefile_path)
    println(possible_values)
    val all_clauses = PredicateLogic.generateCountedDefiniteClauses(body_length,possible_values,positive_symbol).toList//sortWith{(x,y) => x.body.length < y.body.length }
    val clauses = PredicateLogic.removeDontCareClauses(all_clauses,datas).toList
      .sortWith{(x,y) => x.body.length < y.body.length }
/*      (0 to clauses.length-1).map{i =>
        print(i)
        print(": ")
        println(clauses(i))}
 */
    print(clauses.length)
    println(" possible clauses")

    println("Building BDD...")
    BDDMain.buildBDDFromRelationalData(b, datas,possible_values,clauses,positive_symbol,body_length)
  }

  def generateClauses(datafile_path: String,valuefile_path: String, positive_symbol: String, body_length: Int) = {
    val datas = IO.importData(datafile_path)

    val possible_values = IO.importPossibleValues(valuefile_path)

    val all_clauses = PredicateLogic.generateCountedDefiniteClauses(body_length,possible_values,positive_symbol).toList//sortWith{(x,y) => x.body.length < y.body.length }
    val clauses = all_clauses //PredicateLogic.removeDontCareClauses(all_clauses,datas).toList
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
    val b = BDDFactory.init(5500000,5500000)
    val start = System.currentTimeMillis
    val result = Main.buildBDD(b,mammal_data_path,mammal_value_path,"positive",body_length)
    println((System.currentTimeMillis - start) + "msec")
    print(result.nodeCount)
    println(" nodes")
    print(result.satCount)
    println(" assignments")
    val clauses = Main.generateClauses(mammal_data_path,mammal_value_path,"positive",body_length)
    val weights = clauses.map{c => c.body.size}.toArray
    minimumWeightTai(b,result,weights)
  }

  def mammal_mini_test(body_length: Int) = {
    val b = BDDFactory.init(5500000,5500000)
    val start = System.currentTimeMillis
    val result = Main.buildBDD(b,mammal_mini_data_path,mammal_mini_value_path,"positive",body_length)
    println((System.currentTimeMillis - start) + "msec")
    print(result.nodeCount)
    println(" nodes")
    print(result.satCount)
    println(" assignments")
    val clauses = Main.generateClauses(mammal_data_path,mammal_value_path,"positive",body_length)
    val weights = clauses.map{c => c.body.size}.toArray
    minimumWeightTai(b,result,weights)
  }

  def tic_tac_toe_test(body_length: Int) = {
    val b = BDDFactory.init(5500000,5500000)
    val start = System.currentTimeMillis
    val result = Main.buildBDD(b,tic_data_path,tic_value_path,"positive",body_length)
    println((System.currentTimeMillis - start) + "msec")
    print(result.nodeCount)
    println(" nodes")
    result
  }

  def mushroom_test(body_length: Int) = {
    val b = BDDFactory.init(5500000,5500000)
    val start = System.currentTimeMillis
    val result = Main.buildBDD(b,mushroom_data_path,mushroom_value_path,"e",body_length)
    println((System.currentTimeMillis - start) + "msec")
    result
  }

  def mushroom_mini_test(body_length: Int) = {
    val b = BDDFactory.init(5500000,5500000)
    val start = System.currentTimeMillis
    val result = Main.buildBDD(b,mushroom_mini_data_path,mushroom_value_path,"e",body_length)
    println((System.currentTimeMillis - start) + "msec")
    result
  }

  def soy_small_test(body_length: Int) = {
    val b = BDDFactory.init(32000000,32000000)
    val start = System.currentTimeMillis
    val result = Main.buildBDD(b,soybean_small_data_path,soybean_small_value_path,"D1",body_length)
    println("BDD Construction Time: " + (System.currentTimeMillis - start) + "msec")
    print(result.nodeCount)
    println(" nodes")
//    result

    val clauses = Main.generateClauses(soybean_small_data_path,soybean_small_value_path,"D1",body_length)
    println(clauses.length.toString + " possible clauses (variables)")
    val weights = clauses.map{c => c.body.size}.toArray
/*    val bools = minimumWeight(result,weights)

    (0 to bools.size-1).foreach{i =>
      if(bools(i) == 1)
        println(clauses(i))
 }*/
    val start_top = System.currentTimeMillis
    val top_result = minimumWeight(result,weights)
    println("Get Top Hypothesis Time: " + (System.currentTimeMillis - start_top) + "msec")
    val start_tie = System.currentTimeMillis
    val tie_result = minimumWeightTai(b,result,weights)
    println("Tie-BDD Construction Time: " + (System.currentTimeMillis - start_tie) + "msec")
    tie_result
  }

  def soy_learge_test(body_length: Int) = {
    val b = BDDFactory.init(5500000,5500000)
    val start = System.currentTimeMillis
    val result = Main.buildBDD(b,soybean_large_data_path,soybean_large_value_path,"D1",body_length)
    println((System.currentTimeMillis - start) + "msec")
    print(result.nodeCount)
    println(" nodes")
    result
  }

  def testAllSat = {
    val b = BDDFactory.init(5500000,5500000)
   val result = Main.buildBDD(b,mammal_mini_data_path,mammal_mini_value_path,"positive",2)
    result
  }

  def printBDD = {
    import net.sf.javabdd.BDD
    import net.sf.javabdd.BDDFactory
    val b = BDDFactory.init(10,10)
    b.setVarNum(4)
    val x0 = b.ithVar(0)
    val x1 = b.ithVar(1)
    val x2 = b.ithVar(2)
    val x3 = b.ithVar(3)

    x0.and(x1).or(x2).printDot
  }

  def testEven = {
    val b = BDDFactory.init(10,10)
    b.setVarNum(7)
    val f0 = b.ithVar(0)
    val f1 = b.ithVar(1).or(b.ithVar(2).and(f0))
    val f2 = b.ithVar(3).or(b.ithVar(4).and(f0).or(b.ithVar(5).and(f1)).or(b.ithVar(6).and(f0).and(f1)))
    val result = f0.and(f2).and(f1.not)
    println("f2==========================")
    f2.printDot
    println("result======================")
    result.printDot
  }



  def example1 = {
    val b = BDDFactory.init(10,10)
    b.setVarNum(3)
    (b.ithVar(0).and(b.ithVar(1))).or(b.ithVar(2)).printDot
  }
}


object NumTest extends BDDUtil with SetUtil{
  import NumPredicateLogic._
  import BDDMain._
  import net.sf.javabdd.BDD
  import net.sf.javabdd.BDDFactory

  def isEven(number: Int) = number % 2 == 0
  def isOdd(number: Int) = !isEven(number)

  def even_test(size: Int, cache_size: Int)= {
    val nums = Range(0,size+1).toList.map{x => intToLiteral(x)}
    val clauses = generateDefiniteClauses(size)
    println("clauses: ")
    (0 to clauses.length - 1).map{i =>
      print(i.toString + " : ")
      println(clauses(i))
    }
    println("variables: " + clauses.length.toString)
    val b = BDDFactory.init(cache_size, cache_size)
    b.setVarNum(clauses.length)
    val start = System.currentTimeMillis
    val result = numsToBDD(b, nums, isEven, clauses)
    println((System.currentTimeMillis - start) + "msec")
    print("nodes: ")
    println(result.nodeCount)
    print("assignments: ")
    println(result.satCount)
    result
  }
  def even_sample(size: Int, cache_size: Int)= {
    val nums = Range(0,size+1).toList.map{x => intToLiteral(x)}
    var clauses = generateDefiniteClauses(size).toArray
    var tmp = clauses(1)
    clauses(1) = clauses(0)
    clauses(0) = tmp
    tmp = clauses(3)
    clauses(3) = clauses(2)
    clauses(2) = tmp
    tmp = clauses(6)
    clauses(6) = clauses(5)
    clauses(5) = tmp
    println("clauses: ")
    val clauses_ls = clauses.toList
    (0 to clauses_ls.length - 1).map{i =>
      print(i.toString + " : ")
      println(clauses_ls(i))
    }
    println("variables: " + clauses.length.toString)
    val b = BDDFactory.init(cache_size, cache_size)
    b.setVarNum(clauses.length)
    val start = System.currentTimeMillis
    val result = numsToBDD(b, nums, isEven, clauses_ls)
    println((System.currentTimeMillis - start) + "msec")
    print("nodes: ")
    println(result.nodeCount)
    print("assignments: ")
    println(result.satCount)

    val weights = clauses_ls.map{c => c.body.size + 1}.toArray
    weights.map{println(_)}
/*    val bools = minimumWeight(result,weights)

    (0 to bools.size-1).foreach{i =>
      if(bools(i) == 1)
        println(clauses(i))
 }*/
    val start_top = System.currentTimeMillis
    // Array(0,...,1,..,1..,0)
    val top_result = minimumWeight(result,weights)
    println("Get Top Hypothesis Time: " + (System.currentTimeMillis - start_top) + "msec")
    val start_tie = System.currentTimeMillis
    val tie_result = minimumWeightTai(b,result,weights)
    println("Tie-BDD Construction Time: " + (System.currentTimeMillis - start_tie) + "msec")

    val best_hyp = clauses_ls.zip(top_result).filter{x => x._2 == 1}.map{_._1}
    println(best_hyp)
    println("TOP-TIE BDD")
    tie_result.printDot
    tie_result
  }

  def even_sample2 = {
    val nums = List(
      PredicateSymbol("e", Const("0")),
      PredicateSymbol("e", Const("1")),
      PredicateSymbol("e", Const("2")))
    val clauses = List(
      DefiniteClause(PredicateSymbol("e", Const("0"))),
      DefiniteClause(PredicateSymbol("e", NumVar("x", 0))),
      DefiniteClause(PredicateSymbol("e", Const("1"))),
      DefiniteClause(PredicateSymbol("e", NumVar("x", 1))),
      DefiniteClause(PredicateSymbol("e", NumVar("x", 1)), PredicateSymbol("e", NumVar("x", 0))),
      DefiniteClause(PredicateSymbol("e", Const("2"))),
      DefiniteClause(PredicateSymbol("e", NumVar("x", 2))),
      DefiniteClause(PredicateSymbol("e", NumVar("x", 2)), PredicateSymbol("e", NumVar("x", 0))),
      DefiniteClause(PredicateSymbol("e", NumVar("x", 2)), PredicateSymbol("e", NumVar("x", 1))),
      DefiniteClause(PredicateSymbol("e", NumVar("x", 2)), PredicateSymbol("e", NumVar("x", 0)), PredicateSymbol("e", NumVar("x", 1)))
    )

    def isEven(number: Int) = number % 2 == 0
    val b = BDDFactory.init(2000,2000)
    b.setVarNum(clauses.length)
    numsToBDD(b, nums, isEven, clauses)
  }

  def even_batch_test(size: Int) = {
    (2 to size).foreach{ s =>
      even_test(s,1000)
    }
  }

}
