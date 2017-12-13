import net.sf.javabdd.BDD
import net.sf.javabdd.BDDFactory

import collection.JavaConversions._

trait BDDUtil{
  def seqAnd(bdds: Seq[BDD]): BDD = {
    val bddnum = bdds.length
    def seqAndLoop(bdds: Seq[BDD], result: BDD): BDD = {
      println((bddnum-bdds.length).toString + " / " + bddnum.toString)
      if(bdds.length == 0)
        result
      else{
        val bdd = bdds.head
        val new_result = bdd.and(result)
        seqAndLoop(bdds.tail, new_result)
      }
    }
    seqAndLoop(bdds.tail, bdds.head)
  }
  def seqOr(bdds: Seq[BDD]): BDD = {
    def seqOrLoop(bdds: Seq[BDD], result: BDD): BDD = {
      if(bdds.length == 0)
        result
      else{
        val bdd = bdds.head
        val new_result = bdd.or(result)
        seqOrLoop(bdds.tail, new_result)
      }
    }
    seqOrLoop(bdds.tail, bdds.head)
  }
}

trait BDDAlgo{
  def inf = 10000
  def minimumWeight(bdd: BDD, weights: Array[Int]): Array[Int] = {
    var mts: Map[BDD,(Int,Boolean)] = Map.empty

    def minimumWeightLoop(bdd: BDD): Int = {

      if(bdd.isZero){
        inf
      }else if(bdd.isOne){
        0
      }else{
        print("bdd: ")
        println(bdd.`var`)
        print(bdd.nodeCount)
        println(" nodes")
        val low = bdd.low
        val high = bdd.high
        val lw = minimumWeightLoop(low)
        val hw = minimumWeightLoop(high) + weights(bdd.`var`)
        if (lw < hw){
          if(mts.get(bdd) == None)
            mts += (bdd -> (lw,false))
          lw
        } else {
          if(mts.get(bdd) == None)
            mts += (bdd -> (hw,true))
          hw
        }
      }
    }


    println("calculate ms ts...")
    // calculate ms ts
    minimumWeightLoop(bdd)

    // create x's
    var xs: Array[Int] = Array.fill(weights.size)(0)
    var tmp_bdd = bdd

    while(!tmp_bdd.isOne){
      if(mts.getOrElse(tmp_bdd,(0,false))._2){
        xs(tmp_bdd.`var`) = 1
        tmp_bdd = tmp_bdd.high
       }else{
        xs(tmp_bdd.`var`) = 0
        tmp_bdd = tmp_bdd.low
       }
    }
    xs
  }
}

object BDDMain extends BDDUtil with BDDAlgo{
  def dataToBDD(data: RelationalData, clauses: List[DefiniteClause], b: BDDFactory): BDD = {
    val attrs_data = data.attrs
    val dep_clauses = PredicateLogic.getDependentClauses(attrs_data,clauses)
    val indexes = dep_clauses.map{c => clauses.indexOf(c)}
    val nodes = indexes.map{i => b.ithVar(i)}
    val bdd = seqOr(nodes.toSeq)
    bdd
  }

  def buildBDDFromRelationalData(datas: List[RelationalData], possible_values: Map[String, Set[Const]], clauses: List[DefiniteClause], positive_symbol: String, body_length: Int) = {
    val b = BDDFactory.init(5500000,5500000)
    b.setVarNum(clauses.size)
    var i = 0
    val label_bdds = datas.map{data =>
      print("data")
      println(i)
      i += 1
      (data.label, BDDMain.dataToBDD(data,clauses,b))}
    val positive_bdds = label_bdds.filter{lb => lb._1 == positive_symbol}.map{lb => lb._2}
    val negative_bdds = label_bdds.filter{lb => lb._1 != positive_symbol}.map{lb => lb._2.not}
    val positive_bdd = seqAnd(positive_bdds)
    val negative_bdd = seqAnd(negative_bdds)
    positive_bdd.and(negative_bdd)
  }

/*  def BDDToHypothesises(bdd: BDD, clauses: List[DefiniteClause]) = {
    val bools_array = bdd.allsat.map{x => x}.toList
    bools_array.map{bools =>
      val zipped = bools.zip(clauses)
      zipped.filter{z => z._1 == 1}.map{x => x._2}
    }
  }*/
}
