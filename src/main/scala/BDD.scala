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
  def inf = 99999999

  def minimumWeight(bdd: BDD, weights: Array[Int]): Array[Int] = {
    // nodeID -> (m, t)
    var mts: Map[Int,(Int,Boolean)] = Map.empty
    
    def minimumWeightLoop(bdd: BDD): Int = {
      if(bdd.isZero){
        inf
      }else if(bdd.isOne){
        0
      }else{
        val before_mt = mts.get(bdd.hashCode)
        before_mt match {
          // already visited
          case Some((m,t)) => m
          // first time
          case None =>
            val lw = minimumWeightLoop(bdd.low)
            val hw = minimumWeightLoop(bdd.high) + weights(bdd.`var`)
            if(lw < hw){
              // update mts map
              mts += (bdd.hashCode -> (lw,false))
              lw
            }else{
              // update mts map
              mts += (bdd.hashCode -> (hw,true))
              hw
          }
        }
      }
    }

    println("calculate ms ts...")
    // calculate ms ts
    minimumWeightLoop(bdd)

    // create x's
    // init xs = Array(0,0,...,0)
    var xs: Array[Int] = Array.fill(weights.size)(0)
    var tmp_bdd = bdd

    // from root to bottom
    while(!(tmp_bdd.nodeCount == 1)){
      val mt = mts.get(tmp_bdd.hashCode)
      mt match {
        case Some((m,t)) =>
          if(t){
            xs(tmp_bdd.`var`) = 1
            tmp_bdd = tmp_bdd.high
          }else{
            xs(tmp_bdd.`var`) = 0
            tmp_bdd = tmp_bdd.low
          }
        case _ => throw new Exception("Internal Error: Invalid mts")
      }
    }
    return xs
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
