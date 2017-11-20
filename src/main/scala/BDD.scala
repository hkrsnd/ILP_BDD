import net.sf.javabdd.BDD
import net.sf.javabdd.BDDFactory

trait BDDUtil{
  def seqAnd(bdds: Seq[BDD]): BDD = {
    def seqAndLoop(bdds: Seq[BDD], result: BDD): BDD = {
      if(bdds == List())
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
      if(bdds == List())
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

object BDDMain extends BDDUtil{
  // class(dog,mammal)
  def dataToBDD(data: RelationalData, clauses: List[DefiniteClause], b: BDDFactory): BDD = {
    val attrs_data = data.attrs
    val dep_clauses = PredicateLogic.getDependentClauses(attrs_data,clauses)
    val indexes = dep_clauses.map{c => clauses.indexOf(c)}
    val nodes = indexes.map{i => b.ithVar(i)}
    val bdd = seqOr(nodes.toSeq)
    bdd
  }

  def buildBDDFromRelationalData(data_filepath:String, values_filepath: String, positive_symbol: String, body_length: Int) = {
    val datas = IO.importData(data_filepath)
    val possible_values = IO.importPossibleValues(values_filepath)
    val clauses = PredicateLogic.generateCountedDefiniteClauses(body_length,possible_values,positive_symbol)
      .toList.sortWith{(x,y) => x.body.length < y.body.length }

    val b = BDDFactory.init(10000,10000)
    b.setVarNum(clauses.size)
    //    getDependentClauses(datas.head, clauses).map{println(_)}
    val label_bdds = datas.map{data =>
      (data.label, BDDMain.dataToBDD(data,clauses,b))}

    val positive_bdds = label_bdds.filter{lb => lb._1 == positive_symbol}.map{lb => lb._2}
    val negative_bdds = label_bdds.filter{lb => lb._1 != positive_symbol}.map{lb => lb._2.not}

    val positive_bdd = seqAnd(positive_bdds)
    val negative_bdd = seqAnd(negative_bdds)

    clauses.foreach{println(_)}
    positive_bdd.and(negative_bdd)
  }
}
