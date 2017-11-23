import net.sf.javabdd.BDD
import net.sf.javabdd.BDDFactory

trait BDDUtil{
  def seqAnd(bdds: Seq[BDD]): BDD = {
    def seqAndLoop(bdds: Seq[BDD], result: BDD): BDD = {
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

object BDDMain extends BDDUtil{
  def dataToBDD(data: RelationalData, clauses: List[DefiniteClause], b: BDDFactory): BDD = {
    val attrs_data = data.attrs
    val dep_clauses = PredicateLogic.getDependentClauses(attrs_data,clauses)
    val indexes = dep_clauses.map{c => clauses.indexOf(c)}
    val nodes = indexes.map{i => b.ithVar(i)}
    val bdd = seqOr(nodes.toSeq)
    bdd
  }

  def buildBDDFromRelationalData(datas: List[RelationalData], possible_values: Map[String, Set[Const]], clauses: List[DefiniteClause], positive_symbol: String, body_length: Int) = {
    val b = BDDFactory.init(10000,10000)
    b.setVarNum(clauses.size)
    val label_bdds = datas.map{data =>
      (data.label, BDDMain.dataToBDD(data,clauses,b))}
    val positive_bdds = label_bdds.filter{lb => lb._1 == positive_symbol}.map{lb => lb._2}
    val negative_bdds = label_bdds.filter{lb => lb._1 != positive_symbol}.map{lb => lb._2.not}
    val positive_bdd = seqAnd(positive_bdds)
    val negative_bdd = seqAnd(negative_bdds)
    positive_bdd.and(negative_bdd)
  }
}
