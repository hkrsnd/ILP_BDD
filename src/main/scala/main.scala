import net.sf.javabdd.BDD
import net.sf.javabdd.BDDFactory


object Test extends BDDUtil{
  def main() = {
    //    println("hello ILP BDD project")
    val b = BDDFactory.init(1000,1000)
    b.setVarNum(10);
    val v1 = b.ithVar(0)
    val v2 = b.ithVar(1)
    val v3 = b.ithVar(2)
    val v4 = b.ithVar(3)
    val v5 = b.ithVar(5)

    val c1 = v1.and(v2)
    val c2 = c1.or(v3)
    val c = seqOr(List(c1,v3,v4))
    val d = v5.or(c)
//    val d = c.or(v4)
    d.printDot()
    d.printSet()
  }
}

abstract class Num{
  def abs(): Int
  def isVar(): Boolean
}
class Var(name: String) extends Num{
  def abs(): Int = name.length
  def isVar(): Boolean = true
}
class Const(value: Int) extends Num{
  def abs(): Int = 1 //　基礎節の絶対値においては定数の数を考える 各定数につき１
  def isVar(): Boolean = false
}
class Atom(args: Num*){
  def abs(): Int = {
    args.foldLeft(0)(_.abs + _.abs)
  }
  def hasVar(): Boolean =
    //args.head.isVar
    args.map{a => a.isVar()}.foldLeft(false){_ || _}
//    args.foldLeft(false){(x: Num,y: Num) => x.isVar || y.isVar}
}
case class DefClause(body: Atom, head: Atom*)
case class Hypothesis(clauses: List[DefClause])


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
  // ILPの問題からその解を列挙するBDDを構築する
  def buildILPBDD(atoms: List[Atom], clauses: List[DefClause], positives: List[Atom], negatives: List[Atom]): Map[Atom, BDD] = {
    // ε+ ∪ ε-
    val pos_negs = List.concat(positives,negatives)
    //    val derivable_indexes = for {e <- pos_negs} yield getDerivableClauseIndexes(e,clauses)
    val atom_to_derivable_clauses_map = generateDerivableClausesMap(atoms,clauses)
    buildBDDs(atoms,clauses,atom_to_derivable_clauses_map)
  }

  // 各原子論式について、BDDを構築する
  def buildBDDs(atoms: List[Atom], clauses: List[DefClause], atom_to_derivable_clauses_map: Map[Atom, List[DefClause]]): Map[Atom, BDD] = {
    val b = BDDFactory.init(1000,1000)
    b.setVarNum(clauses.length) // 各clauseのインデックスが論理変数名に対応

    def buildBDDsLoop(atoms: List[Atom], clauses: List[DefClause], atom_to_derivable_clauses_map: Map[Atom, List[DefClause]], atom_to_bdd_map: Map[Atom, BDD]): Map[Atom, BDD] = {
      if(atoms == List()){
        atom_to_bdd_map
      } else {
        val atom = atoms.head
        val derivable_clauses = atom_to_derivable_clauses_map.getOrElse(atom, List())
        print(atom)
        print(" : ")
        println(clauses.indexOf(DefClause(atom)))
        print("derivables: ")
        println(derivable_clauses)
        //　導出しうる節がない場合
        if(derivable_clauses == List()){
          val atom_bdd = b.ithVar(clauses.indexOf(DefClause(atom)))
          println("No derivable clauses")
          atom_bdd.printDot()
          val new_atom_to_bdd_map = atom_to_bdd_map + (atom -> atom_bdd)
          buildBDDsLoop(atoms.tail, clauses, atom_to_derivable_clauses_map, new_atom_to_bdd_map)
        } else {
          //他に導出しうる節がある場合
          val derivable_clause_bdds = derivable_clauses.map{derivable_clause => 
            clauseToBDD(derivable_clause, clauses.indexOf(derivable_clause), atom_to_bdd_map, b)}.toList
          val atom_own_bdd = b.ithVar(clauses.indexOf(DefClause(atom)))
          val atom_bdd = atom_own_bdd.or(seqOr(derivable_clause_bdds))
          println("Some derivable clauses")
          println("derivable clause BDDs")
          derivable_clause_bdds.map{bdd => bdd.printDot()}
          println("getting OR")
          seqOr(derivable_clause_bdds).printDot()
          println("own BDD")
          atom_own_bdd.printDot()
          println("atom BDD")
          atom_bdd.printDot()
          val new_atom_to_bdd_map = atom_to_bdd_map + (atom -> atom_bdd)
          buildBDDsLoop(atoms.tail, clauses, atom_to_derivable_clauses_map, new_atom_to_bdd_map)
        }
      }
    }

    // call loop
    buildBDDsLoop(atoms, clauses, atom_to_derivable_clauses_map, Map().empty)
  }

  def clauseToBDD(clause: DefClause, index: Int, atom_to_bdd_map: Map[Atom, BDD], b: BDDFactory): BDD = {
    if (clause.head.length <= 0)
      b.ithVar(index)
    else {
      val own_bdd = b.ithVar(index)
      val head_atoms = clause.head.toList
      val head_bdds = head_atoms.map{atom => atom_to_bdd_map(atom)}
      val heads_bdd = seqAnd(head_bdds)
//      print("clause:  ")
//      println(clause)
//      own_bdd.and(heads_bdd).printDot
      own_bdd.and(heads_bdd)
    }
  }

  def generateDerivableClausesMap(atoms: List[Atom], clauses: List[DefClause]): Map[Atom, List[DefClause]] = {
    //    val indexes = (0 to clauses.length-1).filter{i => clauses(i).instanceOf[Var] && atom.abs >= clauses(i).abs}
    var atom_to_derivable_clauses_map:Map[Atom, List[DefClause]] = Map().empty // init
    atoms.map{atom =>
      val derivables = clauses.filter{c =>
        // c = p(x) <- hoge かつ |p(x)| <= |atom| のとき p(x)からAtomを導出できる p(x) から p(0), p(sx)からp(ssx)など
        (c.body.hasVar && c.body.abs <= atom.abs && (c.body != atom || c.head.length > 0)) || (c.body == atom && c.head.length > 0)}.toList
      atom_to_derivable_clauses_map += (atom -> derivables)
    }
    atom_to_derivable_clauses_map
  }
}


object Main extends BDDUtil{
  val p0 = new Atom(new Const(0))
  val p1 = new Atom(new Const(1))
  val p2 = new Atom(new Const(2))
  val px = new Atom(new Var("x"))
  val psx = new Atom(new Var("sx"))
  val pssx = new Atom(new Var("ssx"))
  val atoms = List(px,p0,psx,p1,pssx,p2)

  val positives = List(p0, p2)
  val negatives = List(p1)

  val c0 = DefClause(p0)
  val c1 = DefClause(px)
  val c2 = DefClause(p1)
  val c3 = DefClause(p1,p0)
  val c4 = DefClause(psx)
  val c5 = DefClause(psx,px)
  val c6 = DefClause(psx,px,p0)
  val c7 = DefClause(p2)
  val c8 = DefClause(p2,p0)
  val c9 = DefClause(p2,p1)
  val c10 = DefClause(p2,p0,p1)
  val c11 = DefClause(pssx)
  val c12 = DefClause(pssx,psx)
  val c13 = DefClause(pssx,px,psx)


  val clauses = List(c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13)
  val prop_vars = ('a' to 'z').toList.take(14)



  def main(args: Array[String]) = {
    val atom_to_bdd_map = BDDMain.buildILPBDD(atoms,clauses,positives,negatives)
    val positive_bdds = positives.map{p => atom_to_bdd_map(p)}
    val negative_bdds = negatives.map{p => atom_to_bdd_map(p)}.map{bdd => bdd.not}
    val bdd_solution = seqAnd(positive_bdds).and(seqAnd(negative_bdds))
    bdd_solution.printDot()
    println("completed")
  }
}
