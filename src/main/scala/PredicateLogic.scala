/*
 f:            predicate function
 t_1,...,t_n:  term
 
 Term := Const
 | Var
 | f(t_1,t_2, ... ,t_n)
 */
abstract class Term

case class Const(name: String) extends Term

case class Var(name: String) extends Term

case class PredicateFunction(name: String, terms: Term*) extends Term

/*
 p:           predicate symbol
 t1,...,t_n:  term
 
 Atom := p(t1,t_2,...,t_n)
 */
abstract class Atom

case class PredicateSymbol(name: String, terms: Term*) extends Atom

case class DefiniteClause(head: PredicateSymbol , body: PredicateSymbol*)


trait SetUtil {
  def seqCartesianProduct[A](sets: Seq[Set[A]]): Seq[Seq[A]] = {
    def _seqCartesianProduct[B](sets: Seq[Set[B]], product: Seq[Seq[B]]): Seq[Seq[B]] = {
      if(sets.length == 0){
        product
      } else{
        val new_product = for{
          x <- product // x: seq[A]
          y  <- sets.head // y: A
        } yield {
          x :+ y
        }
        _seqCartesianProduct(sets.tail, new_product)
      }
    }
    _seqCartesianProduct(sets, Seq(Seq()))
  }

  def powerSet[A](s:TraversableOnce[A]) = s.foldLeft(Set(Set.empty[A])) {
    (set, element) => set union (set map (_ + element))
  }

  def powerSetWithSize[A](s: TraversableOnce[A], size: Int) = {
    s.foldLeft(Set(Set.empty[A])) {
      (set, element) =>
      println(element)
      val newset = set.filter{x => x.size <= size}
      newset union (newset map (_ + element))}
  }
}


object PredicateLogic extends SetUtil{
  def getDependentClauses(data: Map[String,Term], clauses: List[DefiniteClause]): Set[DefiniteClause] = {
    clauses.filter{clause =>
      clause.body.toList match { 
        case List() => true
        case predicate_symbols => predicate_symbols.map{predicate_symbol =>
          data.get(predicate_symbol.name) match {
            case None => false
            case Some(const) => const == predicate_symbol.terms.last
          }
        }.foldLeft(true){_ && _}
      }
    }.toSet
  }

  def generateCountedDefiniteClauseBodies(body_length: Int, attr_to_possible_values: Map[String, Set[Const]], positive_class_name: String): Set[Seq[PredicateSymbol]]
  = {
    val keys = attr_to_possible_values.map{_._1}
//    val key_powerset = powerSet(keys).filter{x => x.size <= body_length}
    val key_powerset = powerSetWithSize(keys,body_length).filter{x => x.size <= body_length}
    val key_and_values_powerset: Set[Set[(String,Set[Const])]] = key_powerset.map{key_set =>
      key_set.map{key =>
        (key, attr_to_possible_values.getOrElse(key, Set()))}
    }

    val counted_clauses = key_and_values_powerset.map{key_values_tuple_set =>
      val keys_seq = key_values_tuple_set.map{_._1}.toSeq
      val possible_values_seq = key_values_tuple_set.toSeq.map{_._2}

      val product = seqCartesianProduct(possible_values_seq)
      product.map{values =>
        (0 to values.length-1).map{i =>
          PredicateSymbol(keys_seq(i), Var("x"), values(i))
        }
      }
    }

    // rmeove nest
    var result_set: Set[Seq[PredicateSymbol]] = Set()
    counted_clauses.map{ ls =>
      ls.map{vec => result_set += vec}
    }
    result_set
  }

  def generateCountedDefiniteClauses(body_length: Int, attr_to_possible_values: Map[String, Set[Const]], positive_class_name: String):Set[DefiniteClause] = {
    val bodies = generateCountedDefiniteClauseBodies(body_length,attr_to_possible_values, positive_class_name)

    bodies.map{body =>
      DefiniteClause(PredicateSymbol("class",Var("x"),Const(positive_class_name)),
      body: _*)}
  }

  def removeDontCareClauses(clauses: List[DefiniteClause], datas: List[RelationalData]): Set[DefiniteClause] = {
    val empty: Set[DefiniteClause] = Set()
    datas.map{data =>
      getDependentClauses(data.attrs,clauses)
    }.foldLeft(empty){_ union _}
  }
}



object PredicateTest extends BDDUtil with SetUtil{
  import PredicateLogic._
  import net.sf.javabdd.BDD
  import net.sf.javabdd.BDDFactory

  def test3() = {
    val datas = IO.importData("resource/data/mammal.data")
//    println(datas)
    val possible_values = IO.importPossibleValues("resource/data/mammal.values")

    val clauses = generateCountedDefiniteClauses(3,possible_values,"positive")
      .toList.sortWith{(x,y) => x.body.length < y.body.length }


    val b = BDDFactory.init(10000,10000)
    b.setVarNum(clauses.size)
    //    getDependentClauses(datas.head, clauses).map{println(_)}
    val label_bdds = datas.map{data =>
      (data.label, BDDMain.dataToBDD(data,clauses,b))}

    val positive_bdds = label_bdds.filter{lb => lb._1 == "positive"}.map{lb => lb._2}
    val negative_bdds = label_bdds.filter{lb => lb._1 != "positive"}.map{lb => lb._2.not}

    val positive_bdd = seqAnd(positive_bdds)
    val negative_bdd = seqAnd(negative_bdds)

    clauses.foreach{println(_)}
    positive_bdd.and(negative_bdd)
  }


  def test5() = {
    val m = Seq(Set(Const("o"), Const("x"), Const("b")),Set(Const("o"), Const("x"), Const("b")),Set(Const("o"), Const("x"), Const("b")),Set(Const("o"), Const("x"), Const("b")))
    seqCartesianProduct(m)
  }


/*
  def test2() = {
//    val datas = IO.importData("resource/data/mammal.data")
    val data1 =
      RelationalData("positive", Map[String,String]("body"->"hair","milk"->"yes"))
    val data2 =
      RelationalData("negative", Map[String,String]("body"->"skin","milk"->"no"))

    val attr_to_possible_values = Map[String,Set[String]]("body" -> Set("hair","skin"),
    "milk" -> Set("yes","no"))


    generateCountedDefiniteClauses(2,attr_to_possible_values,"positive")
  }


}
 */

/*
  def test1() = {
    val c0 = DefiniteClause(PredicateString('class, Var('z), Const('mammal)))
    val c1 = DefiniteClause(PredicateString('class, Var('z), Const('mammal)), PredicateString('body, (Var('z)), Const('hair)))
    val c2 = DefiniteClause(PredicateString('class, Var('z), Const('mammal)), PredicateString('body, (Var('z)), Const('skin)))
    val c3 = DefiniteClause(PredicateString('class, Var('z), Const('mammal)), PredicateString('milk, (Var('z)), Const('yes)))
    val c4 = DefiniteClause(PredicateString('class, Var('z), Const('mammal)), PredicateString('milk, (Var('z)), Const('no)))
    val c5 = DefiniteClause(PredicateString('class, Var('z), Const('mammal)), PredicateString('body, (Var('z)), Const('hair)),  PredicateString('milk, (Var('z)), Const('yes)))
    val c6 = DefiniteClause(PredicateString('mclass, Var('z), Const('mammal)), Predicscala object dataateString('body, (Var('z)), Const('hair)),  PredicateString('milk, (Var('z)), Const('no)))
    val c7 = DefiniteClause(PredicateString('class, Var('z), Const('mammal)), PredicateString('body, (Var('z)), Const('skin)),  PredicateString('milk, (Var('z)), Const('yes)))
    val c8 = DefiniteClause(PredicateString('class, Var('z), Const('mammal)), PredicateString('body, (Var('z)), Const('skin)),  PredicateString('milk, (Var('z)), Const('no)))

    val clauses = List(c0,c1,c2,c3,c4,c5,c6,c7,c8)


    val datas = List(
      Map[String,Term]('id -> Const('dog), 'body -> Const('hair), 'milk -> Const('yes)),
      Map[String,Term]('id -> Const('dolphin), 'body -> Const('skin), 'milk -> Const('yes)),
      Map[String,Term]('id -> Const('shark), 'body -> Const('skin), 'milk -> Const('no)))

    val b = BDDFactory.init(1000,1000)
    b.setVarNum(9)
    //    getDependentClauses(datas.head, clauses).map{println(_)}
    val bdds = datas.map{data =>
      BDDMain.dataToBDD(data,clauses,b)}
    bdds(0).and(bdds(1)).and(bdds(2).not)
  }
*/

}
