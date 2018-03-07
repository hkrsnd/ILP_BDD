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

case class NumVar(name: String, pre: Int) extends Term

case class PredicateFunction(name: String, terms: Term*) extends Term

/*
 p:           predicate symbol
 t1,...,t_n:  term
 
 Atom := p(t1,t_2,...,t_n)
 */
abstract class Atom

case class PredicateSymbol(name: String, terms: Term*) extends Atom

case class DefiniteClause(head: PredicateSymbol, body: PredicateSymbol*)

case class Hypothesis(clauses: List[DefiniteClause])



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
            case None => println(predicate_symbol.name) ;false
            case Some(const) => const == predicate_symbol.terms.last
          }
        }.foldLeft(true){_ && _}
      }
    }.toSet
  }
  
  def generateCountedDefiniteClauseBodies(body_length: Int, attr_to_possible_values: Map[String, Set[Const]], positive_class_name: String): Set[Seq[PredicateSymbol]]
  = {
    val keys = attr_to_possible_values.map{_._1}
//    println(keys)
    //    val key_powerset = powerSet(keys).filter{x => x.size <= body_length}
    val key_powerset = powerSetWithSize(keys,body_length).filter{x => x.size >= body_length}
//    println(key_powerset)
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
    // remove nest
    var result_set: Set[Seq[PredicateSymbol]] = Set()
    counted_clauses.map{ ls =>
      ls.map{vec => result_set += vec}
    }
    println(result_set)
    result_set
  }

  def generateCountedDefiniteClauses(body_length: Int, attr_to_possible_values: Map[String, Set[Const]], positive_class_name: String): Set[DefiniteClause] = {
    val bodies = generateCountedDefiniteClauseBodies(body_length,attr_to_possible_values, positive_class_name)

    bodies.map{body =>
      DefiniteClause(PredicateSymbol("class",Var("x"),Const(positive_class_name)),
      body: _*)}
  }

/*
  def generateCountedDefiniteClauses(body_length: Int, attr_to_possible_values: Map[String, Set[Const]], positive_class_name: String): Set[DefiniteClause] = {
    val keys = attr_to_possible_values.map{_._1}.toSeq
    val values_list = seqCartesianProduct(attr_to_possible_values.map{_._2}.toSeq)
    val bodies = values_list.map{ values =>
      (0 to values.length -1).map{i =>
        PredicateSymbol(keys(i), Var("x"), values(i))
      }
    }
    bodies.map{ body =>
    DefiniteClause(PredicateSymbol("class",Var("x"),Const(positive_class_name)),
      body: _*)}.toSet
  }
 */
  def removeDontCareClauses(clauses: List[DefiniteClause], datas: List[RelationalData]): Set[DefiniteClause] = {
    val empty: Set[DefiniteClause] = Set()
    datas.map{data =>
      getDependentClauses(data.attrs,clauses)
    }.foldLeft(empty){_ union _}
  }


  def getCoveredDatasByClause(clause: DefiniteClause, data_list: List[RelationalData]): List[RelationalData] = {
    val body_symbols = clause.body
    val attrs_tuples = body_symbols.map{x => (x.name, x.terms.last)}
    data_list.filter{data =>
      val attr_bools = attrs_tuples.map{x => data.attrs(x._1) == x._2}
      attr_bools.foldLeft(true){_ && _}
    }
  }

  def hypothesisLength(hypo: Hypothesis): Int = {
    hypo.clauses.map{_.body.length}.foldLeft(0){_ + _}
  }
}

object NumPredicateLogic extends SetUtil{
/*  implicit def intToString(n: Int) = {
    n.toString
  }
  implicit def stringToInt(s: String) = {
    s.toInt
  }*/

  def literalToInt(num_literal: PredicateSymbol): Int = {
    num_literal match{
      case PredicateSymbol(name, Const(num)) => num.toInt
    }
  }
  def intToLiteral(num_int: Int): PredicateSymbol = {
    PredicateSymbol("p", Const(num_int.toString))
  }

  def generateAtoms(size: Int): List[PredicateSymbol] = {
    val ls = Range(0, size).toList
    ls.map{x => PredicateSymbol("p", Const(x.toString))}
  }

  def substitute(variable: Atom, value: Int): PredicateSymbol = {
    variable match{
      case PredicateSymbol(name, terms @_*) =>
        terms.head match {
          case NumVar(var_name, pre) => PredicateSymbol(name, Const((pre + value).toString))
          case Const(num) => PredicateSymbol(name, Const(num))
        }
    }
  }

  def getCountedDefiniteClauses(head_p: PredicateSymbol, clauses: List[DefiniteClause]): List[DefiniteClause] = {
    clauses.filter{c => c match{
      case DefiniteClause(head, body @_*) =>
        head match{
          case PredicateSymbol(name, Const(num)) => head_p == PredicateSymbol("p", Const(num))
          case PredicateSymbol(name, NumVar(nv_name, pre)) =>
            head_p match{
              case PredicateSymbol(head_p_name, Const(num)) =>
                // s(s(x)) (pre==2) から s(s(s(0)))は導出できるが s(0)は導出できない
                num.toInt >= pre
            }
        }
    }}.map{c => c match{
      // e(s^2(x)) <- e(x) のxに代入を行う
      case DefiniteClause(head, body @_*) =>
        head match{
          case PredicateSymbol(head_name, NumVar(name, pre)) =>
            head_p match{
              case PredicateSymbol(head_p_name, Const(num)) =>
                val subs_value = num.toInt - pre
                val subs_head = substitute(head, subs_value)
                val subs_bodies = body.map{b => substitute(b, subs_value)}
                DefiniteClause(subs_head, subs_bodies: _*)
                DefiniteClause(head, body: _*)
            }
          case PredicateSymbol(head_name, Const(num)) =>
            DefiniteClause(head, body: _*)
        }
    }}
  }

  def generateDefiniteClauses(max_num: Int): List[DefiniteClause] = {
    Range(0, max_num+1).toList.map{ num =>
      val num_set = Range(0, num).toSet
      val power_set_list = powerSet(num_set)
        //.map{_.map{s => PredicateSymbol("p", Const(s.toString))}}
        .map{_.toList}
        .toList.sortWith{_.length < _.length}
      power_set_list.map{body_num_list =>
        if (body_num_list.length > 0){
          val head_var = NumVar("x", num)
          val body_var_list = body_num_list.map{body_num => NumVar("x", body_num)}
          val body_list = body_var_list.map{body_var => PredicateSymbol("p", body_var)}
          List(DefiniteClause(PredicateSymbol("p", head_var), body_list: _*))
        } else {
        val head_var = NumVar("x", num)
        List(
          DefiniteClause(PredicateSymbol("p", head_var)),
          DefiniteClause(PredicateSymbol("p", Const(num.toString))))
        }}.flatten
      }.flatten
  }
}


object NumPredicateTest extends BDDUtil with SetUtil{
  import NumPredicateLogic._
  import BDDMain._
  import net.sf.javabdd.BDD
  import net.sf.javabdd.BDDFactory

  def isEven(number: Int) = number % 2 == 0
  def isOdd(number: Int) = !isEven(number)

  def even_test(size: Int): BDD = {

    val nums = Range(0,size+1).toList.map{x => intToLiteral(x)}
    val clauses = generateDefiniteClauses(size)
    val b = BDDFactory.init(99999999, 99999999)
    b.setVarNum(clauses.length)
    numsToBDD(b, nums, isEven, clauses)
  }
}

object PredicateTest extends BDDUtil with SetUtil{
  import PredicateLogic._

  import net.sf.javabdd.BDD
  import net.sf.javabdd.BDDFactory

  def slide_test() = {
    val b = BDDFactory.init(20,20)
    b.setVarNum(9)
    // 0 1 3 5
    val bdd_dog = b.ithVar(0).or(b.ithVar(1)).or(b.ithVar(3)).or(b.ithVar(5))
    // 0 2 3 7
    val bdd_dolphin = b.ithVar(0).or(b.ithVar(2)).or(b.ithVar(3)).or(b.ithVar(7))
    // 0 2 4 8
    val bdd_shark = b.ithVar(0).or(b.ithVar(2)).or(b.ithVar(4)).or(b.ithVar(8))
    val bdd = bdd_dog.and(bdd_dolphin).and(bdd_shark.not)

    bdd_dog.printDot
    bdd_dolphin.printDot
    bdd_shark.not.printDot
    bdd.printDot
  }

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
