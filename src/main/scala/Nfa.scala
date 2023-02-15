import scala.annotation.tailrec

class Nfa[A](val K: Set[A], val Sigma: Set[String], val Delta: Set[(A, String, A)], val q0: A, val F: Set[A]) {

  def map[B](f: A => B) : Nfa[B] = {
    new Nfa[B](
      K.map(f),
      Sigma,
      Delta.map(x => (f(x._1), x._2, f(x._3))),
      f(q0),
      F.map(f)
    )
  }

  def next(state:A, c: Char): Set[A] = {
    @tailrec
    def aux(Delta: Set[(A, String, A)], stareActuala: A, ch: Char, ret: Set[A]): Set[A] = {
      if (Delta.isEmpty)
        return ret
      if (Delta.head._1 == stareActuala && Delta.head._2.head == ch) {
        aux(Delta.tail, stareActuala, ch, ret + Delta.head._3)
      }
      else {
        aux(Delta.tail, stareActuala, ch, ret)
      }
    }
    aux(Delta, state, c, Set())
  }

  def accepts(str: String): Boolean = {
    def aux(stare: A, sir: String): Boolean = {
      if (sir.isEmpty)
        if (isFinal(stare))
          true
        else
          if(next(stare, '~').isEmpty)
            false
          else
            next(stare, '~').foldLeft(false)((acc, x) => acc || aux(x, sir))
      else {
        val s = next(stare, sir.head).foldLeft(false)((acc, x) => acc || aux(x, sir.tail))
        val ss = next(stare, '~').foldLeft(false)((acc, x) => acc || aux(x, sir))
        s || ss
      }
    }
    aux(q0, str)
  }

  def epsilon(state: A): Set[A] = {
    val aux = next(state, '~').diff(Set(state))
    aux.isEmpty match {
      case true => Set(state)
      case _ => aux.foldLeft(Set[A]())((a: Set[A], s: A) => a.union(epsilon(s))).union(Set(state))
    }
  }

  def getStates: Set[A] = K

  def isFinal(state: A): Boolean = F.contains(state)
}

object Nfa {
  def fromPrenex(str: String): Nfa[Int] = {
    val words = str.split(" ").reverse
    @tailrec
    def aux(NFAs: List[Nfa[Int]], index: Int): Nfa[Int] = {
      if (index == words.length)
        NFAs.head
      else
        if (words(index) == "UNION")
          aux(UNION(NFAs.head, NFAs.tail.head)::NFAs.tail.tail, index + 1)
        else if (words(index) == "CONCAT")
          aux(CONCAT(NFAs.head, NFAs.tail.head)::NFAs.tail.tail, index + 1)
        else if (words(index) == "STAR")
          aux(STAR(NFAs.head)::NFAs.tail, index + 1)
        else if (words(index) == "MAYBE")
          aux(MAYBE(NFAs.head)::NFAs.tail, index + 1)
        else if (words(index) == "PLUS")
          aux(PLUS(NFAs.head)::NFAs.tail, index + 1)
        else if (words(index) == "void")
          aux(NFAs ++ List(VOID()), index + 1)
        else if (words(index) == "eps")
          aux(NFAs ++ List(EPSILON()), index + 1)
        else {
          if (words(index) != "'")
            aux(CHARLIE(words(index))::NFAs, index + 1)
          else {
            if (words.length == index + 1)
              aux(CHARLIE(words(index))::NFAs, index + 1)
            else
              aux(CHARLIE(" ")::NFAs, index + 2)
          }
        }
    }
    aux(List(), 0)
  }

  def Nfa2Dfa(a: Nfa[Int]): Dfa[Int] =
  {
    def subSet(nfa: Nfa[Int]): Dfa[Set[Int]] = {
      val multistareInit: Set[Int] = nfa.epsilon(nfa.q0)
      var multistari: Set[Set[Int]] = Set(multistareInit)
      var tranzitii: Set[(Set[Int], String, Set[Int])] = Set()

      def f(multistare: Set[Int], ch: Char): Set[Int] = multistare.flatMap(s => nfa.next(s, ch)).flatMap(s => nfa.epsilon(s))

      def subSet_aux(multistare: Set[Int]): Unit = {
        multistare.isEmpty match {
          case true =>
          case _ => {
            val noiTranzitii = nfa.Sigma.diff(Set("~")).map(ch => (multistare, ch, f(multistare, ch.charAt(0))))
            val continue = noiTranzitii.filter(t => t._3.isEmpty || !multistari.contains(t._3))
            noiTranzitii.foreach(t => multistari = multistari.union(Set(t._3)))
            tranzitii = tranzitii.union(noiTranzitii)
            continue.foreach(t => subSet_aux(t._3))
          }
        }
      }

      subSet_aux(multistareInit)
      new Dfa[Set[Int]](multistari, nfa.Sigma.diff(Set("~")), tranzitii, multistareInit, multistari.filter(ms => ms.exists(nfa.isFinal)), Set())
    }

    def transform(stare: Set[Int]): Int = {
      stare.isEmpty match {
        case true => -1
        case _ => stare.toList.sorted.foldLeft(0)((acc, x) => acc * 10 + x)
      }
    }

    val aux = subSet(a)
    aux.map(transform)
  }

  def UNION(a: Nfa[Int], b: Nfa[Int]): Nfa[Int] = {
    val c = b.map(_ + a.getStates.size)
    val q0prim = a.getStates.size + c.getStates.size;
    val Deltaprim = a.Delta.union(c.Delta.union(Set[(Int,String,Int)]
      ( (q0prim, "~", a.q0),
        (q0prim, "~", c.q0)
      )))
    val Fprim = Set[Int](a.getStates.size + c.getStates.size + 1)
    val epsilon1 = a.F.foldLeft(Set[(Int, String, Int)]())((acc, sf) => acc.union(Set((sf,"~",a.getStates.size + c.getStates.size + 1))))
    val epsilon2 = c.F.foldLeft(Set[(Int, String, Int)]())((acc, sf) => acc.union(Set((sf,"~",a.getStates.size + c.getStates.size + 1))))

    new Nfa[Int](
      a.getStates.union(c.getStates.union(Fprim.union(Set[Int](q0prim)))),
      a.Sigma.union(c.Sigma),
      Deltaprim.union(epsilon1.union(epsilon2)),
      q0prim,
      Fprim
    )
  }

  def STAR(a: Nfa[Int]): Nfa[Int] = {
    val init = a.getStates.size
    val fin = a.getStates.size + 1

    val initToFin = Set[(Int, String, Int)]((init, "~", fin), (init, "~", a.q0))
    val repeat = a.F.foldLeft(Set[(Int, String, Int)]())((acc, s)=>acc.union(Set[(Int, String, Int)]((s, "~", a.q0))))
    val fins = a.F.foldLeft(Set[(Int, String, Int)]())((acc, sf) => acc.union(Set[(Int, String, Int)]((sf, "~", fin))))

    new Nfa[Int](
      a.getStates.union(Set[Int](init, fin)),
      a.Sigma,
      a.Delta.union(fins.union(repeat.union(initToFin))),
      init,
      Set[Int](fin)
    )
  }

  def CONCAT(a: Nfa[Int], b: Nfa[Int]): Nfa[Int] = {
    val c = b.map(_ + a.getStates.size)
    val epsilon = a.F.foldLeft(Set[(Int, String, Int)]())((acc, sf) => acc.union(Set((sf, "~", c.q0))))
    new Nfa[Int](
      a.getStates.union(c.getStates),
      a.Sigma.union(c.Sigma),
      a.Delta.union(c.Delta.union(epsilon)),
      a.q0,
      c.F)
  }

  def VOID(): Nfa[Int] = {
    new Nfa[Int](
      Set[Int](0,1),
      Set[String]("~"),
      Set[(Int, String, Int)](),
      0,
      Set[Int](1)
    )
  }

  def EPSILON(): Nfa[Int] = {
    new Nfa[Int](
      Set[Int](0),
      Set[String]("~"),
      Set[(Int, String, Int)](),
      0,
      Set[Int](0)
    )
  }

  def CHARLIE(c: String): Nfa[Int] = {
    new Nfa[Int](
      Set[Int](0,1),
      Set[String]("~", c),
      Set[(Int, String, Int)]((0, c, 1)),
      0,
      Set[Int](1)
    )
  }

  def MAYBE(a: Nfa[Int]): Nfa[Int] = {
    UNION(EPSILON(), a)
  }

  def PLUS(a: Nfa[Int]): Nfa[Int] = {
    CONCAT(a,STAR(a))
  }
}