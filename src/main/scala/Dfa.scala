import Nfa.Nfa2Dfa

import scala.annotation.tailrec

class Dfa[A] (val K: Set[A], val Sigma: Set[String], val Delta: Set[(A, String, A)], val q0: A, val F: Set[A], val SINK: A){

  def map[B](f: A => B) : Dfa[B] = {
    new Dfa[B](
      K.map(f),
      Sigma,
      Delta.map(x => (f(x._1), x._2, f(x._3))),
      f(q0),
      F.map(f),
      f(SINK)
    )
  }

  def next(state:A, c: Char): A = {
    if (Delta.isEmpty)
      return SINK
    @tailrec
    def Aux(DeltaHere: Set[(A, String, A)]): A = {
      if (DeltaHere.isEmpty)
        SINK
      else {
        if (DeltaHere.head._1 == state && DeltaHere.head._2.head == c)
          DeltaHere.head._3
        else
          Aux(DeltaHere.tail)
      }
    }
    Aux(Delta)
  }

  def accepts(str: String): Boolean = {
    @tailrec
    def aux(stare: A, sir: String): Boolean = {
      if (sir.isEmpty)
        isFinal(stare)
      else {
        if (Delta.isEmpty)
          false
        else
          aux(next(stare, sir.head), sir.tail)
      }
    }
    aux(q0, str)
  }

  def getStates: Set[A] = K

  def isFinal(state: A): Boolean = F.contains(state)
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String): Dfa[Int] = Nfa2Dfa(Nfa.fromPrenex(str))

}