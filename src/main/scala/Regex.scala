object Regex {
  def simplificare(str: String): List[Char] = {
    val alphabetMic = "abcdefghijklmnopqrstuvwxyz{"
    val alphabetMare = "ABCDEFGHIJKLMNOPQRSTUVWXYZ["
    val digits = "0123456789:"
    def aux(here: List[Char]): List[Char] = {
      def prelucrare(prima: Char, ultima: Char, alpha: String, ret: List[Char]): List[Char] = {
        if(prima > alpha.head)
          prelucrare(prima, ultima, alpha.tail, ret)
        else if (prima == alpha.head)
          prelucrare(prima, ultima, alpha.tail, '('::alpha.head::ret)
        else
          if (alpha.nonEmpty && ultima >= alpha.head)
            prelucrare(prima, ultima, alpha.tail, (alpha.head::'|'::ret.reverse).reverse)
          else
            (')'::ret.reverse).reverse
      }
      if (here.isEmpty)
        List[Char]()
      else if (here.head == '[') {
        if (here.drop(3).head < 'A')
          prelucrare(here.tail.head, here.drop(3).head, digits, List[Char]()) ++ aux(here.drop(5))
        else if (here.drop(3).head < 'a')
          prelucrare(here.tail.head, here.drop(3).head, alphabetMare, List[Char]()) ++ aux(here.drop(5))
        else
          prelucrare(here.tail.head, here.drop(3).head, alphabetMic, List[Char]()) ++ aux(here.drop(5))

      }
      else {
        here.head :: aux(here.tail)
      }
    }
    aux(str.toList)
  }
  def adaugaPunct(str: List[Char]): List[Char] = {
    def aux(here: List[Char]): List[Char] = {
      if (here.isEmpty)
        List[Char]()
      else {
        if (here.head == '\'') {
          if (here.tail.head != '\'') {
            if (here.drop(3).nonEmpty && here.drop(3).head != '|' && here.drop(3).head != ')')
              here.tail.head :: '.' :: aux(here.drop(3))
            else
              here.tail.head :: aux(here.drop(3))
          } else
            here.head :: '.' :: aux(here.tail)
        }
        else {
          if (here.head.isLetter || here.head.isDigit || here.head == ')' || here.head == '*' || here.head == '?' || here.head == '+')
            if (here.tail.isEmpty || (!here.tail.head.isLetter && !here.tail.head.isDigit && here.tail.head != '(' && here.tail.head != '\''))
              here.head :: aux(here.tail)
            else
              here.head :: '.' :: aux(here.tail)
          else
            here.head :: aux(here.tail)
        }
      }
    }
    aux(str)
  }
  def inNotatiaNFA(input: List[Char]): String = {
    def aux(here: List[Char]): List[Char] = {
      if (here.isEmpty)
        return "".toList
      if (here.head == '.')
        "CONCAT ".toList ++ aux(here.tail)
      else if (here.head == '|')
        "UNION ".toList ++ aux(here.tail)
      else if (here.head == '*')
        "STAR ".toList ++ aux(here.tail)
      else if (here.head == '+')
        "PLUS ".toList ++ aux(here.tail)
      else if (here.head == '?')
        "MAYBE ".toList ++ aux(here.tail)
      else if (here.head == '\'') {
        if (here.tail.head == ' ')
          "' ' ".toList ++ aux(here.tail)
        else
          here.head :: ' ' :: aux(here.tail)
      } else {
        if (here.head != ' ')
          here.head :: ' ' :: aux(here.tail)
        else
          '\'' :: ' ' :: '\'' :: ' ' :: aux(here.tail)
      }
    }
    aux(input).mkString("")
  }
  def Algoritm(input: List[Char]): List[Char] = {
    val operationsSingle = "?+*"
    val operationsDouble = ".|"
    def aux(here: List[Char], op: List[Char], result:List[List[Char]]): List[Char] = {
      if (here.isEmpty) {
        if (op.isEmpty)
          result.head
        else
          aux(here, op.tail, List(op.head :: result.tail.head ++ result.head) ++ result.tail.tail)
      }
      else {
        if (here.head == '(')
          aux(here.tail, '(' :: op, result)
        else if (operationsSingle.contains(here.head))
          aux(here.tail, op, List(here.head :: result.head) ++ result.tail)
        else if (operationsDouble.contains(here.head)) {
          if (op.isEmpty || !operationsDouble.contains(op.head))
            aux(here.tail, here.head :: op, result)
          else {
            if (op.head == '.')
              aux(here, op.tail, List(op.head :: result.tail.head ++ result.head) ++ result.tail.tail)
            else {
              if (here.head == '.')
                aux(here.tail, here.head::op, result)
              else
                aux(here, op.tail, List(op.head :: result.tail.head ++ result.head) ++ result.tail.tail)
            }
          }
        } else if (here.head == ')')
          if (op.head == '(')
            aux(here.tail, op.tail, result)
          else
            aux(here, op.tail, List(op.head :: result.tail.head ++ result.head) ++ result.tail.tail)
        else
          aux(here.tail, op, List(here.head) :: result)
      }
    }
    aux(input, List(), List())
  }

  def preprocess(str: String): String = {
    if (str == "eps")
      "eps"
    else {
      val simplificat: List[Char] = simplificare(str)
      val cuPunct: List[Char] = adaugaPunct(simplificat)
      val algoritm: List[Char] = Algoritm(cuPunct)
      inNotatiaNFA(algoritm)
    }
  }

  def toPrenex(str: String): String = preprocess(str)
}

//object Main {
//  def main(args: Array[String]): Unit ={
//   println(Regex.preprocess("A|BC*"))
//  }
//}
