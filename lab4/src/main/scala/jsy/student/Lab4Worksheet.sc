/*
 * CSCI 3155: Lab 4 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab4.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab4.Parser.parse

// Imports the ast nodes
import jsy.lab4.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab4._

// Try compressRec
//val cr1 = compressRec(List(1, 2, 2, 3, 3, 3))

// Parse functions with possibly multiple parameters and type annotations.
parse("function fst(x: number, y: number): number { return x }")
parse("function (x: number) { return x }")
parse("function (f: (y: number) => number, x: number) { return f(x) }")

// Parse objects
parse("{ f: 0, g: true }")
parse("x.f")

def drop(n :Int, list: List[Any]): List[Any] = {
  def loop(index: Int, l: List[Any]): List[Any] = l match {
    case Nil => Nil
    case h::Nil if (index == n) => Nil
    case h::t if (index != n) => h::loop(index+1,t)
    case h::t => t
  }

  loop(0,list)
}

println(drop(3, List(1,2,3,4,5,6)))


/*Higher Order Functions and Callbacks
  - Higher order functions: Function that takes another function as a param
  - Callback: Executable code that is passed to another code
  - Scala includes: **map**, flatMap, **foldLeft**, **foldRight**, forall, exists
*/

//Map example: running map on list(1,2,3) with callback f, map returns list(f(1),f(2),f(3))
//Translates a list of As using callback function into a list of Bs

//Problem 2: I want to negate all negative integers in a list (make them positive)

val ans = List(1,-2,-3,-4,5,6).map{ elem => if (elem < 0) -elem else elem}


//foldLeft

/*
  - Takes z, a start value, of type B. List with types A. f, a binary operator
  - Applies binary operator to a start value and every element in a list going from left --> right

  -List(1,2,3).foldLeft(0)(f) = f( f( f(0,1), 2), 3)
 */

//Problem 3: Sum only the odd integers of a list

def sumOdd(acc: Int, n: Int): Int = {
  if (n % 2 == 1) acc + n else acc
}

val ans1 = List(1,2,3,4,5,6).foldLeft(0)(sumOdd)


def treeFromList(l: List[Int]): Tree =
  l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }

def sum(t: Tree): Int = foldLeft(t)(0){ (acc, d) => acc + d }

val ans2: TEnv = Map(("A",TString),("B",TString),("C",TString))
val ans3 = List(("a",TNumber),("b",TNumber),("c",TNumber))

val ans4 = ans3.foldRight(ans2){
  (h,acc) => acc + (h._1 -> h._2)
}

val v5 = ans2 ++ ans3

//DoInequalityNumber and DoInequalityString
step(Binary(Lt,N(1),N(2)))
step(Binary(Ge,S("a"),S("b")))
step(step(Binary(Gt,Unary(Neg,N(3)),N(1))))

//DoEquality
step(Binary(Eq,S("a"),S("b")))
step(Binary(Eq,N(1),N(2)))
step(Binary(Eq,Binary(Plus,N(1),N(2)),N(3)))
step(step(Binary(Eq,Binary(Plus,N(1),N(2)),N(3))))

step(Binary(Ne,S("a"),S("b")))
step(Binary(Ne,N(1),N(2)))
step(Binary(Ne,Binary(Plus,N(1),N(2)),N(3)))
step(step(Binary(Ne,Binary(Plus,N(1),N(2)),N(3))))

//DoAndTrue
step(Binary(And,B(true),Unary(Neg,N(-1))))

//DoAndFalse
step(Binary(And,B(false),Binary(Plus,N(1),N(2))))

//DoOrTrue
step(Binary(Or,B(true),Unary(Neg,N(-1))))

//DoOrFalse
step(Binary(Or,B(false),Binary(Plus,N(1),N(2))))

//DoIfTrue
step(If(B(true),N(1),N(2)))

//DoIfFalse
step(If(B(false),N(1),N(2)))

//DoDecl
//step(Decl(MConst,"a",N(1),N(2)))

typeof(empty,Unary(Neg,N(1)))

typeof(empty,Function(None,List(),None,Binary(Plus,N(1),N(2))))

