package jsy.lab6

import jsy.lab6.ast._
import jsy.util.options.SetBool
import jsy.util.{DoWith, JsyApplication}
import jsy.util.DoWith._

import jsy.lab6.JsyParser.SyntaxError
import jsy.lab6.JsyParser.parse

trait Lab6Like { a: JsyApplication =>
  import scala.util.parsing.combinator.Parsers
  import scala.util.parsing.input.CharSequenceReader

  /*** Exercises with Continuations ***/

  sealed abstract class Tree {
    def insert(n: Int): Tree = this match {
      case Empty => Node(Empty, n, Empty)
      case Node(l, d, r) =>
        if (n < d) Node(l insert n, d, r) else Node(l, d, r insert n)
    }

    def pretty: String = {
      def p(acc: String, t: Tree, indent: Int): String = t match {
        case Empty => acc
        case Node(l, d, r) =>
          val spacer = " " * indent
          p("%s%d%n".format(spacer, d) + p(acc, l, indent + 2), r, indent + 2)
      }
      p("", this, 0)
    }
  }
  case object Empty extends Tree
  case class Node(l: Tree, d: Int, r: Tree) extends Tree

  def treeFromList(l: List[Int]): Tree = l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }

  def foldLeftAndThen[A,B](t: Tree)(z: A)(f: (A,Int) => A)(sc: A => B): B
  def dfs[A](t: Tree)(f: Int => Boolean)(sc: List[Int] => A)(fc: () => A): A

  /*** Regular Expression Parsing ***/

  trait REParserLike extends Parsers {
    type Elem = Char

    def re(next: Input): ParseResult[RegExpr]
    def union(next: Input): ParseResult[RegExpr]
    def intersect(next: Input): ParseResult[RegExpr]
    def concat(next: Input): ParseResult[RegExpr]
    def not(next: Input): ParseResult[RegExpr]
    def star(next: Input): ParseResult[RegExpr]
    def atom(next: Input): ParseResult[RegExpr]

    /* External Interface */
    def parse(next: Input): RegExpr = re(next) match {
      case Success(r, next) if (next.atEnd) => r
      case Success(_, next) => throw SyntaxError("remaining input", next.pos)
      case Failure(msg, next) => throw SyntaxError(msg, next.pos)
    }

    def parse(s: String): RegExpr = parse(new CharSequenceReader(s))
  }
  val REParser: REParserLike

  /*** Regular Expression Matching ***/

  def test(re: RegExpr, chars: List[Char])(sc: List[Char] => Boolean): Boolean
  def retest(re: RegExpr, s: String): Boolean

  /*** JavaScripty Interpreter ***/

  /* Environments */
  def empty[A]: Map[String,A] = Map.empty
  def lookup[A](env: Map[String,A], x: String): A = env(x)
  def extend[A](env: Map[String,A], x: String, a: A): Map[String,A] = {
    env + (x -> a)
  }

  /* Type Inference */
  type TEnv = Map[String, MTyp]
  def typeof(env: TEnv, e: Expr): Typ

  /* Step */
  def step(e: Expr): DoWith[Mem,Expr]

  /* Lower */
  def lower(e: Expr): Expr

  /*** External Interfaces ***/

  /** Interface to run your type checker. */
  def inferType(e: Expr): Typ = {
    val t = typeof(empty, e)
    println(s"## type ${e} : ${pretty(t)}")
    t
  }

  /** Interface to run your small-step interpreter
    * and print out the steps of evaluation if debugging. */
  def iterateStep(e: Expr): Expr = {
    //require(closed(e), "input Expr to iterateStep is not closed: free variables: %s".format(freeVars(e)) )
    def loop(e: Expr, n: Int): DoWith[Mem,Expr] =
      if (Some(n) == maxSteps) throw TerminationError(e, n)
      else if (isValue(e)) doreturn( e )
      else {
        for {
          m <- doget[Mem]
          _ = println("## step %4d:%n##  %s%n##  %s".format(n, m, e))
          ep <- step(e)
          epp <- loop(ep, n + 1)
        } yield epp
      }
    val (m,v) = loop(e, 0)(memempty)
    println("## result:%n##  %s%n##  %s".format(m, v))
    v
  }

  // Convenience to pass in a jsy expression as a string.
  def iterateStep(s: String): Expr = iterateStep(lower(parse(s)))

  var useReferenceRegExprParser = false // set to true to use the reference parser

  this.flagOptions = this.flagOptions ++ List(
    ("ref-reparser", SetBool(b => useReferenceRegExprParser = b, Some(b => useReferenceRegExprParser == b)), "using the reference regular expression parser")
  )

  // Select the parser to use based on the useReferenceRegExprParser flag
  lazy val parser: JsyParser =
    if (useReferenceRegExprParser) JsyParser else new JsyParser(REParser.parse)

  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("# ============================================================")
      println("# File: " + file.getName)
      println("# Parsing ...")
    }

    val exprin =
      handle(None: Option[Expr]) {Some{
        parser.parseFile(file)
      }} getOrElse {
        return
      }

    if (debug) {
      println("# ------------------------------------------------------------")
      println("# Lowering %s ...".format(exprin))
    }

    val expr =
      handle(None: Option[Expr]) {Some{
        lower(exprin)
      }} getOrElse {
        return
      }

    if (debug) {
      println("# ------------------------------------------------------------")
      println("# Type checking %s ...".format(expr))
    }

    val welltyped = handle(false) {
      val t = inferType(expr)
      true
    }
    if (!welltyped) return

    if (debug) {
      println("# ------------------------------------------------------------")
      println("# Stepping %s ...".format(expr))
    }

    handle() {
      val v = iterateStep(expr)
      println(pretty(v))
    }
  }

}
