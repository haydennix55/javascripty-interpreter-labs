package jsy.lab3

import jsy.lab3.ast._
import jsy.lab3.ast.isValue
import jsy.lab3.Parser.parse
import jsy.util.JsyApplication

trait Lab3Like { a: JsyApplication =>

  type Env = Map[String, Expr]
  val empty: Env = Map()
  def lookup(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }

  def toNumber(v: Expr): Double
  def toBoolean(v: Expr): Boolean
  def toStr(v: Expr): String

  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean

  def eval(env: Env, e: Expr): Expr

  def iterate(e0: Expr)(body: (Expr, Int) => Option[Expr]): Expr
  def substitute(e: Expr, v: Expr, x: String): Expr
  def step(e: Expr): Expr

  /** Interface to run your big-step interpreter starting from an empty environment
    * and print out the test input if debugging. */
  def evaluate(e: Expr): Expr = {
    require(closed(e))
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with eval ...")
      println("\nExpression:\n  " + e)
    }
    val v = eval(empty, e)
    if (debug) {
      println("Value: " + v)
    }
    v
  }

  /** Interface to run your big-step interpreter from a string. This is convenient for unit testing. */
  def eval(s: String): Expr = evaluate(parse(s))

  /** Interface to run your small-step interpreter
    * and print out the steps of evaluation if debugging. */
  def iterateStep(e: Expr): Expr = {
    require(closed(e))
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with step ...")
    }
    val v = iterate(e) { (e: Expr, n: Int) =>
      if (debug) { println(s"Step $n: $e") }
      if (isValue(e)) None else Some(step(e))
    }
    if (debug) { println("Value: " + v) }
    v
  }

  /** Interface to take a small-step from a string. This is convenient for unit testing. */
  def oneStep(s: String): Expr = step(parse(s))

  /** Interface to run your small-step interpreter from a string. This is convenient for unit testing. */
  def iterateStep(s: String): Expr = iterateStep(parse(s))

  /** Interface for main for JsyApplication */
  def processFile(file: java.io.File) {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }

    val e1 =
      handle(None: Option[Expr]) {
        Some {
          Parser.parseFile(file)
        }
      } getOrElse {
        return
      }

    handle() {
      println("# Evaluating ...")
      val v1 = evaluate(e1)
      println(pretty(v1))
    }

    handle() {
      println("# Stepping ...")
      def loop(e: Expr, n: Int): Expr = {
        println("## %4d: %s".format(n, e))
        if (isValue(e)) e else loop(step(e), n + 1)
      }
      val v1 = loop(e1, 0)
      println(pretty(v1))
    }
  }

}
