/*
 * CSCI 3155: Lab 6 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab6.scala.
 */

// Imports the ast nodes
import jsy.lab6.ast._
import jsy.lab6._

// Imports all of the functions from your implementation
import jsy.student.Lab6._

// Experiment with the warm-up exercises
val l1 = List(1, 2, 3, 4, 5, 6)
val t1 = treeFromList(l1)
val r1 = foldLeftAndThen(t1)(Nil: List[Int]) { (acc, h) => h :: acc } { acc => acc }
val p1 = dfs(t1) { i => i == 3 } { path => path } { () => Nil }
val l2 = List(3, 4, 2, 1, 6, 5)
val t2 = treeFromList(l2)
val r2 = foldLeftAndThen(t2)(Nil: List[Int]) { (acc, h) => h :: acc } { acc => acc }
val p2 = dfs(t2) { i => i == 3 } { path => path } { () => Nil }
val p2b = dfs(t2) { i => i == 10 } { path => path } { () => Nil }
val p2c = dfs[Option[List[Int]]](t2) { i => i == 10 } { path => Some(path) } { () => None }

// Jsy parser using the your RegExpr parser.
val yourparser = new JsyParser(REParser.parse)

// Try out the reference RegExpr parser
val re1: RegExpr = RegExprParser.parse("a")
val e1: Expr = JsyParser.parse("/^a$/")

// Try out your RegExpr parser
//REParser.parse("a")
//yourparser.parse("/^a$/")

// Try out your matcher
//retest(re1, "a")