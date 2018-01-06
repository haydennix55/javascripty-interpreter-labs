package jsy.student

import jsy.lab6.ast._
import jsy.lab6.{Lab6Like, RegExprParser}
import jsy.tester.JavascriptyTester
import org.scalatest._
import org.scalatest.prop.PropertyChecks

import scala.util.matching.Regex

object Lab6Harness {

  def regExprToRegex(re: RegExpr): Regex = {
    def toR(re: RegExpr): String = re match {
      case REmptyString => ""
      case RSingle(c) => c.toString
      case RConcat(re1, re2) => toR(re1) + toR(re2)
      case RUnion(re1, re2) => s"(${toR(re1)}|${toR(re2)})"
      case RStar (re1) => s"(${toR(re1)})*"
      case RAnyChar => "."
      case RPlus(re1) => s"(${toR(re1)})+"
      case ROption(re1) => s"(${toR(re1)})?"
      case _ => throw new IllegalArgumentException(s"Unsupported RegExpr ${re}")
    }
    new Regex(raw"\A" + toR(re) + raw"\z")
  }

  def retestRegex(re: Regex, str: String): Boolean =
    !re.findPrefixOf(str).isEmpty

}

class Lab6FoldLeftTests(lab6: Lab6Like) extends FlatSpec with PropertyChecks with Matchers {
  behavior of "foldLeftAndThen"

  it should "perform left-to-right, in-order traversal to reverse a list" in {
    forAll { (l: List[Int]) =>
      val t = lab6.treeFromList(l)
      val folded = lab6.foldLeftAndThen(t)(Nil: List[Int]) { (acc, h) => h :: acc } { acc => acc }
      folded shouldBe l.sorted.reverse
    }
  }

  it should "perform left-to-right, in-order traversal to compute multiplication of elements" in {
    forAll { (l: List[Int]) =>
      val t = lab6.treeFromList(l)
      val op = { (acc: Int, h: Int) => acc * h }
      val sc = { acc: Int => acc * 2 }
      val folded = lab6.foldLeftAndThen(t)(1)(op)(sc)
      val golden = sc(l.foldLeft(1)(op))
      folded shouldBe golden
    }
  }
}

class Lab6Spec(lab6: Lab6Like) extends FlatSpec {
  import Lab6Harness._
  import lab6._

  /*** Warm-Up ***/

  {
    val l1 = List(1, 2, 3, 4, 5, 6)
    val t1 = treeFromList(l1)
    val l1rev = l1.reverse
    val l1revdup = l1rev map { i => i * 2 }

    "foldLeftAndThen" should s"perform left-to-right, in-order traversal and yield ${l1rev}" in {
      assertResult(l1rev) {
        foldLeftAndThen(t1)(Nil: List[Int]) { (acc, h) => h :: acc } { acc => acc }
      }
    }

    it should s"then call the success continuation and yield ${l1revdup}" in {
      assertResult(l1revdup) {
        foldLeftAndThen(t1)(Nil: List[Int]) { (acc, h) => h :: acc } { acc => acc map { i => i * 2 } }
      }
    }

    val l1path = List(3, 2, 1)
    "dfs" should s"perform depth first search and yield ${l1path}" in {
      assertResult(l1path) {
        dfs(t1) { i => i == 3 } { path => path } { () => Nil }
      }
    }
  }


  /*** Regular Expressions ***/

  val strings = List(
     "",
     "a",
     "aa",
     "ab",
     "aaa"
  )

  val respecs = List(
    "a",
    "b",
    ".",
    "aa",
    "(aa)*"
  )
  
  val respecsast = respecs map { s => RegExprParser.parse(s) }


  behavior of "parse"

  for ((restr,re) <- (respecs,respecsast).zipped) {
    it should s"on '${restr}' produce a RegExpr AST matching the reference" in {
      assertResult(re) { REParser.parse(restr)}
    }
  }


  behavior of "retest"

  // Note that this testing uses Scala's regular expression matcher, which does not
  // support !, &, or ~.
  for (re <- respecsast) {
    for (s <- strings) {
      it should s"test '${s}' on '${pretty(re)}'" in {
        val regex = regExprToRegex(re)
        assertResult(retestRegex(regex, s)) { retest(re, s) }
      }
    }
  }
  
}

// An adapter class to pass in your lab object.
class Lab6SpecRunner extends Lab6Spec(jsy.student.Lab6)
class Lab6FoldLeftTestsRunner extends Lab6FoldLeftTests(jsy.student.Lab6)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab6.
// The test expects a corresponding .ans file with the expected result.
//class Lab6JsyTests extends JavascriptyTester(None, "lab6", Lab6)

class Lab6Suite extends Suites(
  // uncomment this line (and above) if you create .jsy tests
  //new Lab6JsyTests,
  new Lab6SpecRunner,
  new Lab6FoldLeftTestsRunner
)