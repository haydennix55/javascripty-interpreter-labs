package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * <Your Name>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(true) => 1
      case B(false) => 0
      case Undefined => Double.NaN
      case S(s) =>  {
        if(s == "") 0
        else try s.toDouble catch { case error : Throwable => Double.NaN}
      }


    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => if (n == 0) false else true
      case S(s) => if (s == "") false else true
      case Undefined => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case N(n) => n.toString()
      case B(b) => b.toString()
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */

      case N(n) => N(n)
      case B(b) => B(b)
      case S(s) => S(s)
      case Undefined => Undefined

      /* Inductive Cases */

      case Var(x) => lookup(env,x)

        //Either returns negative number, negative of either 1 or 0 for bools,
        //and converts the string to a number if possible and return the negative
        //or return NaN if not.
      case Unary(Neg,e1) => {
        val v1 = eval(env,e1)
        val n = -toNumber(v1)
        N(n)
      }

        //Returns the opposite binary value, and converts a non-empty string or non-zero
        //number to true and else to false, then returns the opposite.
      case Unary(Not, e1) => {
        val v1 = eval(env,e1)
        val b = !toBoolean(v1)
        B(b)
      }

      case Binary(Plus,e1,e2) => {
        val v1 = eval(env,e1)
        val v2 = eval(env,e2)
        (v1, v2) match {

          //If one is a string, regardless of the other's type, concatenate the two as strings
          case (S(x),_) => S(x + toStr(v2))
          case (_,S(x)) => S(toStr(v1) + x)

          //If there is not a string (meaning both expressions are either numbers or bools)
          //return the sum of both converted to numbers (eg. true + true = 2)
          case _ => N(toNumber(v1) + toNumber(v2))
        }
      }

        //Minus has less cases. Numbers simply get subtracted, bools get converted into either
        //1 or 0 and then subtracted, and strings are converted to a number if they contain
        //only digit values, or NaN otherwise. As all of the above occur in the toNumber()
        //function, that is all that is necessary.
      case Binary(Minus,e1,e2) => {
        val v1 = eval(env,e1)
        val v2 = eval(env,e2)

        N(toNumber(v1) - toNumber(v2))
      }

        //Same explaination as above
      case Binary(Times,e1,e2) => {
        val v1 = eval(env,e1)
        val v2 = eval(env,e2)

        N(toNumber(v1) * toNumber(v2))
      }

        //Div acts the same as the above, with two arithmetic concepts that result in a
        //different output. If both are 0, return not a number. If divinding by 0,
        //return positive infinity.
      case Binary(Div,e1,e2) => {
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)
        if (toNumber(v1) == 0 && toNumber(v2) == 0) N(Double.NaN)
        else N(toNumber(v1) / toNumber(v2))

      }
      case Binary(Eq,e1,e2) => {
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)

        B(v1 == v2)
      }
      case Binary(Ne,e1,e2) => {
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)

        B(v1 != v2)
      }


        //Again, these will probably require more cases. JS compares numbers by numerical
        //order for numbers, 1 and 0 associations to bools for numerical order,
        // alphabetical for strings and string number combinations, and
        //seems to always return false when bools are compared to strings.
      case Binary(Lt,e1,e2) => {
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)
        (v1,v2) match{
          case (S(x),S(y)) =>{
            B(x < y)
          }
          case _ =>{
            B(toNumber(v1) < toNumber(v2))
          }
        }

      }
      case Binary(Le,e1,e2) => {
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)

        (v1,v2) match{
          case (S(x),S(y)) =>{
            B(x <= y)
          }
          case _ =>{
            B(toNumber(v1) <= toNumber(v2))
          }
        }
      }
      case Binary(Gt,e1,e2) => {
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)

        (v1,v2) match{
          case (S(x),S(y)) =>{
            B(x > y)
          }
          case _ =>{
            B(toNumber(v1) > toNumber(v2))
          }
        }
      }
      case Binary(Ge,e1,e2) => {
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)

        (v1,v2) match{
          case (S(x),S(y)) =>{
            B(x >= y)
          }
          case _ =>{
            B(toNumber(v1) >= toNumber(v2))
          }
        }
      }

      case Binary(And,e1,e2) => {
        val v1 = eval(env,e1)
        if (!toBoolean(v1)) v1 else eval(env,e2)
      }
      case Binary(Or,e1,e2) => {
        val v1 = eval(env,e1)
        if (toBoolean(v1)) v1 else eval(env,e2)
      }

      case Binary(Seq,e1,e2) => {
        val v1 = eval(env,e1)
        val v2 = eval(env,e2)
        v2
      }

      case If(e1,e2,e3) => {
        val v1 = eval(env,e1)
        if (toBoolean(v1)) eval(env,e2) else eval(env,e3)
      }

      case ConstDecl(x,e1,e2) => {
        val v1 = eval(env,e1)
        val env2 = extend(env,x,v1)
        val v2 = eval(env2,e2)
        v2
      }

      case Print(e1) => println(pretty(eval(env, e1))); Undefined

    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}
