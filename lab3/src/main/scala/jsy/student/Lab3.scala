package jsy.student

import jsy.lab3.Lab3Like
import jsy.util.JsyApplication

object Lab3 extends JsyApplication with Lab3Like {
  import jsy.lab3.ast._

  /*
   * CSCI 3155: Lab 3 
   * Hayden Nix
   * 
   * Partner:
   * Collaborators:
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with your code in each function.
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
   */

  /*
   * The implementations of these helper functions for conversions can come
   * Lab 2. The definitions for the new value type for Function are given.
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(true) => 1
      case B(false) => 0
      case Undefined => Double.NaN
      case S(s) => if (s == "") 0 else try s.toDouble catch { case _: Throwable => Double.NaN }
      case Function(_, _, _) => Double.NaN
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => if (n == 0 || n == Double.NaN) false else true
      case S(s) => if (s == "") false else true
      case Undefined => false
      case Function(_, _, _) => true
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      // Here in toStr(Function(_, _, _)), we will deviate from Node.js that returns the concrete syntax
      // of the function (from the input program).
      case Function(_, _, _) => "function"
      case Undefined => "undefined"
      case N(n) => if (n.isWhole) "%.0f" format n else n.toString
      case B(b) => if (b) "true" else "false"
    }
  }

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1))
    require(isValue(v2))
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {

        //DoInequalityString
      case (S(x),S(y)) => {
        if (bop == Lt) x < y
        else if (bop == Le) x <= y
        else if (bop == Gt) x > y
        else x >= y
      }

        //DoInequalityNumber1 and DoInequalityNumber2
      case (x,y) => {
        if (bop == Lt) toNumber(x) < toNumber(y)
        else if (bop == Le) toNumber(x) <= toNumber(y)
        else if (bop == Gt) toNumber(x) > toNumber(y)
        else toNumber(x) >= toNumber(y)
        }
      }
    }



  /* Big-Step Interpreter with Dynamic Scoping */

  /*
   * Start by copying your code from Lab 2 here.
   */
  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(_) | B(_) | S(_) | Undefined | Function(_, _, _) => e
      case Var(x) => lookup(env,x)

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      case Call(e1,e2)=> {
        val v1 = eval(env,e1)
        v1 match{

          case Function(None,x,e3)=> {
            val v2 = eval(env,e2)
            val env2 = extend(env,x,v2)
            eval(env2,e3)

          }
          case Function(Some(a),x,e3)=> {
            val v2 = eval(env,e2)
            val env2 = extend(env,x,v2)
            val env3 = extend(env2,a,v1)
            eval(env3,e3)
          }
          case _ => throw DynamicTypeError(e)

        }
      }

      case Unary(Neg,e1) => {
        val v1 = eval(env,e1)
        val n = -toNumber(v1)
        N(n)
      }

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

      case Binary(Minus,e1,e2) => {
        val v1 = eval(env,e1)
        val v2 = eval(env,e2)

        N(toNumber(v1) - toNumber(v2))
      }

      case Binary(Times,e1,e2) => {
        val v1 = eval(env,e1)
        val v2 = eval(env,e2)

        N(toNumber(v1) * toNumber(v2))
      }

      case Binary(Div,e1,e2) => {
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)
        if (toNumber(v1) == 0 && toNumber(v2) == 0) N(Double.NaN)
        else if (toNumber(v2) == 0) N(Double.PositiveInfinity)
        else N(toNumber(v1) / toNumber(v2))

      }
      case Binary(Eq,e1,e2) => {
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)
        (v1,v2) match {
          case (Function(_,_,_),_) => throw DynamicTypeError(e)
          case (_,Function(_,_,_)) => throw DynamicTypeError(e)
          case (S(s1), S(s2)) => B(s1 == s2)
          case (x, y) => B(toNumber(x) == toNumber(y))
        }


      }
      case Binary(Ne,e1,e2) => {
        val v1 = eval(env, e1)
        val v2 = eval(env, e2)
        (v1,v2) match {
          case (Function(_, _, _), _) => throw DynamicTypeError(e)
          case (_, Function(_, _, _)) => throw DynamicTypeError(e)
          case (S(s1), S(s2)) => B(s1 != s2)
          case (x, y) => B(toNumber(x) != toNumber(y))
        }
      }

      case Binary(bop @ (Lt|Le|Gt|Ge), e1, e2) => {
        val v1 = eval(env,e1)
        val v2 = eval(env,e2)

        B(inequalityVal(bop,v1,v2))
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
//        val v1 = eval(env,e1)
//        val v2 = eval(env,e2)
//        v2
        eval(env,e1) ; eval(env,e2)
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

    }
  }


  /* Small-Step Interpreter with Static Scoping */

  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = {
      next(e,n) match{
        case None => e
        case Some(x) => loop(x,n+1)
      }
    }
    loop(e0, 0)
  }

  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(substitute(e1, v, x))
      case Unary(uop, e1) => {
        Unary(uop,substitute(e1,v,x))
      }
      case Binary(bop, e1, e2) => {
        Binary(bop,substitute(e1,v,x),substitute(e2,v,x))
      }
      case If(e1, e2, e3) => {
        If(substitute(e1,v,x),substitute(e2,v,x),substitute(e3,v,x))
      }
      case Call(e1, e2) => {
        Call(substitute(e1, v, x), substitute(e2, v, x))
      }
      case Var(y) => {
        if (y == x) v else Var(y)
      }
      case Function(None, y, e1) => {
        if (y == x) e
        else Function(None,y,substitute(e1,v,x))
      }
      case Function(Some(y1), y2, e1) => {
        if (y1 == x || y2 == x) e
        else Function(Some(y1),y2,substitute(e1,v,x))
      }
      case ConstDecl(y, e1, e2) => {
        if (x == y) ConstDecl(y, substitute(e1,v,x), e2)
        else ConstDecl(y, substitute(e1,v,x), substitute(e2,v,x))
      }
    }
  }

  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined

      //DoNeg and DoNot
      case Unary(Neg,v) if (isValue(v)) => N(-toNumber(v))
      case Unary(Not,v) if (isValue(v)) => B(!toBoolean(v))

      //DoSeq
      case Binary(Seq,v1,v2) if (isValue(v1)) => v2

      case Binary(Plus,v1,v2) if (isValue(v1) && isValue(v2)) =>
        (v1, v2) match {
          case (S(x),_) => S(x + toStr(v2)) //DoPlusString1
          case (_,S(x)) => S(toStr(v1) + x) //DoPlusString2
          case _ => N(toNumber(v1) + toNumber(v2)) //DoPlusNumber
        }

      //DoArith
      case Binary(Minus,v1,v2) if (isValue(v1) && isValue(v2)) => N(toNumber(v1) - toNumber(v2))
      case Binary(Times,v1,v2) if (isValue(v1) && isValue(v2)) => N(toNumber(v1) * toNumber(v2))
      case Binary(Div,v1,v2) if (isValue(v1) && isValue(v2)) => N(toNumber(v1)/toNumber(v2))


      case Binary(bop @ (Gt | Ge | Lt | Le),v1,v2) if (isValue(v1) && isValue(v2)) => B(inequalityVal(bop,v1,v2))

      //DoEquality
      case Binary(Eq,v1,v2) if (isValue(v1) && isValue(v2)) =>
        (v1, v2) match {
          case (Function(_,_,_),_) => throw DynamicTypeError(e)
          case (_,Function(_,_,_)) => throw DynamicTypeError(e)
          case (S(s1), S(s2)) => B(s1 == s2)
          case (x, y) => B(toNumber(x) == toNumber(y))
        }
      case Binary(Ne,v1,v2) if (isValue(v1) && isValue(v2)) =>
        (v1, v2) match {
          case (Function(_,_,_),_) => throw DynamicTypeError(e)
          case (_,Function(_,_,_)) => throw DynamicTypeError(e)
          case (S(s1), S(s2)) => B(s1 != s2)
          case (x, y) => B(toNumber(x) != toNumber(y))
        }

      //DoAndTrue and DoAndFalse
      case Binary(And,v1,v2) if (isValue(v1)) => {
        if (!toBoolean(v1)) v1 else v2
      }
      //DoOrTrue and DoOrFalse
      case Binary(Or,v1,v2) if isValue(v1) => {
        if (toBoolean(v1)) v1 else v2
      }

      //DoIfTrue and DoIfFalse
      case If(v1,v2,v3) if (isValue(v1))=> {
        if (toBoolean(v1)) v2 else v3
      }

      //DoConst
      case ConstDecl(x,v1,v2) if (isValue(v1)) => substitute(v2,v1,x)

      //DoCall and DoCallRec
      case Call(v1,v2) if (isValue(v1) && isValue(v2)) => {
        v1 match {
          case Function(None,x,v3) => substitute(v3,v2,x)
          case Function(Some(x1),x2,v3) => substitute(substitute(v3,v1,x1),v2,x2)
          case _ => throw DynamicTypeError(e)
        }
      }

      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))

      case Unary(uop,v) => Unary(uop,step(v))
      case Binary(bop @ (Eq|Ne),Function(_,_,_),v2) => throw DynamicTypeError(e)
      case Binary(bop @ (Eq|Ne),v1,Function(_,_,_)) => throw DynamicTypeError(e)
      case Binary(bop,v1,v2) if (isValue(v1)) => Binary(bop,v1,step(v2))
      case Binary(bop,v1,v2) => Binary(bop,step(v1),v2)
      case If(v1,v2,v3) => If(step(v1),v2,v3)
      case ConstDecl(x,v1,v2) => ConstDecl(x,step(v1),v2)

      case Call(v1,v2) if (isValue(v1))=> {
        v1 match {
          case Function(_,_,_) => Call(v1,step(v2))
          case _ => throw DynamicTypeError(e)
        }
      }
      case Call(v1,v2) => Call(step(v1),v2)

      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }


  /* External Interfaces */

  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

}