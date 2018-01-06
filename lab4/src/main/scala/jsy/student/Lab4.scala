package jsy.student

import jsy.lab4.Lab4Like

object Lab4 extends jsy.util.JsyApplication with Lab4Like {
  import jsy.lab4.ast._
  import jsy.lab4.Parser
  
  /*
   * CSCI 3155: Lab 4
   * Hayden Nix
   *
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
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
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */
  
  /* Collections and Higher-Order Functions */
  
  /* Lists */
  
  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => l
    case h1 :: (t1 @ (h2 :: _)) => {
      if (h1 == h2)  compressRec(t1) else h1::compressRec(t1)
    }
  }
  
  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]){
    (h, acc) => acc match {
      case Nil => h::acc
      case g::_ => if (h == g) acc else h::acc
    }
  }
  
  def mapFirst[A](l: List[A])(f: A => Option[A]): List[A] = l match {
    case Nil => l
    case h :: t => f(h) match {
      case None => h::mapFirst(t)(f)
      case Some(g) => g::t
    }
  }
  
  /* Trees */

  def foldLeft[A](t: Tree)(z: A)(f: (A, Int) => A): A = {
    def loop(acc: A, t: Tree): A = t match {
      case Empty => acc
      case Node(l, d, r) => {
        val aLeft = loop(acc,l)
        val aData = f(aLeft, d)
        val aRight = loop(aData,r)
        aRight
      }
    }
    loop(z, t)
  }

  // An example use of foldLeft
  def sum(t: Tree): Int = foldLeft(t)(0){ (acc, d) => acc + d }

  // Create a tree from a list. An example use of the
  // List.foldLeft method.
  def treeFromList(l: List[Int]): Tree =
    l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }

  def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = foldLeft(t)((true, None: Option[Int])){
      (acc,d) => acc match {
        case (bVal,None) => (bVal,Some(d))
        case (bVal,Some(prev)) => (prev < d && bVal, Some(d))
      }
    }
    b
  }

  /* Type Inference */

  // While this helper function is completely given, this function is
  // worth studying to see how library methods are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }
  
  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => env(x)
      case Decl(m, x, e1, e2) => typeof(env + (x -> typeof(env, e1)),e2)
      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case other => err(other, e1)
      } //************************************************
      case Unary(Not, e1) => typeof(env,e1) match {
        case TBool => TBool
        case other => err(other,e1)
      }
      case Binary(Plus, e1, e2) => {
        typeof(env,e1) match {
          case TNumber => if (typeof(env,e2) == TNumber) TNumber else err(typeof(env,e2),e2)
          case TString => if (typeof(env,e2) == TString) TString else err(typeof(env,e2),e2)
          case other=> err(other,e1)
        }
      }
      case Binary(Minus|Times|Div, e1, e2) => 
        typeof(env,e1) match {
          case TNumber => if (typeof(env,e2) == TNumber) TNumber else err(typeof(env,e2),e2)
          case other => err(other,e2)
        }
      case Binary(Eq|Ne, e1, e2) => {//They cant be function types, but do they have to be the same?
        val t1 = typeof(env,e1)
        if (!hasFunctionTyp(t1)) {
          val t2 = typeof(env,e2)
          if (!hasFunctionTyp(t2)){
            TBool
          } else {
            err(t2,e2)
          }
        } else {
          err(t1,e1)
        }
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) =>
        typeof(env,e1) match {
          case TNumber => if (typeof(env,e2) == TNumber) TBool else err(typeof(env,e2),e2)
          case TString => if (typeof(env,e2) == TString) TBool else err(typeof(env,e2),e2)
          case other => err(other,e1)
        }
      case Binary(And|Or, e1, e2) =>
        typeof(env,e1) match {
          case TBool => if (typeof(env,e2) == TBool) TBool else err(typeof(env,e2),e2)
          case other => err(other,e1)
        }
      case Binary(Seq, e1, e2) => typeof(env,e2)
      case If(e1, e2, e3) =>
        typeof(env,e1) match {
          case TBool => if (typeof(env,e2) == typeof(env,e3)) typeof(env,e2) else err(typeof(env,e2),e2)
          case _ => err(typeof(env,e1),e1)
        }
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1: TEnv = (p, tann) match {
          /***** Add cases here *****/
          case (Some(name),Some(t)) => {
            val t2 = TFunction(params,t)
            env + (name -> t2)
          }
          case (None,_) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.

        val env2 = params.foldRight(env1) {
          case ((s, MTyp(_, t)), acc) => acc + (s -> t)
        }

        // Infer the type of the function body
        val t1 = TFunction(params,typeof(env2, e1))
        // Check with the possibly annotated return type
        tann match {
          case None => t1
          case Some(expectedType) => if (expectedType == t1) t1 else err(t1,e1)
        }
      }
      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, t) if (params.length == args.length) =>
          (params zip args).foreach {
            (tuple) => {
              val (paramN,argumentN) = tuple
              val argType = typeof(env,argumentN)
              if (paramN._2 != argType) err(argType,e) else argType
            }
          };
          t
        case tgot => err(tgot, e1)
      }
      case Obj(fields) => {
        val fieldsTypeMaps = fields.foldLeft(Map(): Map[String,Typ]) {
          (acc, objectField) => acc + (objectField._1 -> typeof(env,objectField._2))
        }
        TObj(fieldsTypeMaps)
      }
      case GetField(e1, f) => typeof(env,e1) match {
          case TObj(fields) => fields.get(f) match {
            case None => err(typeof(env,e1), e1)
            case Some(v) => v
          }
          case _ => err(typeof(env,e1), e1)
        }
      }
    }
  
  
  /* Small-Step Interpreter */

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3.
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
      case (N(x),N(y)) => {
        if (bop == Lt) (x < y)
        else if (bop == Le) (x <= y)
        else if (bop == Gt) x > y
        else x >= y
      }
    }
  }

  /* This should be the same code as from Lab 3 */
  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = {
      next(e,n) match{
        case None => e
        case Some(x) => loop(x,n+1)
      }
    }
    loop(e0, 0)
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    //def subst(e: Expr): Expr =
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(substitute(e1, esub, x))
        /***** Cases from Lab 3 */
      case Unary(uop, e1) => {
        Unary(uop,substitute(e1,esub,x))
      }
      case Binary(bop, e1, e2) => {
        Binary(bop,substitute(e1,esub,x),substitute(e2,esub,x))
      }
      case If(e1, e2, e3) => {
        If(substitute(e1,esub,x),substitute(e2,esub,x),substitute(e3,esub,x))
      }
      case Var(y) => {
        if (y == x) esub else Var(y)
      }
      case Decl(mode, y, e1, e2) => {
        if (x == y) Decl(mode,y, substitute(e1,esub,x), e2)
        else Decl(mode,y, substitute(e1,esub,x), substitute(e2,esub,x))
      }

        /***** Cases needing adapting from Lab 3 */
      case Function(p, params, tann, e1) => p match {
        case Some(f) if f == x =>  e
        case _ =>
          params.foreach {
            case (param, t) => if (x == param) return e
          }
          Function(p, params, tann, substitute(e1,esub,x))
      }
      case Call(e1, args) => Call(substitute(e1,esub,x),args.map((arg) => substitute(arg,esub,x)))
        /***** New cases for Lab 4 */
      case Obj(fields) => Obj(fields.mapValues((v) => substitute(v,esub,x)))
      case GetField(e1, f) => GetField(substitute(e1,esub,x),f)
    }

    //val fvs = freeVars(???)
    //def fresh(x: String): String = if (???) fresh(x + "$") else x
    //subst(e)
  }

  /* Rename bound variables in e */
  def rename(e: Expr)(fresh: String => String): Expr = {
    def ren(env: Map[String,String], e: Expr): Expr = {
      e match {
        case N(_) | B(_) | Undefined | S(_) => e
        case Print(e1) => Print(ren(env, e1))

        case Unary(uop, e1) => ???
        case Binary(bop, e1, e2) => ???
        case If(e1, e2, e3) => ???

        case Var(y) =>
          ???
        case Decl(mode, y, e1, e2) =>
          val yp = fresh(y)
          ???

        case Function(p, params, retty, e1) => {
          val (pp, envp): (Option[String], Map[String,String]) = p match {
            case None => ???
            case Some(x) => ???
          }
          val (paramsp, envpp) = params.foldRight( (Nil: List[(String,MTyp)], envp) ) {
            ???
          }
          ???
        }

        case Call(e1, args) => ???

        case Obj(fields) => ???
        case GetField(e1, f) => ???
      }
    }
    ren(empty, e)
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst => if (isValue(e)) false else true
    case MName => false
  }

  def step(e: Expr): Expr = {
    require(!isValue(e), s"step: e ${e} to step is a value")
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
        /***** Cases needing adapting from Lab 3. */
      case Unary(Neg,N(v)) => N(-v)
      case Unary(Not,B(v))  => B(!v)
      case Binary(Seq,v1,exp) if (isValue(v1)) => exp
      case Binary(bop @ (Plus | Minus | Times | Div),N(x),N(y)) =>
        bop match {
          case Plus => N(x + y)
          case Minus => N(x - y)
          case Times => N(x * y)
          case Div => N(x / y)
        }
      case Binary(Plus,S(x),S(y))  => S(x + y)
      case Binary(bop @ (Gt | Ge | Lt | Le),v1,v2) if (isValue(v1) && isValue(v2)) =>
        (v1,v2) match {
          case (S(x),S(y)) => B(inequalityVal(bop,v1,v2))
          case (N(x),N(y)) => B(inequalityVal(bop,v1,v2))
        }
      case Binary(Eq,v1,v2) if (isValue(v1) && isValue(v2)) => B(v1 == v2)
      case Binary(Ne,v1,v2) if (isValue(v1) && isValue(v2)) => B(v1 != v2)

      case Binary(And,B(true),e2) => e2
      case Binary(And,B(false),e2) => B(false)
      case Binary(Or, B(true),e2) => B(true)
      case Binary(Or,B(false),e2) => e2

      case If(B(true),e2,e3) => e2
      case If(B(false),e2,e3) => e3

      case Decl(m,x,e1,e2) if (!isRedex(m,e1)) => substitute(e2,e1,x)

        /***** More cases here */
//
      case Call(v1, args) if isValue(v1) =>
        v1 match {
          case Function(p, params, _, e1) => { //DoCall and DoCallRec
            val pazip = params zip args
            if (!pazip.forall( (x) =>
            x match {
              case (((xi,mi),ei)) => isRedex(mi.m, ei) //implement redex
            }))
            {
              val e1p = pazip.foldRight(e1) {
                case (((xi,mi),ei),e1) => substitute(e1,ei,xi)
              }
              p match {
                case None => e1p
                case Some(funName) => substitute(e1p,v1,funName)
              }
            }
            else { //searchCall2
              val pazip2 = mapFirst(pazip) {
                case (x) => x match {
                  case ((xi,mi),ei) =>
                    if (isRedex(mi.m,ei)) { Some((xi,mi),step(ei))
                    } else { None }
                }
              }
              val (_,args2) = pazip2.unzip
              Call(v1,args2)
            }
          }
          case _ => throw StuckError(e)
        }
      case GetField(Obj(fields),f) if (fields.forall { case (_,vi) => isValue(vi)}) => {
        fields.get(f) match {
          case Some(v) => v
          case None => throw StuckError(e)
        }

      }
      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      /***** Cases from Lab 3. */
      case Unary(uop,e) => Unary(uop,step(e))
      case Binary(bop,v1,e2) if (isValue(v1)) => Binary(bop,v1,step(e2))
      case Binary(bop,e1,e2) => Binary(bop,step(e1),e2)
      case If(e1,e2,e3) => If(step(e1),e2,e3)
      case Decl(m,x,e1,e2) if (isRedex(m,e1))  => Decl(m,x,step(e1),e2)


      /***** More cases here */
      /***** Cases needing adapting from Lab 3 */
      //case Call(v1 @ Function(_, _, _, _), args) => ???
      case Call(e1, args) => Call(step(e1),args)
      /***** New cases for Lab 4. */

      case Obj(fields) => {
//        val fields2 = fields
//        val newFields = fields.foldLeft(fields2) {
//          case (acc,(xi,ei)) => acc + (xi -> step(ei))
//        }
//        Obj(newFields)
        val l = fields.toList
        val l2 = mapFirst(l)({
          case x => x match {
            case (s, e1) => if (!isValue(e1)) Some(s,step(e1)) else None
          }
        })
        val newFields = l2.toMap
        Obj(newFields)

      }
      case GetField(e1,f) => GetField(step(e1),f)

      /* Everything else is a stuck error. Should not happen if e is well-typed.
       *
       * Tip: you might want to first develop by comment out the following line to see which
       * cases you have missing. You then uncomment this line when you are sure all the cases
       * that you have left the ones that should be stuck.
       */
      //case _ => throw StuckError(e)
    }
  }
  
  
  /* External Interfaces */
  def iterate(e: Expr): Expr =
    if (isValue(e)) e else iterateStep(step(e))
  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}

