package jsy.student

import jsy.lab5.Lab5Like

object Lab5 extends jsy.util.JsyApplication with Lab5Like {
  import jsy.lab5.ast._
  import jsy.util.DoWith
  import jsy.util.DoWith._

  /*
   * CSCI 3155: Lab 5
   * <Hayden Nix>
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

  /*** Exercise with DoWith ***/

  def rename[W](env: Map[String,String], e: Expr)(fresh: String => DoWith[W,String]): DoWith[W,Expr] = {
    def ren(env: Map[String,String], e: Expr): DoWith[W,Expr] = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => doreturn(e)
      case Print(e1) => ren(env,e1) map { e1p => Print(e1p) }

      case Unary(uop, e1) => ren(env,e1) map {e1p => Unary(uop, e1p)}
      case Binary(bop, e1, e2) => ren(env,e1) flatMap {
        e1p => ren(env,e2) map {
          e2p => Binary(bop,e1p,e2p)
        }
      }
      case If(e1, e2, e3) => ren(env,e1) flatMap {
        e1p => ren(env,e2) flatMap {
          e2p => ren(env,e3) map {
            e3p => If(e1p,e2p,e3p)
          }
        }
      }

      case Var(y) => doreturn(Var(env.getOrElse(y,y)))

      case Decl(m, x, e1, e2) => fresh(x) flatMap { xp =>
        val env1 = env + (x -> xp)
        ren(env,e1) flatMap {
          e1p => ren(env1,e2) map {
            e2p => Decl(m,xp,e1p,e2p)
          }
        }
      }

      case Function(p, params, retty, e1) => {
        val w: DoWith[W,(Option[String], Map[String,String])] = p match {
          case None => doreturn((p,env))
          case Some(x) => fresh(x) flatMap {
            xp => doreturn(Some(xp),env + (x -> xp))
          }
        }
        w flatMap { case (pp, envp) =>
          params.foldRight[DoWith[W,(List[(String,MTyp)],Map[String,String])]]( doreturn((Nil: List[(String, MTyp)], envp))) {
            case ((x,mty), acc) => fresh(x) flatMap {
              xp => acc flatMap {
                case (newParamsList,envacc) => {
                  doreturn(((xp,mty) :: newParamsList,envacc + (x -> xp)))
                }
              }
            }
          } flatMap {
            case (newParams,newEnv) => {
              ren(newEnv,e1) map {
                e1p => Function(pp,newParams,retty,e1p)
              }
            }
          }
        }
      }

      case Call(e1, args) => ren(env,e1) map {
          e1p => {
            val newArgs = args.foldRight(Nil: List[Expr]) {
              case (en,acc) => {
                ren(env,en) map {
                  (enp) => acc :+ enp
                }
                acc
              }
            }
            Call(e1p,newArgs)
          }
      }
      case Obj(fields) => ???
      case GetField(e1, f) => ren(env,e1) map {
        e1p => GetField(e1p,f)
      }

      case Assign(e1,e2) => ren(env,e1) flatMap{
        e1p => ren(env,e2) map {
          e2p => Assign(e1p,e2p)
        }
      }

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
    ren(env, e)
  }

  def myuniquify(e: Expr): Expr = {
    val fresh: String => DoWith[Int,String] = { (x: String) =>
      ???
    }
    val (_, r) = rename(empty, e)(fresh)(???)
    r
  }

  /*** Helper: mapFirst to DoWith ***/

  // List map with an operator returning a DoWith
  def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]] = {
    l.foldRight[DoWith[W,List[B]]]( doreturn(Nil)) {
      (a,L0) => {
        val b0 = f(a)
        b0 flatMap {
          b => {
            L0 map {
              L => b::L
            }
          }
        }
      }
    }
  }

  // Map map with an operator returning a DoWith
  def mapWith[W,A,B,C,D](m: Map[A,B])(f: ((A,B)) => DoWith[W,(C,D)]): DoWith[W,Map[C,D]] = {
    m.foldRight[DoWith[W,Map[C,D]]](doreturn(Map())) {
      (a,M0) => {
        f(a) flatMap {
          case (c,d) => {
            M0 map { (m) => m + (c -> d) }
          }
        }
      }
    }
  }

  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  def mapFirstWith[W,A](l: List[A])(f: A => Option[DoWith[W,A]]): DoWith[W,List[A]] = l match {
    case Nil => doreturn(l)
    case h :: t => f(h) match {
      case None => mapFirstWith(t)(f) map { trailing => List(h) ::: trailing}
      case Some(d) => d map { a => List(a) ::: t}
    }
  }

  // There are better ways to deal with the combination of data structures like List, Map, and
  // DoWith, but we won't tackle that in this assignment.

  /*** Casting ***/
  //Error is type trying to cast to is "too big"

  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
      /***** Make sure to replace the case _ => ???. */
    case (TNull, TObj(_)) => true
    case (_,_) if (t1 == t2) => true
    case (TObj(fields1), TObj(fields2)) =>
      val up = fields2 forall {case (fieldName,ti) =>
        fields1.get(fieldName) match{
          case Some(t2) => if(ti == t2) true else false
          case None => true
        }}
      val down = fields1 forall {case (fieldName,ti) =>
        fields2.get(fieldName) match{
          case Some(t2) => if(ti == t2) true else false
          case None => true
        }}
      up || down
      /***** Cases for the extra credit. Do not attempt until the rest of the assignment is complete. */
    case (TInterface(tvar, t1p), _) => ???
    case (_, TInterface(tvar, t2p)) => ???
      /***** Otherwise, false. */
    case _ => false
  }

  /*** Type Inference ***/

  // A helper function to check whether a jsy type has a function type in it.
  // While this is completely given, this function is worth studying to see
  // how library functions are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }

  def isBindex(m: Mode, e: Expr): Boolean = m match {
    case MRef => e match {
      case Var(_) => true
      case GetField(_,_) => true
      case _ => false

    }
    case _ => true
  }

  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => env.get(x) match {
        case None => throw DynamicTypeError(e)
        case Some(m) => m.t
      }
      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
        /***** Cases directly from Lab 4. We will minimize the test of these cases in Lab 5. */
      case Unary(Not, e1) => typeof(env,e1) match {
        case TBool => TBool
        case other => err(other,e1)
      }
      case Binary(Plus, e1, e2) => {
        typeof(env,e1) match {
          case TNumber => if (typeof(env,e2) == TNumber) TNumber else err(typeof(env,e2),e2)
          case TString => if (typeof(env,e2) == TString) TString else err(typeof(env,e2),e2)
          case other => err(other,e1)
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

      case Obj(fields) => {
        val fieldsTypeMaps = fields.foldLeft(Map(): Map[String,Typ]) {
          (acc, objectField) => acc + (objectField._1 -> typeof(env,objectField._2))
        }
        TObj(fieldsTypeMaps)
      }
      case GetField(e1, f) => typeof(env,e1) match {
        case TObj(fields) => fields.get(f) match {
          case None => err(typeof(env,e1), e1)
          case Some(t) => t
        }
        case _ => err(typeof(env,e1), e1)
      }

        /***** Cases from Lab 4 that need a small amount of adapting. */
      case Decl(m, x, e1, e2) if (isBindex(m,e1)) => {
        val t1 = typeof(env, e1)
        val mt = MTyp(m, t1)
        val t2 = typeof(env + (x -> mt), e2)
        t2

      }

      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(tret)) =>
            val tprime = TFunction(params, tret)
            env + (f -> MTyp(MConst,tprime))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params.foldRight(env1) {
          case ((xn,mtn),acc) => {
            acc + (xn -> mtn)
          }
        }
        // Match on whether the return type is specified.
        val tFinal = TFunction(params,typeof(env2,e1))

        tann match {
          case None => tFinal
          case Some(expectedType) => if (expectedType == tFinal) tFinal else err(tFinal,e1)
        }
      }

      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) =>
          (params, args).zipped.foreach {
            (paramN,argumentN) => {
              val argType = typeof(env,argumentN)
              //DO WE NEED TO CHECK IF ALL THE TYPES ALIGN OR IS THAT
              if (paramN._2 != argType && !isBindex(paramN._2.m,e1)) err(argType,e) else argType //!isBindex or isBindex
            }
          }
          tret
        case tgot => err(tgot, e1)
      }

        /***** New cases for Lab 5. ***/
      case Assign(Var(x), e1) => {
        env.get(x) match {
          case Some(mt) => mt.m match {
            case (MVar | MRef)  => {
              if (typeof(env,e1) == mt.t) mt.t else err(typeof(env,e1),e)
            }
          }
        }

      }
      case Assign(GetField(e1, f), e2) =>
        typeof(env,e2)
      case Assign(_, _) => err(TUndefined, e)

      case Null => TNull

      case Unary(Cast(t), e1) => typeof(env, e1) match {
        case tgot if castOk(t,tgot) => tgot
        case tgot => err(tgot, e1)
      }

      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }

  /*** Small-Step Interpreter ***/

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3 and Lab 4.
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

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
      case Print(e1) => Print(subst(e1))
        /***** Cases from Lab 3 */
      case Unary(uop, e1) => Unary(uop,substitute(e1,esub,x))
      case Binary(bop, e1, e2) => Binary(bop,substitute(e1,esub,x),substitute(e2,esub,x))
      case If(e1, e2, e3) => If(substitute(e1,esub,x),substitute(e2,esub,x),substitute(e3,esub,x))
      case Var(y) => if (y == x) esub else Var(y)
        /***** Cases need a small adaption from Lab 3 */
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
        /***** Cases needing adapting from Lab 4 */
      case Function(p, params, retty, e1) => p match { //DONT UNDERSTAND WHERE THIS NEEDS ADAPTING
        case (Some(x)) => e
        case _ => {
          params.foreach {
            case (param, t) => if (x == param) return e
          }
          Function(p, params, retty, substitute(e1,esub,x))
        }
      }
        /***** Cases directly from Lab 4 */
      case Call(e1, args) => Call(substitute(e1,esub,x),args.map((arg) => substitute(arg,esub,x)))
      /***** New cases for Lab 4 */
      case Obj(fields) => Obj(fields.mapValues((v) => substitute(v,esub,x)))
      case GetField(e1, f) => GetField(substitute(e1,esub,x),f)
        /***** New case for Lab 5 */
      case Assign(e1, e2) => Assign(substitute(e1,esub,x),substitute(e2,esub,x))

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }

    def myrename(e: Expr): Expr = {
      val fvs = freeVars(esub)
      def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x
      rename[Unit](e)(???){ x => ??? }
    }

    subst(e)
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case (MConst | MVar) if (!isValue(e)) => true
    case (MRef) if (!isLValue(e)) => true
    case _ => false
  }

  def getBinding(mode: Mode, e: Expr): DoWith[Mem,Expr] = {
    require(!isRedex(mode,e), s"expression ${e} must not reducible under mode ${mode}")
    mode match {
      case MConst if (isValue(e))=> doreturn(e)
      case MRef if (isLValue(e)) => doreturn(e)
      case MVar if (isValue(e)) => {
        val a = memalloc(e)
        a map {
          ap => Unary(Deref, ap)
        }
      }
      case MName => doreturn(e)
    }
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
    require(!isValue(e), "stepping on a value: %s".format(e))
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => doget map { m => println(pretty(m, v1)); Undefined }
        /***** Cases needing adapting from Lab 3. */
      case Unary(Neg,N(v)) => doreturn(N(-v))
      case Unary(Not,B(v))  => doreturn(B(!v))
      case Binary(Seq,v1,e2) if (isValue(v1)) => doreturn(e2)
      case Binary(bop @ (Plus | Minus | Times | Div),N(x),N(y)) =>
        bop match {
          case Plus => doreturn(N(x + y))
          case Minus => doreturn(N(x - y))
          case Times => doreturn(N(x * y))
          case Div => doreturn(N(x / y))
        }
      case Binary(Plus,S(x),S(y))  => doreturn(S(x + y))

      case Binary(bop @ (Gt | Ge | Lt | Le),v1 @ N(x),v2 @ N(y)) => doreturn(B(inequalityVal(bop,v1,v2)))
      case Binary(bop @ (Gt | Ge | Lt | Le),v1 @ S(x),v2 @ S(y)) => doreturn(B(inequalityVal(bop,v1,v2)))

      case Binary(Eq,v1,v2) if (isValue(v1) && isValue(v2)) => doreturn(B(v1 == v2))
      case Binary(Ne,v1,v2) if (isValue(v1) && isValue(v2)) => doreturn(B(v1 != v2))

      case Binary(And,B(true),e2) => doreturn(e2)
      case Binary(And,B(false),_) => doreturn(B(false))
      case Binary(Or, B(true),_) => doreturn(B(true))
      case Binary(Or,B(false),e2) => doreturn(e2)

      case If(B(true),e2,_) => doreturn(e2)
      case If(B(false),_,e3) => doreturn(e3)
        /***** More cases here */
        /***** Cases needing adapting from Lab 4. */
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) => {
        memalloc(e)
      }

      case GetField(a @ A(_), f) => {
        doget map {
          mem => mem.get(a) match {
            case Some(Obj(fields)) => {
              fields.get(f) match {
                case Some(v) if (isValue(v))=> v
                case _ => throw StuckError(e)
              }
            }
            case _ => throw StuckError(e)
          }
        }
      }

//      case Decl(MConst, x, v1, e2) if isValue(v1) =>
//        doreturn(substitute(e2, v1, x))
//      case Decl(MVar, x, v1, e2) if isValue(v1) =>
//        memalloc(v1) map {
//          a => substitute(e2,Unary(Deref,a),x)
//        }
      case Decl(m,x,e1,e2) if (!isRedex(m,e1))=> {
        val dwe1 = getBinding(m,e1)
        dwe1 map {
          e1p => substitute(e2,e1p,x)
        }
      }
        /***** New cases for Lab 5. */
      case Unary(Deref, a @ A(_)) => {
        doget map {
          mem => mem.get(a) match {
            case Some(e) => e
            case  None => throw StuckError(e)
          } //
        }
      }


      case Assign(Unary(Deref, a @ A(_)), v) if isValue(v) =>
        domodify[Mem] { m => m + (a -> v) } map {_ => v}

      case Assign(GetField(a @ A(_), f), v) if isValue(v) =>
        domodify[Mem] {
          mem => mem.get(a) match {
            case Some(Obj(fields)) => {
              mem + (a -> Obj(fields + (f -> v))) //update field or add new field to object in memory
          }
            case _ => throw StuckError(e)
          }
        } map {
          _ => v
        }

      case Call(v @ Function(p, params, _, e), args) => { //THIS IS VERY LIKELY INCORRECT
        val pazip = params zip args
        if (pazip.forall( (x) =>
          x match {
            case ((xn,mtn),en) => !isRedex(mtn.m,en)
          }

        )) {
          val dwep = pazip.foldRight( doreturn(e) : DoWith[Mem,Expr] )  {
            case (((xi, MTyp(mi, _)), ei), dwacc) => dwacc flatMap {
              (eacc) => getBinding(mi,ei) map {
                (eip) => substitute(eacc,eip,xi)
              }
            }
          }
          p match {
            case None => dwep
            case Some(x) => dwep map {
              (e1p) => substitute(e1p,v,x)
            }
          }
        }
        else {
          val dwpazipp = mapFirstWith(pazip) {
            case (pair @ (nameAndMTyp,ei)) => nameAndMTyp match {
              case (xi,mti) => {
                if (isRedex(mti.m,ei)) {
                  val dwstep = step(ei)
                  val dwFinal = dwstep map {
                    (estepped) => (nameAndMTyp,estepped)
                  }
                  Some(dwFinal)
                  //Some(doreturn((nameAndMTyp,step(ei))))
                } else None
              }
            }
          }
          dwpazipp map {
            (l) => {
              val (_,argslist) = l.unzip
              Call(v,argslist)
            }
          }
        }
      }

        /* Base Cases: Error Rules */
        /***** Replace the following case with a case to throw NullDeferenceError.  */
      //case _ => throw NullDeferenceError(e)

      /* Inductive Cases: Search Rules */
        /***** Cases needing adapting from Lab 3. Make sure to replace the case _ => ???. */
      case Print(e1) => step(e1) map { e1p => Print(e1p) }
      case Unary(uop, e1) => step(e1) map {
          (e1p) => Unary(uop,e1p)
        }
      case Binary(bop,v1,e2) if isValue(v1) => step(e2) map {
        (e2p) => Binary(bop,v1,e2p)
      }
      case Binary(bop,e1,e2) => step(e1) map {
        (e1p) => Binary(bop,e1p,e2)
      }

      case If(e1,e2,e3) => step(e1) map {
        (e1p) => If(e1p,e2,e3)
      }
        /***** Cases needing adapting from Lab 4 */
      case GetField(e1, f) => step(e1) map {
        (e1p) => GetField(e1p,f)
      }

      case Obj(fields) => {
        fields find {
          case (_,en) => !isValue(en)
        } match {
          case Some((x,e1)) => {
            step(e1) map {
              (e1p) => Obj(fields + (x -> e1p))
            }
          }
          case None => throw StuckError(e) //should be handled by do rule and never run
        }
      }


      case Decl(mode, x, e1, e2) =>
        if (isRedex(mode,e1)) {
          step(e1) map {
            (e1p) => Decl(mode,x,e1p,e2)
          }
        } else {
          throw StuckError(e)
        }
      case Call(e1, args) => step(e1) map {
        (e1p) => Call(e1p,args)
      }

        /***** New cases for Lab 5.  */

      case Unary(Cast(_),v) if isValue(v) && (v match { case (A(_)) => false case _ => true }) => {
        doreturn(v)
      }

      case Unary(Cast(TObj(fields)), Null) => {
        doreturn(Null)
      }

      case Unary(Cast(TObj(fields)), a @ A(_)) => {
        //if there are any fields in type that are not in M(a) than error; otherwise ok
        doget.map { mem =>
          val e: Expr = mem(a)
          e match {
            case Obj(memfields) => {
              // memfieelds maps strings to expressions
              // fields maps strings to types
              val r: Boolean = fields forall { case (fieldname,fieldtype) => memfields.contains(fieldname) }

              if (r) { a }  else { throw DynamicTypeError(e) }
            }
          }
        }
      }
      case Assign(e1, e2) if !isLValue(e1) =>
        step(e1) map {
          (e1p) => Assign(e1p,e2)
        }
      case Assign(e1, e2) =>
        step(e2) map {
          (e2p) => Assign(e1,e2p)
        }

      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** Extra Credit: Lowering: Remove Interface Declarations ***/

  def lower(e: Expr): Expr =
    /* Do nothing by default. Change to attempt extra credit. */
    e

  /*** External Interfaces ***/

  //this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}
