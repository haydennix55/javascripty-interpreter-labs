package jsy.lab6

import jsy.util.Visitor

class ast extends jsy.lab5.ast {
  /* Regular Expressions */
  case class RE(re: RegExpr) extends Expr
  case object TRegExp extends Typ

  sealed abstract class RegExpr
  case object RNoString extends RegExpr
  case object REmptyString extends RegExpr
  case class RSingle(c: Char) extends RegExpr
  case class RConcat(re1: RegExpr, re2: RegExpr) extends RegExpr
  case class RUnion(re1: RegExpr, re2: RegExpr) extends RegExpr
  case class RStar(re1: RegExpr) extends RegExpr
  case object RAnyChar extends RegExpr
  case class RPlus(re1: RegExpr) extends RegExpr
  case class ROption(re1: RegExpr) extends RegExpr
  case class RIntersect(re1: RegExpr, re2: RegExpr) extends RegExpr
  case class RNeg(re1: RegExpr) extends RegExpr

  /* Generic Run-Time Call */
  //case class RuntimeCall(f: String, args: List[Expr]) extends Expr

  override def isValue(e: Expr): Boolean = e match {
    case RE(_) => true
    case _ => super.isValue(e)
  }

  override def prettyVal: Visitor[Expr,String] = super.prettyVal extendWith (_ => {
    case RE(re) => "/^" + pretty(re) + "$/"
  })

  def pretty(re: RegExpr): String = re match {
    case RNoString => "!"
    case REmptyString => "#"
    case RSingle(c) => c.toString
    case RConcat(re1,re2) => pretty(re1) + pretty(re2)
    case RUnion(re1,re2) => s"(${pretty(re1)}|${pretty(re2)})"
    case RStar(re1) => s"(${pretty(re1)})*"
    case RAnyChar => "."
    case RPlus(re1) => s"(${pretty(re1)})+"
    case ROption(re1) => s"(${pretty(re1)})?"
    case RIntersect(re1,re2) => s"(${pretty(re1)}&${pretty(re2)})"
    case RNeg(re1) => s"~(${pretty(re1)})"
  }

  override def prettyTyp: Visitor[Typ,String] = super.prettyTyp extendWith (_ => {
    case TRegExp => "RegExp"
  })

  override def freeVarsVar: Visitor[Expr,Set[Var]] = super.freeVarsVar extendWith (_ => {
    case RE(_) => Set.empty
  })
}

object ast extends ast
