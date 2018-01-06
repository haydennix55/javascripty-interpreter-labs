package jsy.lab2

import jsy.lab2.ast.Expr
import jsy.lab2.ast.isValue
import jsy.lab2.Parser.parse

trait Lab2Like {

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
  def eval(env: Env, e: Expr): Expr

  /** Interface to run your interpreter starting from an empty environment. */
  def eval(e: Expr): Expr = eval(empty, e)

  /** Interface to run your interpreter from a string.  This is convenient for unit testing. */
  def eval(s: String): Expr = eval(parse(s))
}
