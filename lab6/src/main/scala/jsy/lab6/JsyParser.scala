package jsy.lab6

import jsy.lab5
import jsy.lab5.{ast => ast5}
import jsy.lab6.ast._
import jsy.lab6.{ast => ast6}
import scala.util.parsing.input.StreamReader
import java.io.{InputStreamReader,FileInputStream}
import java.io.InputStream
import java.io.File

trait JSTokens extends lab5.JSTokens {
  case class RegExpLiteral(chars: String) extends Token {
    override def toString = chars
  }
}

class Lexer extends lab5.Lexer with JSTokens {
  override def token: Parser[Token] =
    regexp ^^ { s => RegExpLiteral(s) } |
    super.token
    
  import scala.util.parsing.input.CharArrayReader.EofCh
  def regexp: Parser[String] =
    '/' ~> '^' ~> rep( chrExcept('$', '/', EofCh) ) <~ '$' <~ '/' ^^ { cs => cs.mkString }
}

trait TokenParser extends lab5.TokenParser {
  type Tokens <: JSTokens
  def regexpLit: Parser[String] =
    elem("regexp", _.isInstanceOf[lexical.RegExpLiteral]) ^^ (_.chars)
}

class JsyParser(regExprParser: String => RegExpr) extends TokenParser with lab5.Parser {
  type Tokens = JSTokens
  val lexical = new Lexer

  /* Lexer Set Up */
  lexical.reserved ++= lab5.Parser.lexical.reserved
  lexical.reserved += "RegExp"
  lexical.delimiters ++= lab5.Parser.lexical.delimiters
  lexical.delimiters ++= List("/", "^", "$")

  override def term: Parser[ast5.Expr] =
    positioned(
      regexpLit ^^ { s => RE(regExprParser(s)).asInstanceOf[ast5.Expr] }
    ) |
    super.term

  //override def jsy: Parser[ast5.Expr] =
  //  (super.jsy map { _.asInstanceOf[ast5.Expr] }) |
  // "jsy" ~> "." ~> withpos(ident) ~ ("(" ~> repsep(noseq, ",") <~ ")") ^^ {
  //    case ((pos, f)) ~ args =>
  //      val e = RuntimeCall(f, args map { x => x.asInstanceOf[ast6.Expr]}) setPos pos
  //      e.asInstanceOf[ast5.Expr]
  //  }

  type Expr = ast6.Expr

  def parseTokens(tokens: lexical.Scanner): Expr = {
    phrase(prog)(tokens) match {
      case Success(e, _) => e.asInstanceOf[Expr]
      case NoSuccess(msg, next) => throw SyntaxError(msg, next.pos)
    }
  }

  def parseTypTokens(tokens: lexical.Scanner): Typ = {
    phrase(ty)(tokens) match {
      case Success(t, _) => t.asInstanceOf[Typ]
      case NoSuccess(msg, next) => throw SyntaxError(msg, next.pos)
    }
  }

  /*** External Interface ***/

  def formatErrorMessage(e: Expr, kind: String, msg: String): String =
    formatErrorMessage(e.pos, kind, msg)

  def parse(s: String): Expr = {
    parseTokens(new lexical.Scanner(s))
  }

  def parseTyp(s: String): Typ = {
    parseTypTokens(new lexical.Scanner(s))
  }

  def parse(in: InputStream): Expr = {
    val reader = StreamReader(new InputStreamReader(in))
    parseTokens(new lexical.Scanner(reader))
  }

  def parseFile(filename: String): Expr = {
    parseSource = filename
    parse(new FileInputStream(filename))
  }

  def parseFile(file: File): Expr = {
    parseSource = file.getName
    parse(new FileInputStream(file))
  }

  implicit class StringExpr(s: String) {
    val e = parse(s)
    def a: Expr = e
  }

  implicit class StringTyp(s: String) {
    val typ = parseTyp(s)
    def t: Typ = typ
  }
}

object JsyParser extends JsyParser(RegExprParser.parse)
