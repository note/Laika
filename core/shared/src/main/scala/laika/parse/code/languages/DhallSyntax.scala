package laika.parse.code.languages

import cats.implicits._
import cats.data.NonEmptyList
import laika.ast
import laika.ast.CodeSpan
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.code.CodeCategory.{DeclarationName, TypeName}
import laika.parse.code.common.NumberLiteral.NumericParser
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.code.common.{Comment, Identifier, Keywords, NumberLiteral, StringLiteral}
import laika.parse.text.{CharGroup, PrefixedParser}
import laika.parse.text.TextParsers._
import laika.parse.implicits._

// https://github.com/dhall-lang/dhall-lang/blob/master/standard/dhall.abnf
object DhallSyntax extends SyntaxHighlighter {
  /** The names of the language (and its optional aliases) as used in text markup */
  override def language: NonEmptyList[String] = NonEmptyList.of("dhall")

  val comment =
    Comment.singleLine("--") ++ Comment.multiLine("{-", "-}")

  val keywords = Keywords(
    "if", "then", "else", "let", "in",
    "using", "missing", "assert", "as",
    "Infinity", "NaN", "merge", "Some",
    "toMap", "forall-keyword", "with"
  )

  val stringLiteral = StringLiteral.singleLine('"') ++ StringLiteral.multiLine("''")

  val numberLiteral = NumberLiteral.hex ++
    NumberLiteral.decimalFloat ++
    NumericParser(CharGroup.digit, someOf('-', '+').max(1).some) ++
    NumberLiteral.decimalInt


  val anyOfWs = anyOf(' ', '\t')

  val tpe = someOf(CharGroup.alphaNum.add('_'))

  val identifier = Identifier.alphaNum.withCategory(CodeCategory.Identifier)

  val typedDeclaration = {
    val parser =
      (literal("let").map(s => Seq(CodeSpan(s, CodeCategory.Keyword))) ~ someOf(' ', '\t').other.map(Seq(_))).concat ~
      Identifier.alphaNum.withCategory(CodeCategory.DeclarationName).map(Seq(_)) ~
      (anyOfWs.other ~ literal(": ").other ~ anyOfWs.other).mapN(Seq(_, _, _)) ~
      (tpe ~ ((literal(" -> ") | someOf(' ', '\t')) ~ tpe).rep.map(_.flatMap { case t =>
        Seq(CodeSpan(t._1), CodeSpan(t._2, TypeName))
      })).map(t => Seq(CodeSpan(t._1, TypeName)) ++ t._2) ~ anyOfWs.other.map(Seq(_)) ~ literal("=").other.map(Seq(_))
    CodeSpanParser(parser.concat)
  }

  val untypedDeclaration = {
    val parser =
      (literal("let").map(s => Seq(CodeSpan(s, CodeCategory.Keyword))) ~ someOf(' ', '\t').other.map(Seq(_))).concat ~
        Identifier.alphaNum.withCategory(CodeCategory.DeclarationName).map(Seq(_)) ~
        anyOfWs.other.map(Seq(_)) ~
        literal("=").other.map(Seq(_))
    CodeSpanParser(parser.concat)
  }

  /** The parsers for individual code spans written in this language */
  override def spanParsers: Seq[CodeSpanParser] = Seq(
    comment,
    stringLiteral,
    typedDeclaration,
    untypedDeclaration,
    keywords,
    identifier,
    numberLiteral
  )

  private implicit class ParserOps(p: Parser[String]) {
    def other = p.map(s => CodeSpan(s))
  }
}
