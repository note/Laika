package laika.parse.code.languages

import cats.data.NonEmptyList
import cats.implicits._
import laika.ast.~
import laika.ast.CodeSpan
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.code.common.NumberLiteral.NumericParser
import laika.parse.code.common._
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.implicits._
import laika.parse.text.{CharGroup, PrefixedParser}
import laika.parse.text.TextParsers._

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

  val stringLiteral = StringLiteral.singleLine('"') ++
    StringLiteral.multiLine("''")

  val numberLiteral = NumberLiteral.hex ++
    NumberLiteral.decimalFloat ++
    NumericParser(CharGroup.digit, someOf('-', '+').max(1).some) ++
    NumberLiteral.decimalInt

  val identifier = Identifier.alphaNum

  val anyOfWs = anyOf(' ', '\t', '\n')

  val tpe: PrefixedParser[Seq[CodeSpan]] = {
    val t = someOf(CharGroup.alphaNum.add('_'))
    val nonKindedType = (t ~ (literal(".") ~ t).rep).map { case a ~ seq =>
      seq.lastOption match {
        case Some(last) =>
          Seq(CodeSpan(a, CodeCategory.Identifier)) ++
            seq.dropRight(1).flatMap { case sep ~ id => Seq(CodeSpan(sep), CodeSpan(id, CodeCategory.Identifier)) } ++
            Seq(CodeSpan(last._1), CodeSpan(last._2, CodeCategory.TypeName))
        case None =>
          Seq(CodeSpan(a, CodeCategory.TypeName))
      }
    }

    (nonKindedType ~ ((literal(" -> ") | someOf(' ', '\t')).other ~ nonKindedType).rep.map(_.flatMap { case h ~ t =>
      h +: t
    })).concat
  }

  val beginningOfLet =
    ((literal("let").map(s => Seq(CodeSpan(s, CodeCategory.Keyword))) ~ someOf(' ', '\t').other.map(Seq(_))).concat ~
      Identifier.alphaNum.withCategory(CodeCategory.DeclarationName).map(Seq(_))).concat

  val equals = literal("=").other.map(Seq(_))

  val typedDeclaration = {
    val parser = {
      beginningOfLet ~
      (anyOfWs.other ~ literal(": ").other ~ anyOfWs.other).mapN(Seq(_, _, _)) ~
      tpe ~ anyOfWs.other.map(Seq(_)) ~ equals
    }
    CodeSpanParser(parser.concat)
  }

  val untypedDeclaration =
    CodeSpanParser(
      (beginningOfLet ~ anyOfWs.other.map(Seq(_)) ~ equals).concat
    )

  val attrName = identifier.withCategory(CodeCategory.AttributeName)

  val recordEntry = CodeSpanParser((attrName ~ (anyOfWs ~ oneOf('=')).other).mapN(Seq(_, _)))

  val recordTypeEntry =
    CodeSpanParser(
      (attrName ~ (anyOfWs ~ literal(": ") ~ anyOfWs).other.map(Seq(_)) ~ tpe).concat
    )

  /** The parsers for individual code spans written in this language */
  override def spanParsers: Seq[CodeSpanParser] = Seq(
    comment,
    stringLiteral,
    typedDeclaration,
    untypedDeclaration,
    recordTypeEntry,
    recordEntry,
    keywords,
    identifier,
    numberLiteral
  )

  private implicit class ParserOps(p: Parser[String]) {
    def other = p.map(s => CodeSpan(s))
  }

  private implicit class ParserOps2(p: Parser[String ~ String]) {
    def other = p.map { case s1 ~ s2 => CodeSpan(s1 + s2)}
  }

  private implicit class ParserOps3(p: Parser[String ~ String ~ String]) {
    def other = p.map { case s1 ~ s2 ~ s3 => CodeSpan(s1 + s2 + s3)}
  }

}
