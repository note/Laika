package laika.parse.code.languages

import cats.data.NonEmptyList
import laika.ast.CodeSpan
import laika.bundle.SyntaxHighlighter
import laika.parse.builders._
import laika.parse.code.common.{Comment, Identifier, NumberLiteral, StringLiteral}
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.implicits._
import laika.parse.text.TextParsers.{literal, oneOf}

object YAMLSyntax extends SyntaxHighlighter {
  /** The names of the language (and its optional aliases) as used in text markup */
  override def language: NonEmptyList[String] = NonEmptyList.of("yaml", "yml")

  private val keyName = Identifier.alphaNum.withIdStartChars('_', '-', '.').withCategory(CodeCategory.AttributeName)

  private val valueLiteral = {
    val doubleQuotedStrLiteral = StringLiteral.singleLine('"')
    val singleQuotedStrLiteral = StringLiteral.singleLine('\'')
    val number = NumberLiteral.decimalFloat | NumberLiteral.decimalInt
    val boolean = (literal("true") | literal("false")).map(b => Seq(CodeSpan(b, CodeCategory.BooleanLiteral)))
    val nullLiteral = (literal("null")).map(n => Seq(CodeSpan(n, CodeCategory.LiteralValue)))
    val unquotedStrLiteral = delimitedBy(literal(" #") | literal("\n")).acceptEOF.keepDelimiter.map(s => List(CodeSpan(s, CodeCategory.StringLiteral)))

    doubleQuotedStrLiteral | singleQuotedStrLiteral | number | boolean | nullLiteral | unquotedStrLiteral
  }

  private val keyAndValue = {
    val attributeName = {
      // https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html#yaml-basics:
      // the colon must be followed by a space
      (keyName ~ oneOf(':') ~ oneOf(' ','\t')).map { r =>
        List(r._1._1, CodeSpan(r._1._2 + r._2))
      }
    }

    (attributeName ~ valueLiteral).concat
  }

  private val key: CodeSpanParser = {
    CodeSpanParser((keyName ~ oneOf(':') ~ anyOf(' ','\t') ~ oneOf('\n')).map { r =>
      List(r._1._1._1, CodeSpan(r._1._1._2 + r._1._2 + r._2))
    })
  }

  private val seqItem = {
    val parser = (literal("- ").map(s => CodeSpan(s)) ~ (keyAndValue | valueLiteral)).concat
    CodeSpanParser(parser)
  }

  /** The parsers for individual code spans written in this language */
  override def spanParsers: Seq[CodeSpanParser] = Seq(
    Comment.singleLine("#"),
    CodeSpanParser(keyAndValue),
    key,
    seqItem
  )
}
