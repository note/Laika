package laika.parse.code.languages

import cats.data.NonEmptyList
import laika.bundle.SyntaxHighlighter
import laika.parse.code.CodeSpanParser
import laika.parse.code.common.Identifier.IdParser
import laika.parse.code.common.{Comment, Keywords, NumberLiteral}
import laika.parse.text.CharGroup.{digit, lowerAlpha, upperAlpha}

// https://alloytools.org/download/alloy-language-reference.pdf
object AlloySyntax extends SyntaxHighlighter {
  /** The names of the language (and its optional aliases) as used in text markup */
  override def language: NonEmptyList[String] = NonEmptyList.of("alloy")

  val comment: CodeSpanParser =
    Comment.singleLine("--") ++ Comment.singleLine("//") ++ Comment.multiLine("/*", "*/")

  val keywords = Keywords(
    "abstract", "all", "and", "as", "assert",
    "but", "check", "disj", "else", "exactly",
    "extends","fact", "for", "fun", "iden",
    "iff", "implies", "in", "Int", "let",
    "lone", "module", "no", "none", "not",
    "one", "open", "or", "pred", "run",
    "set", "sig", "some", "sum", "univ",
    "enum"
  )

  val numberLiteral = NumberLiteral.decimalInt

  val identifiers = IdParser(lowerAlpha ++ upperAlpha, digit.add('_').add('\'').add('"'))

  /** The parsers for individual code spans written in this language */
  override def spanParsers: Seq[CodeSpanParser] = Seq(
    comment,
    keywords,
    identifiers,
    numberLiteral
  )
}
