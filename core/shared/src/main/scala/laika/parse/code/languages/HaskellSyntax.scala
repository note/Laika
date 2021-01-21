package laika.parse.code.languages

import cats.data.NonEmptyList
import laika.bundle.SyntaxHighlighter
import laika.parse.code.common.Identifier.IdParser
import laika.parse.code.common.{Comment, Keywords, NumberLiteral, StringLiteral}
import laika.parse.code.languages.ScalaSyntax.charEscapes
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.text.CharGroup.{digit, lowerAlpha, upperAlpha}

object HaskellSyntax extends SyntaxHighlighter {
  /** The names of the language (and its optional aliases) as used in text markup */
  override def language: NonEmptyList[String] = NonEmptyList.of("hs", "haskell")

  val comment: CodeSpanParser = Comment.singleLine("--") ++ Comment.multiLine("{-", "-}")

  val keywords =
    Keywords("as", "case", "data", "data family", "data instance", "default", "deriving",
      "deriving instance", "do", "else", "forall", "foreign", "hiding", "if", "import", "in",
      "infix", "infixl", "infixr", "instance", "let", "mdo", "module", "newtype", "of",
      "proc", "qualified", "rec", "then", "type", "type family", "type instance", "where"
    )

  val stringLiteral =
    StringLiteral.singleLine('"').embed(charEscapes)

  val numberLiteral =
    NumberLiteral.hex ++ NumberLiteral.octal ++ NumberLiteral.decimalFloat ++ NumberLiteral.decimalInt

  val identifiers = IdParser(lowerAlpha.add('_').add('\''), digit ++ upperAlpha)

  val types = IdParser(upperAlpha.add('_').add('\''), digit ++ lowerAlpha).withCategory(CodeCategory.TypeName)

  /** The parsers for individual code spans written in this language */
  override def spanParsers: Seq[CodeSpanParser] = Seq(
    comment,
    stringLiteral,
    keywords,
    identifiers,
    types,
    numberLiteral
  )
}
