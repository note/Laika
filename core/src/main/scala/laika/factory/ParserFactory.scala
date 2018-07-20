/*
 * Copyright 2013-2016 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.factory

import com.typesafe.config.ConfigFactory
import laika.api.ext.{ExtensionBundle, ParserDefinitionBuilders, RootParserHooks}
import laika.io.Input
import laika.parse.core.combinator.Parsers
import laika.parse.core.markup.{DocumentParser, RootParserBase}
import laika.tree.Documents.Document
import laika.tree.Paths.Path

/** Responsible for creating parser instances for a specific markup format.
 *  A parser is simply a function of type `Input => Document`.
 *  
 *  @author Jens Halm
 */
trait ParserFactory {
  
  
  /** The file suffixes recognized by this parser.
   *  When transforming entire directories only files with
   *  names ending in one of the specified suffixes will
   *  be considered.
   * 
   *  It is recommended not to support `txt`
   *  or similarly common suffixes as this might interfere
   *  with other installed formats.
   */
  def fileSuffixes: Set[String]
  
  /** The parser-specific extensions that need to be installed
   *  for each transformation that involves this parser.
   * 
   *  One scenario where a parser needs to provide a bundle
   *  is when it produces tree elements that are unknown
   *  to the built-in rewrite rules and renderers.
   */
  def extensions: Seq[ExtensionBundle]

  def newRootParser (parserExtensions: ParserDefinitionBuilders): RootParserBase // TODO - temporary

  /**  Creates a new parser instance.
   *   Such an instance is expected to be stateless and thread-safe,
   *   thus capable of repeated and parallel executions.
   */
  def newParser (parserExtensions: ParserDefinitionBuilders): Input => Document = {

    // TODO - extract this logic into DocumentParser and/or OperationConfig
    val configHeaderParsers = parserExtensions.configHeaderParsers :+ { _:Path => Parsers.success(Right(ConfigFactory.empty)) }
    val configHeaderParser = { path: Path => configHeaderParsers.map(_(path)).reduce(_ | _) }

    val hooks = parserExtensions.rootParserHooks.getOrElse(RootParserHooks())

    hooks.preProcessInput andThen DocumentParser.forMarkup(newRootParser(parserExtensions).rootElement, configHeaderParser) andThen hooks.postProcessDocument
  }

}
