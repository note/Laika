package laika.format

import cats.effect.Async
import laika.ast.DocumentTreeRoot
import laika.runtime.OutputRuntime
import laika.factory.{BinaryPostProcessor, RenderFormat, TwoPhaseRenderFormat}
import laika.io.{BinaryOutput, RenderedDocument, RenderedTree, RenderedTreeRoot}
import laika.render.TextFormatter

object TestRenderResultProcessor extends TwoPhaseRenderFormat[TextFormatter, BinaryPostProcessor] {

  val interimFormat: RenderFormat[TextFormatter] = AST

  def prepareTree (tree: DocumentTreeRoot): DocumentTreeRoot = tree

  object postProcessor extends BinaryPostProcessor {
    
    override def process[F[_] : Async] (result: RenderedTreeRoot, output: BinaryOutput): F[Unit] = {
      
      def append (sb: StringBuilder, result: RenderedTree): Unit = {
        result.content.foreach {
          case d: RenderedDocument => sb.append(d.content + "\n")
          case t: RenderedTree => append(sb, t)
          case _ => ()
        }
      }

      val sb = new StringBuilder
      append(sb, result.tree)
      val resultString = sb.toString

      OutputRuntime.asStream(output).use { out =>
        Async[F].delay(out.write(resultString.getBytes("UTF-8")))
      }
    }
  }
  
}