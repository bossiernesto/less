package org.uqbar.less.ide.components

import org.uqbar.less.ide.components.StyleAttribute._
import java.awt.Dimension
import scala.swing.event.ValueChanged
import org.uqbar.less.parser.Parse
import scala.swing.event.Event

class Editor extends FormattedTextArea {

	preferredSize = new Dimension(800, 400)

	defineStyle('default)(
		FontFamily("Ubuntu Mono"),
		FontSize(16),
		TabWidth(4)
	)

	reactions += { case _: ValueChanged => publish(EditorParsed(this, parse.successful)) }

	def parse = Parse(text)
}

//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// EVENTS
//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════

case class EditorParsed(source: Editor, successful: Boolean) extends Event