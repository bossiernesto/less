package org.uqbar.less.ide.components

import org.uqbar.less.ide.components.StyleAttribute._
import java.awt.Dimension
import scala.swing.event.ValueChanged
import org.uqbar.less.parser.Parse
import scala.swing.event.Event
import org.uqbar.less.encoder.PreferenceFixture

class Editor extends FormattedTextArea {

	preferredSize = new Dimension(800, 400)

	reactions += { case _: ValueChanged => publish(EditorParsed(this, parse.successful)) }

	def parse = Parse(text)

	def applyPreferences(fixture: PreferenceFixture) = {
		defineStyle('default)(
			FontFamily("Ubuntu Mono"),
			FontSize(16),
			TabWidth(fixture[Int]("Tabulation", "Tabulation Size"))
		)
		applyStyle('default, true)()
	}
}

//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// EVENTS
//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════

case class EditorParsed(source: Editor, successful: Boolean) extends Event