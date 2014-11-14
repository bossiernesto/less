package org.uqbar.less.ide.components

import java.awt.Color

import scala.collection.JavaConversions.enumerationAsScalaIterator
import scala.swing.TextComponent
import scala.swing.event.ValueChanged

import org.uqbar.less.ide.components.StyleAttribute._
import org.uqbar.less.ide.components.AlignmentValue._

import javax.swing.JTextPane
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.DefaultStyledDocument
import javax.swing.text.SimpleAttributeSet
import javax.swing.text.StyleConstants
import javax.swing.text.StyleContext.DEFAULT_STYLE
import javax.swing.text.StyledEditorKit
import javax.swing.text.{ TabSet => JTabSet }

class FormattedTextArea extends TextComponent {

	override lazy val peer: JTextPane = new JTextPane with SuperMixin { setEditorKit(new StyledEditorKit) }

	protected lazy val document = new DefaultStyledDocument {
		addDocumentListener(new DocumentListener {
			def changedUpdate(e: DocumentEvent) { publish(new ValueChanged(FormattedTextArea.this)) }
			def insertUpdate(e: DocumentEvent) { publish(new ValueChanged(FormattedTextArea.this)) }
			def removeUpdate(e: DocumentEvent) { publish(new ValueChanged(FormattedTextArea.this)) }
		})
		peer.setDocument(this)
	}

	def isDefinedStyle(name: Symbol) = document.getStyleNames.contains(name.toString)

	def defineStyle(name: Symbol, parentName: Symbol = null)(attributes: StyleAttribute*) {

		if (isDefinedStyle(name)) document.removeStyle(name.toString)

		val parent = document.getStyle(Option(parentName).fold(DEFAULT_STYLE)(_.toString))
		val newStyle = document.addStyle(name.toString, parent)

		for (attribute <- attributes) newStyle.addAttribute(attribute match {
			case _: FontFamily => StyleConstants.FontFamily
			case _: FontSize => StyleConstants.FontSize
			case _: Bold => StyleConstants.Bold
			case _: Italic => StyleConstants.Italic
			case _: Underline => StyleConstants.Underline
			case _: StrikeThrough => StyleConstants.StrikeThrough
			case _: Superscript => StyleConstants.Superscript
			case _: Subscript => StyleConstants.Subscript
			case _: FontColor => StyleConstants.Foreground
			case _: Background => StyleConstants.Background
			case _: FirstLineIndent => StyleConstants.FirstLineIndent
			case _: LeftIndent => StyleConstants.LeftIndent
			case _: RightIndent => StyleConstants.RightIndent
			case _: LineSpacing => StyleConstants.LineSpacing
			case _: SpaceAbove => StyleConstants.SpaceAbove
			case _: SpaceBelow => StyleConstants.SpaceBelow
			case _: Alignment => StyleConstants.Alignment
			case _: TabSet => StyleConstants.TabSet
		}, attribute match {
			case Alignment(Right) => StyleConstants.ALIGN_RIGHT
			case Alignment(Left) => StyleConstants.ALIGN_LEFT
			case Alignment(Justified) => StyleConstants.ALIGN_JUSTIFIED
			case Alignment(Center) => StyleConstants.ALIGN_CENTER
			case _ => attribute.value
		})

		if (parentName == null) document.setLogicalStyle(0, newStyle)
	}

	def applyStyle(name: Symbol, replace: Boolean = false)(offset: Int = 0, length: Int = text.size) {
		require(isDefinedStyle(name), s"Undefined Style: $name")

		document.setCharacterAttributes(offset, length, document.getStyle(name.toString), replace)
	}

	def write(s: String) = document.insertString(text.size, s, null)
	def write(styleNames: Symbol*)(s: String) = {
		require(styleNames.forall(isDefinedStyle), s"Undefined Style(s): ${styleNames.filterNot(isDefinedStyle).mkString(", ")}")

		val attributes = new SimpleAttributeSet
		for (styleName <- styleNames) attributes.addAttributes(document.getStyle(styleName.toString))

		document.insertString(text.size, s, attributes)
	}
}

trait StyleAttribute { def value: Any }
object StyleAttribute {
	case class FontFamily(value: String) extends StyleAttribute
	case class FontSize(value: Int) extends StyleAttribute
	case class Bold(value: Boolean) extends StyleAttribute
	case class Italic(value: Boolean) extends StyleAttribute
	case class Underline(value: Boolean) extends StyleAttribute
	case class StrikeThrough(value: Boolean) extends StyleAttribute
	case class Superscript(value: Boolean) extends StyleAttribute
	case class Subscript(value: Boolean) extends StyleAttribute
	case class FontColor(value: Color) extends StyleAttribute
	case class Background(value: Color) extends StyleAttribute
	case class FirstLineIndent(value: Float) extends StyleAttribute
	case class LeftIndent(value: Float) extends StyleAttribute
	case class RightIndent(value: Float) extends StyleAttribute
	case class LineSpacing(value: Float) extends StyleAttribute
	case class SpaceAbove(value: Float) extends StyleAttribute
	case class SpaceBelow(value: Float) extends StyleAttribute
	case class Alignment(value: AlignmentValue) extends StyleAttribute
	case class TabSet(value: JTabSet) extends StyleAttribute
}

sealed trait AlignmentValue
object AlignmentValue {
	case object Right extends AlignmentValue
	case object Left extends AlignmentValue
	case object Justified extends AlignmentValue
	case object Center extends AlignmentValue
}