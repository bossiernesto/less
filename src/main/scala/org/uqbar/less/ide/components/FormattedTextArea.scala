package org.uqbar.less.ide.components

import java.awt.Color
import java.awt.Font
import java.awt.Font.BOLD
import java.awt.Font.ITALIC
import java.awt.Font.PLAIN

import scala.collection.JavaConversions.enumerationAsScalaIterator
import scala.language.implicitConversions
import scala.swing.TextComponent
import scala.swing.event.ValueChanged
import org.uqbar.less.ide.components.StyleAttribute._
import org.uqbar.less.ide.components.AlignmentValue._
import javax.swing.JTextPane
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.DefaultStyledDocument
import javax.swing.text.SimpleAttributeSet
import javax.swing.text.StyleConstants.ALIGN_CENTER
import javax.swing.text.StyleConstants.ALIGN_JUSTIFIED
import javax.swing.text.StyleConstants.ALIGN_LEFT
import javax.swing.text.StyleConstants.ALIGN_RIGHT
import javax.swing.text.StyleConstants.{ Alignment => JAlignment }
import javax.swing.text.StyleConstants.{ Background => JBackground }
import javax.swing.text.StyleConstants.{ Bold => JBold }
import javax.swing.text.StyleConstants.{ FirstLineIndent => JFirstLineIndent }
import javax.swing.text.StyleConstants.{ FontFamily => JFontFamily }
import javax.swing.text.StyleConstants.{ FontSize => JFontSize }
import javax.swing.text.StyleConstants.{ Foreground => JForeground }
import javax.swing.text.StyleConstants.{ Italic => JItalic }
import javax.swing.text.StyleConstants.{ LeftIndent => JLeftIndent }
import javax.swing.text.StyleConstants.{ LineSpacing => JLineSpacing }
import javax.swing.text.StyleConstants.{ RightIndent => JRightIndent }
import javax.swing.text.StyleConstants.{ SpaceAbove => JSpaceAbove }
import javax.swing.text.StyleConstants.{ SpaceBelow => JSpaceBelow }
import javax.swing.text.StyleConstants.{ StrikeThrough => JStrikeThrough }
import javax.swing.text.StyleConstants.{ Subscript => JSubscript }
import javax.swing.text.StyleConstants.{ Superscript => JSuperscript }
import javax.swing.text.StyleConstants.{ TabSet => JTabSet }
import javax.swing.text.StyleConstants.{ Underline => JUnderline }
import javax.swing.text.StyleContext.DEFAULT_STYLE
import javax.swing.text.StyledEditorKit
import javax.swing.text.{ TabSet => TTabSet }
import javax.swing.text.TabStop

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

		for (attribute <- attributes) newStyle.addAttribute(attribute.key, attribute match {
			case Alignment(alignment) => alignment.code

			case TabWidth(width) =>
				def attributeValue[T](key: Any) = attributes.find(_.key == `key`).fold[Any](parent.getAttribute(key))(_.value).asInstanceOf[T]
				val fontName: String = attributeValue(JFontFamily)
				val fontSize: Int = attributeValue(JFontSize)
				val bold = if (attributeValue(JBold)) BOLD else PLAIN
				val italic = if (attributeValue(JItalic)) ITALIC else PLAIN
				val charWidth = peer.getFontMetrics(new Font(fontName, bold | italic, fontSize)).charWidth(' ')

				new TTabSet((0 to 200).map(i => new TabStop(i * charWidth * width)).toArray)

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

		val attributes = new SimpleAttributeSet {
			for (styleName <- styleNames) addAttributes(document.getStyle(styleName.toString))
		}

		document.insertString(text.size, s, attributes)
	}
}

abstract class StyleAttribute(val key: Any) { def value: Any }
object StyleAttribute {
	case class FontFamily(value: String) extends StyleAttribute(JFontFamily)
	case class FontSize(value: Int) extends StyleAttribute(JFontSize)
	case class Bold(value: Boolean = true) extends StyleAttribute(JBold)
	case class Italic(value: Boolean = true) extends StyleAttribute(JItalic)
	case class Underline(value: Boolean = true) extends StyleAttribute(JUnderline)
	case class StrikeThrough(value: Boolean = true) extends StyleAttribute(JStrikeThrough)
	case class Superscript(value: Boolean = true) extends StyleAttribute(JSuperscript)
	case class Subscript(value: Boolean = true) extends StyleAttribute(JSubscript)
	case class FontColor(value: Color) extends StyleAttribute(JForeground)
	case class Background(value: Color) extends StyleAttribute(JBackground)
	case class FirstLineIndent(value: Float) extends StyleAttribute(JFirstLineIndent)
	case class LeftIndent(value: Float) extends StyleAttribute(JLeftIndent)
	case class RightIndent(value: Float) extends StyleAttribute(JRightIndent)
	case class LineSpacing(value: Float) extends StyleAttribute(JLineSpacing)
	case class SpaceAbove(value: Float) extends StyleAttribute(JSpaceAbove)
	case class SpaceBelow(value: Float) extends StyleAttribute(JSpaceBelow)
	case class Alignment(value: AlignmentValue) extends StyleAttribute(JAlignment)
	case class TabWidth(value: Int) extends StyleAttribute(JTabSet)
}

sealed abstract class AlignmentValue(val code: Int)
object AlignmentValue {
	case object Right extends AlignmentValue(ALIGN_RIGHT)
	case object Left extends AlignmentValue(ALIGN_LEFT)
	case object Justified extends AlignmentValue(ALIGN_JUSTIFIED)
	case object Center extends AlignmentValue(ALIGN_CENTER)
}