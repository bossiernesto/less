package org.uqbar.less.ide.components

import java.awt.Color
import java.awt.Color.BLUE
import java.awt.Color.GREEN
import java.awt.Color.RED
import java.awt.Color.YELLOW
import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.mutable.StringBuilder
import scala.language.reflectiveCalls

import org.uqbar.less.eval.State
import org.uqbar.less.ide.components.StyleAttribute.FontColor
import org.uqbar.less.ide.components.StyleAttribute.FontFamily
import org.uqbar.less.ide.components.StyleAttribute.FontSize
import org.uqbar.less.ide.components.StyleAttribute.Italic
import org.uqbar.less.ide.components.StyleAttribute.TabWidth

class Console extends FormattedTextArea {

	editable = false
	background = Color.lightGray.brighter

	//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
	// STYLES
	//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

	defineStyle('default)(
		FontFamily("Monospace"),
		FontSize(10),
		TabWidth(4)
	)

	defineStyle('info, 'default)(FontColor(BLUE))
	defineStyle('warning, 'default)(FontColor(YELLOW.darker))
	defineStyle('error, 'default)(FontColor(RED))
	defineStyle('ok, 'default)(FontColor(GREEN.darker))
	defineStyle('header, 'default)(Italic())

	//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
	// LOGGING
	//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

	def log(style: Symbol = 'default)(s: String) = {
		write(style, 'header)(s"[${new SimpleDateFormat("HH:mm:ss").format(new Date)}]: ")
		write(style)(s"$s\n")
	}

	def log(s: State): Unit = log('ok){
		val buffer = new StringBuilder("Execution result:")
		buffer ++= s"\n\tstack: ${s.stack.mkString("[", ",", "]")}"
		buffer ++= s"\n\tlocals:"
		for ((k, v) <- s.locals) buffer ++= s"\n\t\t$k: $v"
		buffer ++= s"\n\tmemory:"
		for ((k, v) <- s.memory.value) buffer ++= s"\n\t\t$k: $v"
		buffer.toString
	}
}