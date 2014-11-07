package org.uqbar.less.ide

import scala.swing._
import scala.swing.Swing._
import javax.swing.ImageIcon
import javax.swing.UIManager
import scala.swing.event.EditDone
import scala.swing.event.KeyTyped
import scala.swing.event.ValueChanged
import scala.swing.event.Event
import org.uqbar.less.parser.Parse
import java.awt.Color

object LessIDE extends SimpleSwingApplication {

	def top = new MainFrame {
		title = "Less IDE"

		contents = new BoxPanel(Orientation.Vertical) {
			contents += new MenuBar {
				contents ++= Seq(menuButton("Open", "/icons/open.gif"){ ??? },
					menuButton("Save", "/icons/save.gif"){ editor.text = """{\rtf1 {Esto es un texto en {\b negrita}.}}""" },
					menuButton("Refresh", "/icons/refresh.gif"){ ??? },
					menuButton("Run", "/icons/run.gif"){ ??? },
					HGlue
				)
			}

			val editor = new EditorPane("text/rtf", """{\rtf1 {Esto es un texto en {\b negrita}.}}""") {
				println(peer.getDocument.getText(0, peer.getDocument.getLength))
				listenTo(this)
				reactions += {
					case _: ValueChanged =>
						val r = Parse(peer.getDocument.getText(0, peer.getDocument.getLength))
						if (r.successful) background = Color.green else background = Color.red
				}
			}
			contents += editor
		}

		size = new Dimension(800, 600)
		centerOnScreen
	}

	protected def menuButton(name: String, imagePath: String = "")(action: => Unit) = new Button(Action("")(action)) {
		tooltip = name
		if (imagePath.nonEmpty) icon = new ImageIcon(getClass.getResource(imagePath))
	}
}