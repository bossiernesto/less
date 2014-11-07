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
	val styles = """
		h1{color: red;}
	"""

	def top = new MainFrame {
		title = "Less IDE"

		contents = new BoxPanel(Orientation.Vertical) {
			contents += new MenuBar {
				contents ++= Seq(menuButton("Open", "/icons/open.gif"){ ??? },
					menuButton("Save", "/icons/save.gif"){ ??? },
					menuButton("Refresh", "/icons/refresh.gif"){ ??? },
					menuButton("Run", "/icons/run.gif"){ ??? },
					HGlue
				)
			}

			contents += new EditorPane { //("text/html", s"<style>$styles</style><body> </body>") {
				reactions += {
					case _: ValueChanged =>
						val r = Parse(text)
						if (r.successful) background = Color.green else background = Color.red
				}
			}
		}

		size = new Dimension(800, 600)
		centerOnScreen
	}

	protected def menuButton(name: String, imagePath: String = "")(action: => Unit) = new Button(Action("")(action)) {
		tooltip = name
		if (imagePath.nonEmpty) icon = new ImageIcon(getClass.getResource(imagePath))
	}
}