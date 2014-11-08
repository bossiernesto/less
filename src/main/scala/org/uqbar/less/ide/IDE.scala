package org.uqbar.less.ide

import scala.language.reflectiveCalls
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
import java.io.File
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import java.io.FileInputStream
import org.uqbar.less.encoder.Encode
import org.uqbar.less.SemanticModel._
import javax.swing.KeyStroke
import javax.swing.KeyStroke._
import java.awt.event.KeyEvent._
import java.awt.event.ActionEvent._
import javax.swing.JComponent
import javax.swing.JToolBar
import BorderPanel.Position._
import Orientation._
import scala.swing.Action
import org.uqbar.less.eval.Eval
import org.uqbar.less.obj.eval.Compile
import java.util.Date
import java.util.Calendar
import java.text.SimpleDateFormat
import org.uqbar.less.eval.State
import scala.collection.mutable.StringBuilder

object LessIDE extends SimpleSwingApplication {

	def top = new MainFrame {

		var file: Option[File] = None

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		val editor = new EditorPane { //("text/rtf", """{\rtf1 }""") {
			preferredSize = new Dimension(800, 400)

			reactions += {
				case _: ValueChanged => if (parse.successful) background = Color.green else background = Color.red
			}

			def parse = Parse(peer.getDocument.getText(0, peer.getDocument.getLength))
		}

		val console = new EditorPane {
			editable = false
			background = Color.lightGray.brighter

			protected def now = new SimpleDateFormat("HH:mm:ss").format(new Date)
			def log(s: String) = text += s"$now >> $s"
			def log(s: State) {
				val buffer = new StringBuilder("------------------------------------")
				buffer ++= s"\n  stack: ${s.stack.mkString("[", ",", "]")}"
				buffer ++= s"\n  locals:"
				for ((k, v) <- s.locals) buffer ++= s"\n    $k: $v"
				buffer ++= s"\n  memory:"
				for ((k, v) <- s.memory.value) buffer ++= s"\n    $k: $v"

				log(buffer.toString)
			}
		}

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		contents = new BorderPanel {
			add(toolBar(
				menuButton("Open", "/icons/open.gif", "ctrl O"){ openFile },
				menuButton("Save", "/icons/save.gif", "ctrl S") { saveToFile },
				menuButton("Refresh", "/icons/refresh.gif", "ctrl R"){ refresh },
				menuButton("Run", "/icons/run.gif", "ctrl P"){ runInConsole }
			), North)

			add(new SplitPane(Horizontal, new ScrollPane(editor), new ScrollPane(console)), Center)
		}

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		title = "Less IDE"
		size = new Dimension(800, 600)
		centerOnScreen

		//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════
		// GUI BUILD
		//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════

		protected def menuButton(name: String, imagePath: String = "", keystroke: String = "")(action: => Unit) = new MenuItem(Action("")(action)) {
			tooltip = name
			if (imagePath.nonEmpty) icon = new ImageIcon(getClass.getResource(imagePath)) else action.title = name
			action.accelerator = Option(getKeyStroke(keystroke))
			margin = new Insets(0, 0, 0, 20)
		}

		protected def toolBar(components: Component*) = new Component with SequentialContainer.Wrapper {
			override lazy val peer: JToolBar = new JToolBar
			for (component <- components) peer.add(component.peer)
			maximumSize = new Dimension(34 * components.size, 30)
			preferredSize = maximumSize
		}

		//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════
		// ACTIONS
		//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════

		protected def openFile {
			val chooser = new FileChooser { title = "Open..." }
			chooser.showOpenDialog(null)
			file = Option(chooser.selectedFile)

			refresh
		}

		protected def saveToFile {
			file = file.fold {
				val chooser = new FileChooser { title = "Save as..." }
				chooser.showOpenDialog(null)
				Option(chooser.selectedFile)
			}{ _ => file }

			file.foreach{ file =>
				val out = new ObjectOutputStream(new FileOutputStream(file))
				out.writeObject(editor.parse.get)
				out.close
			}
		}

		protected def refresh {
			file.foreach{ file =>
				val in = new ObjectInputStream(new FileInputStream(file))
				editor.text = Encode(in.readObject.asInstanceOf[Seq[Sentence]]: _*)
				in.close
			}
		}

		protected def runInConsole {
			console log Eval()(Compile(editor.parse.get): _*)
		}
	}

}
