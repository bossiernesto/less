package org.uqbar.less.ide

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import scala.language.reflectiveCalls
import scala.swing.Action
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position.Center
import scala.swing.BorderPanel.Position.North
import scala.swing.BorderPanel.Position.South
import scala.swing.Dimension
import scala.swing.FileChooser
import scala.swing.FlowPanel
import scala.swing.MainFrame
import scala.swing.Orientation.Horizontal
import scala.swing.ScrollPane
import scala.swing.SimpleSwingApplication
import scala.swing.SplitPane

import org.uqbar.less.Bytecode._
import org.uqbar.less.SemanticModel.Sentence
import org.uqbar.less.SemanticModel._
import org.uqbar.less.encoder.Encode
import org.uqbar.less.encoder.PreferenceFixture
import org.uqbar.less.eval.Eval
import org.uqbar.less.ide.components.ConfigModal
import org.uqbar.less.ide.components.Console
import org.uqbar.less.ide.components.Editor
import org.uqbar.less.ide.components.EditorParsed
import org.uqbar.less.ide.components.MenuBar
import org.uqbar.less.ide.components.StatusBar
import org.uqbar.less.ide.components.ToolBar
import org.uqbar.less.obj.eval.Compile

import javax.swing.ImageIcon
import javax.swing.KeyStroke.getKeyStroke

object LessIDE extends SimpleSwingApplication {

	val WORKSPACE = new File("/tmp/less-workspace")
	val PREFERENCES_FILE = new File(WORKSPACE.getAbsoluteFile + "/.preferences")

	def top = new MainFrame {

		var file: Option[File] = None

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		menuBar = new MenuBar(
			"_File" -> saveToFile,
			"_File" -> openFile,
			"_File" -> refresh,
			"_Options._Preferences" -> config,
			"_Run" -> run
		)

		val console = new Console

		val editor = new Editor

		val mainToolBar = new ToolBar(
			openFile,
			saveToFile,
			refresh,
			run,
			config
		)

		val statusBar = new StatusBar

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		contents = new BorderPanel {
			add(new FlowPanel(mainToolBar), North)
			add(new SplitPane(Horizontal, new ScrollPane(editor), new ScrollPane(console)), Center)
			add(statusBar, South)
		}

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		listenTo(editor)
		reactions += {
			case EditorParsed(`editor`, successful) =>
				statusBar.ok(successful)
				saveToFile.enabled = successful
				run.enabled = successful
		}

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		title = "Less IDE"
		size = new Dimension(800, 600)
		centerOnScreen

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
		// ACTIONS
		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		lazy val saveToFile = action("Save", "ctrl S") {
			file = file.fold {
				val chooser = new FileChooser(WORKSPACE) { title = "Save as..." }
				chooser.showOpenDialog(null)
				Option(chooser.selectedFile)
			}{ _ => file }

			file.foreach{ file =>
				val out = new ObjectOutputStream(new FileOutputStream(file))
				out.writeObject(editor.parse.get)
				out.close

				console.log('info)(s"File saved: $file")
			}

			refresh()
		}

		lazy val openFile = action("Open", "ctrl O"){
			val chooser = new FileChooser(WORKSPACE) { title = "Open..." }
			chooser.showOpenDialog(null)
			Option(chooser.selectedFile).map{ f =>
				file = Some(f)

				console.log('info)(s"File opened: $file")
			}

			refresh()
		}

		lazy val refresh = action("Refresh", "ctrl R"){
			file.foreach{ file =>
				val in = new ObjectInputStream(new FileInputStream(file))
				editor.text = Encode(in.readObject.asInstanceOf[Seq[Sentence]]: _*)
				in.close

				console.log('info)(s"File refreshed: $file")
			}

			editor.applyPreferences(preferenceFixture)
		}

		lazy val config = action("Config", "ctrl F"){
			ConfigModal(this, preferenceFixture).map(savePreferences)
		}

		lazy val run = action("Run", "ctrl P"){
			console log Eval()(Compile(editor.parse.get): _*)
		}

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
		// PREFERENCES
		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		implicit var preferenceFixture: PreferenceFixture = _

		if (!WORKSPACE.exists) WORKSPACE.mkdirs
		if (!(PREFERENCES_FILE.exists && loadPreferences <=> PreferenceFixture.Default)) {
			savePreferences(PreferenceFixture.Default)
		}

		protected def savePreferences(fixture: PreferenceFixture) {
			val out = new ObjectOutputStream(new FileOutputStream(PREFERENCES_FILE))
			preferenceFixture = fixture
			out.writeObject(preferenceFixture)
			out.close

			console.log('info)(s"Preferences saved to: $PREFERENCES_FILE")

			refresh()
		}

		protected def loadPreferences = {
			val in = new ObjectInputStream(new FileInputStream(PREFERENCES_FILE))
			preferenceFixture = in.readObject.asInstanceOf[PreferenceFixture]
			in.close

			console.log('info)(s"Preferences loaded from: $PREFERENCES_FILE")

			refresh()

			preferenceFixture
		}
	}

	//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
	// AUXILIARS
	//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

	protected def action(name: String, accelerator: String = "")(body: => Unit) = {
		val action = Action(name)(body)
		Option(getClass.getResource(s"/icons/$name.gif")).map(image => action.icon = new ImageIcon(image))
		action.accelerator = Option(getKeyStroke(accelerator))
		action
	}
}