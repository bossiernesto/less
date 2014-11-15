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
import scala.swing.BorderPanel.Position.East
import scala.swing.BorderPanel.Position.North
import scala.swing.BorderPanel.Position.South
import scala.swing.Component
import scala.swing.Dimension
import scala.swing.FileChooser
import scala.swing.Insets
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.MenuItem
import scala.swing.Orientation.Horizontal
import scala.swing.ScrollPane
import scala.swing.SequentialContainer
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
import org.uqbar.less.obj.eval.Compile
import javax.swing.ImageIcon
import javax.swing.JToolBar
import javax.swing.KeyStroke.getKeyStroke
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.FlowPanel
import scala.swing.event.Key
import scala.swing.Button
import scala.swing.SequentialContainer.Wrapper

object LessIDE extends SimpleSwingApplication {

	val WORKSPACE = new File("/tmp/less-workspace")
	val PREFERENCES_FILE = new File(WORKSPACE.getAbsoluteFile + "/.preferences")

	def top = new MainFrame {

		var file: Option[File] = None

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		val saveAction = action("Save", "ctrl S") { saveToFile }
		val openAction = action("Open", "ctrl O"){ openFile }
		val configAction = action("Config", "ctrl F"){ config }
		val runAction = action("Run", "ctrl P"){ runInConsole }
		val refreshAction = action("Refresh", "ctrl R"){ refresh }

		val console = new Console
		val editor = new Editor

		menuBar = menubar(
			"File" -> saveAction,
			"File" -> openAction,
			"File" -> refreshAction,
			"Options.Preferences" -> configAction,
			"Run" -> runAction
		)

		val mainToolBar = toolBar(
			openAction,
			saveAction,
			refreshAction,
			runAction,
			configAction
		)

		val statusBar = new BorderPanel {
			protected val status = new Label
			protected val okIcon = new ImageIcon(getClass.getResource("/icons/Ok.gif"))
			protected val errorIcon = new ImageIcon(getClass.getResource("/icons/Error.gif"))

			add(status, East)
			ok(true)

			def ok(value: Boolean) = if (value) {
				status.text = "Ok!"
				status.icon = okIcon
			} else {
				status.text = "Error!"
				status.icon = errorIcon
			}
		}

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
		// GUI BUILD
		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		protected def action(name: String, accelerator: String = "")(body: => Unit) = {
			val action = Action(name)(body)
			Option(getClass.getResource(s"/icons/$name.gif")).map(image => action.icon = new ImageIcon(image))
			action.accelerator = Option(getKeyStroke(accelerator))
			action
		}

		protected def toolBar(actions: Action*) = new Component with SequentialContainer.Wrapper {
			override lazy val peer: JToolBar = new JToolBar
			for (action <- actions) peer.add(new Button(action) { text = "" }.peer)
		}

		protected def menubar(entries: (String, Action)*) = new MenuBar {

			for ((path, action) <- entries) {
				def subMenu(name: String, target: Wrapper = this) = target.contents.collectFirst{ case m: Menu if m.text == name => m }
				def getOrCreateMenu(path: Seq[String], target: Wrapper = this): Wrapper = {
					path.headOption.fold(target){ nextName =>
						val next = subMenu(nextName, target).getOrElse{
							val menu = new Menu(nextName)
							target.contents += menu
							menu
						}

						getOrCreateMenu(path.tail, next)
					}
				}

				getOrCreateMenu(path.split('.')).contents += new MenuItem(action)
			}
		}

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		contents = new BorderPanel {
			add(new FlowPanel(
				mainToolBar
			), North)

			add(new SplitPane(Horizontal, new ScrollPane(editor), new ScrollPane(console)), Center)

			add(statusBar, South)
		}

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		listenTo(editor)
		reactions += {
			case EditorParsed(`editor`, successful) =>
				statusBar.ok(successful)
				saveAction.enabled = successful
				runAction.enabled = successful
		}

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		title = "Less IDE"
		size = new Dimension(800, 600)
		centerOnScreen

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
		// ACTIONS
		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		protected def openFile {
			val chooser = new FileChooser(WORKSPACE) { title = "Open..." }
			chooser.showOpenDialog(null)
			file = Option(chooser.selectedFile)
			console.log('info)(s"File opened: $file")

			refresh
		}

		protected def saveToFile {
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

			refresh
		}

		protected def refresh {
			file.foreach{ file =>
				val in = new ObjectInputStream(new FileInputStream(file))
				editor.text = Encode(in.readObject.asInstanceOf[Seq[Sentence]]: _*)
				in.close
				console.log('info)(s"File refreshed: $file")
			}
		}

		protected def config = ConfigModal(this, preferenceFixture).map(savePreferences)

		protected def runInConsole: Unit = console log Eval()(Compile(editor.parse.get): _*)

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

			refresh
		}

		protected def loadPreferences = {
			val in = new ObjectInputStream(new FileInputStream(PREFERENCES_FILE))
			preferenceFixture = in.readObject.asInstanceOf[PreferenceFixture]
			in.close
			console.log('info)(s"Preferences loaded from: $PREFERENCES_FILE")

			preferenceFixture
		}
	}
}