package org.uqbar.less.ide

import java.awt.Color
import java.awt.Color._
import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.mutable.StringBuilder
import scala.language.reflectiveCalls
import scala.swing.Action
import scala.swing.Alignment
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position.Center
import scala.swing.BorderPanel.Position.East
import scala.swing.BorderPanel.Position.North
import scala.swing.BorderPanel.Position.South
import scala.swing.BorderPanel.Position.West
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.CheckBox
import scala.swing.Component
import scala.swing.Dialog
import scala.swing.Dimension
import scala.swing.FileChooser
import scala.swing.FlowPanel
import scala.swing.Insets
import scala.swing.Label
import scala.swing.ListView.IntervalMode.Single
import scala.swing.MainFrame
import scala.swing.MenuItem
import scala.swing.Orientation.Horizontal
import scala.swing.Orientation.Vertical
import scala.swing.ScrollPane
import scala.swing.SequentialContainer
import scala.swing.SimpleSwingApplication
import scala.swing.Slider
import scala.swing.SplitPane
import scala.swing.TextComponent
import scala.swing.TextField
import scala.swing.event.ButtonClicked
import scala.swing.event.KeyTyped
import scala.swing.event.SelectionChanged
import scala.swing.event.ValueChanged
import org.uqbar.less.Bytecode._
import org.uqbar.less.SemanticModel.Sentence
import org.uqbar.less.SemanticModel._
import org.uqbar.less.encoder.Encode
import org.uqbar.less.eval.Eval
import org.uqbar.less.eval.State
import org.uqbar.less.obj.eval.Compile
import org.uqbar.less.parser.Parse
import org.uqbar.less.encoder._
import javax.swing.ImageIcon
import javax.swing.JToolBar
import javax.swing.KeyStroke.getKeyStroke
import javax.swing.text.{ TabSet => JTabSet }
import javax.swing.text.TabStop
import scala.swing.ComboBox
import scala.swing.ListView
import org.uqbar.less.ide.components._
import org.uqbar.less.ide.components.StyleAttribute._
import java.awt.FontMetrics
import org.uqbar.less.encoder.StringPreference
import org.uqbar.less.encoder.PreferenceFixture
import org.uqbar.less.encoder.EncodingPreference
import org.uqbar.less.encoder.IntPreference
import org.uqbar.less.encoder.BooleanPreference

object LessIDE extends SimpleSwingApplication {

	val WORKSPACE = new File("/tmp/less-workspace")
	val PREFERENCES_FILE = new File(WORKSPACE.getAbsoluteFile + "/.preferences")

	def top = new MainFrame {

		var file: Option[File] = None

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		val runAction = menuButton("Run", "ctrl P"){ runInConsole }
		val refreshAction = menuButton("Refresh", "ctrl R"){ refresh }
		val saveAction = menuButton("Save", "ctrl S") { saveToFile }
		val openAction = menuButton("Open", "ctrl O"){ openFile }
		val configAction = menuButton("Config", "ctrl F"){ config }

		val console = new Console
		val editor = new Editor

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

		contents = new BorderPanel {
			add(toolBar(
				openAction,
				saveAction,
				refreshAction,
				runAction,
				configAction
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
		// GUI BUILD
		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		protected def menuButton(itemName: String, keystroke: String = "")(action: => Unit) = new MenuItem(Action("")(action)) {
			Option(getClass.getResource(s"/icons/$itemName.gif")).fold{ action.title = name }{ r => icon = new ImageIcon(r) }
			action.accelerator = Option(getKeyStroke(keystroke))
			margin = new Insets(0, 0, 0, 20)
		}

		protected def toolBar(components: Component*) = new Component with SequentialContainer.Wrapper {
			override lazy val peer: JToolBar = new JToolBar
			for (component <- components) peer.add(component.peer)
			maximumSize = new Dimension(34 * components.size, 30)
			preferredSize = maximumSize
		}

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
		// ACTIONS
		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		protected def openFile {
			val chooser = new FileChooser(WORKSPACE) { title = "Open..." }
			chooser.showOpenDialog(null)
			file = Option(chooser.selectedFile)

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
			}

			refresh
		}

		protected def refresh {
			file.foreach{ file =>
				val in = new ObjectInputStream(new FileInputStream(file))
				editor.text = Encode(in.readObject.asInstanceOf[Seq[Sentence]]: _*)
				in.close
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
			refresh
		}

		protected def loadPreferences = {
			val in = new ObjectInputStream(new FileInputStream(PREFERENCES_FILE))
			preferenceFixture = in.readObject.asInstanceOf[PreferenceFixture]
			in.close
			preferenceFixture
		}
	}
}