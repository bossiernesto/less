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
import scala.swing.event.SelectionChanged
import scala.swing.event.ListSelectionChanged
import ListView.IntervalMode._
import javax.swing.border._
import scala.swing.event.EditDone
import java.text.NumberFormat
import javax.swing.InputVerifier
import scala.swing.event.KeyTyped
import scala.swing.event.ButtonClicked

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

		val editor = new EditorPane { //("text/rtf", """{\rtf1 }""") {
			preferredSize = new Dimension(800, 400)

			reactions += {
				case _: ValueChanged =>
					val parseResult = parse
					statusBar.ok(parseResult.successful)
					saveAction.enabled = parseResult.successful
					runAction.enabled = parseResult.successful
			}

			def parse = Parse(peer.getDocument.getText(0, peer.getDocument.getLength))
		}

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

		title = "Less IDE"
		size = new Dimension(800, 600)
		centerOnScreen

		//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════
		// GUI BUILD
		//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════

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

		protected def preferenceInput(p: Preference[_], currentValue: Any, setPreference: (Preference[_], Any) => Unit) = p match {

			case b: BooleanPreference => new CheckBox {
				selected = b(currentValue)
				maximumSize = new Dimension(Int.MaxValue, 28)
				preferredSize = new Dimension(250, 28)
				reactions += { case _: ButtonClicked => setPreference(p, selected) }
			}

			case i @ IntPreference(_, _, _, None) => new TextField {
				text = i(currentValue).toString
				maximumSize = new Dimension(Int.MaxValue, 28)
				preferredSize = new Dimension(250, 28)
				listenTo(keys)
				reactions += {
					case _: ValueChanged => setPreference(p, text.toInt)
					case e: KeyTyped if !e.char.isDigit => e.consume
				}
			}

			case i @ IntPreference(_, _, _, Some(range)) => new Slider {
				labels = (range.min to range.max).map(n => n -> new Label(n.toString)).toMap
				paintLabels = true
				min = range.min
				max = range.max
				value = i(currentValue)
				maximumSize = new Dimension(Int.MaxValue, 32)
				preferredSize = new Dimension(250, 32)
				reactions += { case _: ValueChanged => setPreference(p, value) }
			}

			case s @ StringPreference(_, _, _, None) => new TextField {
				text = s(currentValue)
				maximumSize = new Dimension(Int.MaxValue, 28)
				preferredSize = new Dimension(250, 28)
				reactions += { case _: ValueChanged => setPreference(p, text) }
			}

			case s @ StringPreference(_, _, _, Some(values)) => new ComboBox(values) {
				selection.index = values.indexOf(currentValue)
				maximumSize = new Dimension(Int.MaxValue, 28)
				preferredSize = new Dimension(250, 28)
				listenTo(selection)
				reactions += { case _: SelectionChanged => setPreference(p, selection.item) }
			}
		}

		//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════
		// ACTIONS
		//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════

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

		protected def config {
			new Dialog(this) {
				modal = true
				resizable = false

				var updatedPreferenceFixture = preferenceFixture.copy()

				contents = new BorderPanel {
					val categories = new ListView(updatedPreferenceFixture.categories) {
						selection.intervalMode = Single
					}
					val preferenceDisplay = new BoxPanel(Vertical) {
						listenTo(categories.selection)
						reactions += { case SelectionChanged(`categories`) => setContent }

						protected def setContent {
							val category = categories.listData(categories.selection.leadIndex)
							val preferences = updatedPreferenceFixture.preferences.filter(_._1.category == category)

							contents.clear
							for ((preference, value) <- preferences) {
								contents += new BoxPanel(Horizontal) {
									xLayoutAlignment = 0
									contents ++= Seq(
										new Label(preference.name + ": ") {
											horizontalAlignment = Alignment.Right
											minimumSize = new Dimension(250, 0)
											maximumSize = new Dimension(250, 28)
											preferredSize = maximumSize
										},
										preferenceInput(preference, value, (p: Preference[_], v: Any) => updatedPreferenceFixture = updatedPreferenceFixture.updated(p, v))
									)
								}
							}
							revalidate
							repaint
						}
					}

					minimumSize = new Dimension(600, 500)
					maximumSize = new Dimension(600, 500)
					preferredSize = maximumSize

					add(new ScrollPane(categories), West)
					add(new ScrollPane(preferenceDisplay), Center)
					categories.selectIndices(0)

					add(new BorderPanel {
						add(new FlowPanel(
							Button("Cancel"){ close },
							Button("Accept"){ savePreferences(updatedPreferenceFixture); refresh; close }
						), East)
					}, South)
				}

				centerOnScreen
				open
			}
		}

		protected def runInConsole {
			console log Eval()(Compile(editor.parse.get): _*)
		}
	}

	//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════
	// PREFERENCES
	//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════

	val preferences = Seq(
		IntPreference("Tabulation Size", "Tabulation", 2, Some(1 to 12)),
		BooleanPreference("Tabulate with Tabs", "Tabulation", true),
		BooleanPreference("After if", "Spacing", false),
		BooleanPreference("After else", "Spacing", false),
		BooleanPreference("After while", "Spacing", false),
		BooleanPreference("After object name", "Spacing", false),
		BooleanPreference("After method name", "Spacing", false),
		BooleanPreference("After method arguments", "Spacing", false),
		BooleanPreference("After message arguments", "Spacing", false),
		BooleanPreference("Before message arguments", "Spacing", false),
		BooleanPreference("Before each message argument", "Spacing", false),
		BooleanPreference("After each message argument", "Spacing", true)
	)

	implicit var preferenceFixture: PreferenceFixture = _
	if (PREFERENCES_FILE.exists) loadPreferences
	else savePreferences(PreferenceFixture(preferences.map(p => p -> p.default).toMap))

	protected def savePreferences(fixture: PreferenceFixture) {
		val out = new ObjectOutputStream(new FileOutputStream(PREFERENCES_FILE))
		preferenceFixture = fixture
		out.writeObject(preferenceFixture)
		out.close
	}

	protected def loadPreferences {
		val in = new ObjectInputStream(new FileInputStream(PREFERENCES_FILE))
		preferenceFixture = in.readObject.asInstanceOf[PreferenceFixture]
		in.close
	}

}
