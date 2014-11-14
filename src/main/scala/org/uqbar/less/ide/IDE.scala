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
import javax.swing.ImageIcon
import javax.swing.JToolBar
import javax.swing.KeyStroke.getKeyStroke
import javax.swing.text.{ TabSet => JTabSet }
import javax.swing.text.TabStop
import scala.swing.ComboBox
import scala.swing.ListView
import org.uqbar.less.ide.components._
import org.uqbar.less.ide.components.StyleAttribute._

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

		val editor = new FormattedTextArea {

			preferredSize = new Dimension(800, 400)

			val fontName = "Ubuntu Mono"
			val fontSize = 15
			val charWidth = new Font(fontName, Font.PLAIN, fontSize).getStringBounds("w", new FontRenderContext(new AffineTransform, true, true)).getWidth.toInt

			defineStyle('default)(
				FontFamily(fontName),
				FontSize(fontSize),
				TabSet(new JTabSet((1 to 100).map(i => new TabStop(i * charWidth * 4)).toArray))
			)

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

		val console = new FormattedTextArea {
			editable = false
			background = Color.lightGray.brighter

			val fontName = "Monospace"
			val fontSize = 10
			val charWidth = new Font(fontName, Font.PLAIN, fontSize).getStringBounds("w", new FontRenderContext(new AffineTransform, true, true)).getWidth.toInt

			defineStyle('default)(
				FontFamily(fontName),
				FontSize(fontSize),
				TabSet(new JTabSet((1 to 100).map(i => new TabStop(i * charWidth * 2)).toArray))
			)

			defineStyle('info, 'default)(FontColor(BLUE))
			defineStyle('warning, 'default)(FontColor(YELLOW.darker))
			defineStyle('error, 'default)(FontColor(RED))
			defineStyle('ok, 'default)(FontColor(GREEN.darker))
			defineStyle('header, 'default)(Italic(true))

			protected def now = new SimpleDateFormat("HH:mm:ss").format(new Date)

			def log(style: Symbol = 'default)(s: String) = {
				write(style, 'header)(s"$now >> ")
				write(style)(s"$s\n")
			}
			def log(s: State) {
				val buffer = new StringBuilder("Execution result:")
				buffer ++= s"\n\tstack: ${s.stack.mkString("[", ",", "]")}"
				buffer ++= s"\n\tlocals:"
				for ((k, v) <- s.locals) buffer ++= s"\n\t\t$k: $v"
				buffer ++= s"\n\tmemory:"
				for ((k, v) <- s.memory.value) buffer ++= s"\n\t\t$k: $v"

				log('ok)(buffer.toString)
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
		BooleanPreference("Before if condition", "Spacing", false),
		BooleanPreference("After if condition", "Spacing", false),
		BooleanPreference("After if argument", "Spacing", true),
		BooleanPreference("Before else", "Spacing", false),
		BooleanPreference("After else", "Spacing", false),
		BooleanPreference("After while", "Spacing", false),
		BooleanPreference("Before while condition", "Spacing", false),
		BooleanPreference("After while condition", "Spacing", false),
		BooleanPreference("After while argument", "Spacing", true),
		BooleanPreference("After not", "Spacing", false),
		BooleanPreference("After object name", "Spacing", false),
		BooleanPreference("After method name", "Spacing", false),
		BooleanPreference("After method arguments", "Spacing", false),
		BooleanPreference("Before each method argument", "Spacing", false),
		BooleanPreference("After each method argument", "Spacing", true),
		BooleanPreference("Before each array argument", "Spacing", false),
		BooleanPreference("After each array argument", "Spacing", true),
		BooleanPreference("Before assign", "Spacing", true),
		BooleanPreference("After assign", "Spacing", true),
		BooleanPreference("Before operator", "Spacing", true),
		BooleanPreference("After operator", "Spacing", true),
		BooleanPreference("Before array index", "Spacing", false),
		BooleanPreference("After array index", "Spacing", false),
		BooleanPreference("After message name", "Spacing", false),
		BooleanPreference("Before each message argument", "Spacing", false),
		BooleanPreference("After each message argument", "Spacing", true)
	)

	implicit var preferenceFixture: PreferenceFixture = _

	val defaultPreferenceFixture = PreferenceFixture(preferences.map(p => p -> p.default).toMap)

	if (!WORKSPACE.exists) WORKSPACE.mkdirs
	if (PREFERENCES_FILE.exists) {
		loadPreferences
		if (preferenceFixture.preferences.size != preferences.size) savePreferences(defaultPreferenceFixture)
	} else savePreferences(defaultPreferenceFixture)

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
