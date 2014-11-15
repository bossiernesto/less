package org.uqbar.less.ide.components

import scala.language.reflectiveCalls
import scala.swing.Alignment
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position.Center
import scala.swing.BorderPanel.Position.East
import scala.swing.BorderPanel.Position.South
import scala.swing.BorderPanel.Position.West
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.CheckBox
import scala.swing.Dialog
import scala.swing.Dimension
import scala.swing.FlowPanel
import scala.swing.Label
import scala.swing.ListView.IntervalMode.Single
import scala.swing.Orientation.Horizontal
import scala.swing.Orientation.Vertical
import scala.swing.ScrollPane
import scala.swing.Slider
import scala.swing.TextField
import scala.swing.Window
import scala.swing.event.ButtonClicked
import scala.swing.event.KeyTyped
import scala.swing.event.SelectionChanged
import scala.swing.event.ValueChanged
import org.uqbar.less.encoder.BooleanPreference
import org.uqbar.less.encoder.EncodingPreference
import org.uqbar.less.encoder.IntPreference
import org.uqbar.less.encoder.PreferenceFixture
import org.uqbar.less.encoder.StringPreference
import scala.swing.ComboBox
import scala.swing.ListView

object ConfigModal {

	def apply(parent: Window, preferenceFixture: PreferenceFixture) = {
		val modal = new ConfigModal(parent, preferenceFixture.copy())
		modal.centerOnScreen
		modal.open
		Option(modal.preferenceFixture)
	}

	//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════
	// MODAL CLASS
	//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════

	protected class ConfigModal(parent: Window, var preferenceFixture: PreferenceFixture) extends Dialog(parent) {
		modal = true
		resizable = false

		contents = new BorderPanel {
			val categories = new ListView(preferenceFixture.categories) {
				selection.intervalMode = Single
			}
			val preferenceDisplay = new BoxPanel(Vertical) {
				listenTo(categories.selection)
				reactions += { case SelectionChanged(`categories`) => setContent }

				protected def setContent {
					val category = categories.listData(categories.selection.leadIndex)
					val preferences = preferenceFixture.categoryPreferences(category)

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
								preferenceInput(preference, value, (p, v) => preferenceFixture = preferenceFixture.updated(p, v))
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
					Button("Cancel"){ preferenceFixture = null; close },
					Button("Accept"){ close }
				), East)
			}, South)
		}

		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
		// GUI BUILD
		//─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

		protected def preferenceInput(p: EncodingPreference, currentValue: Any, setPreference: (EncodingPreference, Any) => Unit) = p match {

			case b: BooleanPreference => new CheckBox {
				selected = b(currentValue)
				reactions += { case _: ButtonClicked => setPreference(p, selected) }
			}

			case i @ IntPreference(_, _, None) => new TextField {
				text = i(currentValue).toString
				maximumSize = new Dimension(Int.MaxValue, 28)
				preferredSize = new Dimension(250, 28)
				listenTo(keys)
				reactions += {
					case _: ValueChanged => setPreference(p, text.toInt)
					case e: KeyTyped if !e.char.isDigit => e.consume
				}
			}

			case i @ IntPreference(_, _, Some(range)) => new Slider {
				labels = (range.min to range.max).map(n => n -> new Label(n.toString)).toMap
				paintLabels = true
				min = range.min
				max = range.max
				value = i(currentValue)
				reactions += { case _: ValueChanged => setPreference(p, value) }
			}

			case s @ StringPreference(_, _, None) => new TextField {
				text = s(currentValue)
				maximumSize = new Dimension(Int.MaxValue, 28)
				preferredSize = new Dimension(250, 28)
				reactions += { case _: ValueChanged => setPreference(p, text) }
			}

			case s @ StringPreference(_, _, Some(values)) => new ComboBox(values) {
				selection.index = values.indexOf(currentValue)
				listenTo(selection)
				reactions += { case _: SelectionChanged => setPreference(p, selection.item) }
			}
		}

	}
}
