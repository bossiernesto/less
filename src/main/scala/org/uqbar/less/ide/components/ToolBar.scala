package org.uqbar.less.ide.components

import scala.swing.Component
import scala.swing.SequentialContainer.Wrapper
import javax.swing.JToolBar
import scala.swing.Button
import scala.swing.Action

class ToolBar(actions: Action*) extends Component with Wrapper {
	override lazy val peer: JToolBar = new JToolBar
	for (action <- actions) peer.add(new Button(action) { text = "" }.peer)
}