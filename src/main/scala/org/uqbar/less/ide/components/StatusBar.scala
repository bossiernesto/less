package org.uqbar.less.ide.components

import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position.East
import scala.swing.Label
import javax.swing.ImageIcon

class StatusBar extends BorderPanel {
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