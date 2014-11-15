package org.uqbar.less.ide.components

import scala.swing.Action
import scala.swing.{ MenuBar => SMenuBar }
import scala.swing.SequentialContainer.Wrapper
import scala.swing.MenuItem
import scala.swing.Menu

class MenuBar(entries: (String, Action)*) extends SMenuBar {
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
