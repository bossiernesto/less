package org.uqbar.less.ide.components

import scala.swing.Action
import scala.swing.{ MenuBar => SMenuBar }
import scala.swing.SequentialContainer.Wrapper
import scala.swing.MenuItem
import scala.swing.Menu
import scala.swing.event.Key

class MenuBar(entries: (String, Action)*) extends SMenuBar {
	for ((path, action) <- entries) {
		def subMenu(name: String, target: Wrapper = this) = target.contents.collectFirst{ case m: Menu if m.text == name => m }
		def getOrCreateMenu(path: Seq[String], target: Wrapper = this): Wrapper = {
			path.headOption.fold(target){ nextName =>
				val cleanNextName = nextName.replace("_", "")
				val mnemonicIndex = nextName.indexOf('_')
				val next = subMenu(cleanNextName, target).getOrElse{
					val menu = new Menu(cleanNextName) { if (mnemonicIndex >= 0) mnemonic = Key(cleanNextName(mnemonicIndex)) }
					target.contents += menu
					menu
				}

				getOrCreateMenu(path.tail, next)
			}
		}

		getOrCreateMenu(path.split('.')).contents += new MenuItem(action)
	}
}
