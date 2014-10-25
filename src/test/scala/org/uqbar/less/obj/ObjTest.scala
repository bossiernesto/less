package org.uqbar.less.obj

import org.uqbar.less.obj.eval._
import org.uqbar.less.obj.Preety._
import org.scalatest.BeforeAndAfter
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

class ObjTest extends FreeSpec with Matchers with BeforeAndAfter {
	"The program" - {
		"5" - {
			val program = N(5)
		}

		"2 + (3 * 4)" - {
			val program = Add(N(2), Mul(N(3), N(4)))
		}

		"Sum all numbers in [3,7,(6 + 2)]" - {
			val program = Seq(
				Assign('r, 0),
				Assign('i, 0),
				Assign('a, %(3, 7, Add(6, 2))),
				While(Greater(Length('a), 'i), Seq(
					Assign('r, Add('r, At('a, 'i))),
					Assign('i, Add('i, 1))
				))
			)
		}
	}
}