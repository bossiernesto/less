package org.uqbar.less.obj.parse

import org.uqbar.less.obj._
import org.uqbar.less.obj.Preety._
import org.scalatest.BeforeAndAfter
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

class ParseTest extends FreeSpec with Matchers with BeforeAndAfter with Parse {

	"Parser" - {

		"should be able to parse" - {

			"references" in {
				implicit val parser = reference
				""" _ """ should beParsedTo (R('_))
				""" x """ should beParsedTo (R('x))
				""" foo """ should beParsedTo (R('foo))
				""" foo_bar """ should beParsedTo (R('foo_bar))
			}

			"numbers" in {
				implicit val parser = number
				""" 0 """ should beParsedTo (N(0))
				""" 192 """ should beParsedTo (N(192))
				""" -10 """ should beParsedTo (N(-10))
			}

			"arrays" in {
				implicit val parser = array
				""" [] """ should beParsedTo (A(Seq()))
				""" [1] """ should beParsedTo (A(Seq(N(1))))
				""" [1,2,3] """ should beParsedTo (A(Seq(N(1), N(2), N(3))))
				""" [[1,2],[3,4]] """ should beParsedTo (A(Seq(A(Seq(N(1), N(2))), A(Seq(N(3), N(4))))))
				""" [[]] """ should beParsedTo (A(Seq(A(Seq()))))
				""" [[],[]] """ should beParsedTo (A(Seq(A(Seq()), A(Seq()))))
			}

			"aritmethic expressions" in {
				implicit val parser = expression
				" 2 + 3 " should beParsedTo (Add(N(2), N(3)))
				" 2 + 3 + 4" should beParsedTo (Add(Add(N(2), N(3)), N(4)))
				" (2 + 3) + 4" should beParsedTo (Add(Add(N(2), N(3)), N(4)))
				" 2 + (3 + 4)" should beParsedTo (Add(N(2), Add(N(3), N(4))))

				" 2 * 3 " should beParsedTo (Mul(N(2), N(3)))
				" 2 * 3 * 4" should beParsedTo (Mul(Mul(N(2), N(3)), N(4)))
				" (2 * 3) * 4" should beParsedTo (Mul(Mul(N(2), N(3)), N(4)))
				" 2 * (3 * 4)" should beParsedTo (Mul(N(2), Mul(N(3), N(4))))

				" 2 == 3 " should beParsedTo (Eq(N(2), N(3)))
				" 2 == 3 == 4" should beParsedTo (Eq(Eq(N(2), N(3)), N(4)))
				" (2 == 3) == 4" should beParsedTo (Eq(Eq(N(2), N(3)), N(4)))
				" 2 == (3 == 4)" should beParsedTo (Eq(N(2), Eq(N(3), N(4))))

				" 2 * 3 + 4" should beParsedTo (Add(Mul(N(2), N(3)), N(4)))
				" (2 * 3) + 4" should beParsedTo (Add(Mul(N(2), N(3)), N(4)))
				" 2 * (3 + 4)" should beParsedTo (Mul(N(2), Add(N(3), N(4))))
				" 4 + 2 * 3" should beParsedTo (Add(N(4), Mul(N(2), N(3))))
				" 4 + (2 * 3)" should beParsedTo (Add(N(4), Mul(N(2), N(3))))
				" (4 + 2) * 3" should beParsedTo (Mul(Add(N(4), N(2)), N(3)))

				" 10 + x == y * 5 + 2 * 3" should beParsedTo (Eq(Add(N(10), R('x)), Add(Mul(R('y), N(5)), Mul(N(2), N(3)))))
			}
		}
	}

	//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
	// MATCHERS
	//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

	case class beParsedTo[T](expected: Sentence)(implicit parser: Parser[T]) extends Matcher[String] {
		def apply(left: String) = {
			val result = parse(parser, left)
			MatchResult(
				result.successful && result.get == expected,
				if (result.successful) s"Parsed ${result.get} did not equal $expected" else s"Parse failed! $result",
				if (result.successful) s"Parsed ${result.get} was equal to $expected" else s"Parse didn't fail! $result"
			)
		}
	}
}