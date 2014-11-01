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

			"arrays" - {

				"at" in {
					implicit val parser = arrayAt

					""" #[] """ should beParsedTo (A(Seq()))
					""" #[1] """ should beParsedTo (A(Seq(N(1))))
					""" #[1,2,3] """ should beParsedTo (A(Seq(N(1), N(2), N(3))))
					""" #[1,1+1,3] """ should beParsedTo (A(Seq(N(1), Add(N(1), N(1)), N(3))))
					""" #[#[1,2],#[3,4]] """ should beParsedTo (A(Seq(A(Seq(N(1), N(2))), A(Seq(N(3), N(4))))))
					""" #[#[]] """ should beParsedTo (A(Seq(A(Seq()))))
					""" #[#[],#[]] """ should beParsedTo (A(Seq(A(Seq()), A(Seq()))))

					""" #[1,2,3][4] """ should beParsedTo (At(A(Seq(N(1), N(2), N(3))), N(4)))
					""" #[1,2,3][4 + 5] """ should beParsedTo (At(A(Seq(N(1), N(2), N(3))), Add(N(4), N(5))))

					""" #[#[1,2]][0][1] """ should beParsedTo (At(At(A(Seq(A(Seq(N(1), N(2))))), N(0)), N(1)))

				}

				"put" in {
					implicit val parser = arrayPut

					""" #[1,2,3][4] = 5 + 6; """ should beParsedTo (Put(A(Seq(N(1), N(2), N(3))), N(4), Add(N(5), N(6))))
				}

			}

			"aritmethic expressions" in {
				implicit val parser = expression

				" 2 && 3 " should beParsedTo (And(N(2), N(3)))
				" 2 && 3 && 4" should beParsedTo (And(And(N(2), N(3)), N(4)))
				" (2 && 3) && 4" should beParsedTo (And(And(N(2), N(3)), N(4)))
				" 2 && (3 && 4)" should beParsedTo (And(N(2), And(N(3), N(4))))

				" 2 || 3 " should beParsedTo (Or(N(2), N(3)))
				" 2 || 3 || 4" should beParsedTo (Or(Or(N(2), N(3)), N(4)))
				" (2 || 3) || 4" should beParsedTo (Or(Or(N(2), N(3)), N(4)))
				" 2 || (3 || 4)" should beParsedTo (Or(N(2), Or(N(3), N(4))))

				" 2 + 3 " should beParsedTo (Add(N(2), N(3)))
				" 2 + 3 + 4" should beParsedTo (Add(Add(N(2), N(3)), N(4)))
				" (2 + 3) + 4" should beParsedTo (Add(Add(N(2), N(3)), N(4)))
				" 2 + (3 + 4)" should beParsedTo (Add(N(2), Add(N(3), N(4))))

				" 2 * 3 " should beParsedTo (Mul(N(2), N(3)))
				" 2 * 3 * 4" should beParsedTo (Mul(Mul(N(2), N(3)), N(4)))
				" (2 * 3) * 4" should beParsedTo (Mul(Mul(N(2), N(3)), N(4)))
				" 2 * (3 * 4)" should beParsedTo (Mul(N(2), Mul(N(3), N(4))))

				" 2 / 3 " should beParsedTo (Div(N(2), N(3)))
				" 2 / 3 / 4" should beParsedTo (Div(Div(N(2), N(3)), N(4)))
				" (2 / 3) / 4" should beParsedTo (Div(Div(N(2), N(3)), N(4)))
				" 2 / (3 / 4)" should beParsedTo (Div(N(2), Div(N(3), N(4))))

				" 2 == 3 " should beParsedTo (Eq(N(2), N(3)))
				" 2 == 3 + 4" should beParsedTo (Eq(N(2), Add(N(3), N(4))))
				" 3 + 4 == 2" should beParsedTo (Eq(Add(N(3), N(4)), N(2)))

				" 2 != 3 " should beParsedTo (Not(Eq(N(2), N(3))))
				" 2 != 3 + 4" should beParsedTo (Not(Eq(N(2), Add(N(3), N(4)))))
				" 3 + 4 != 2" should beParsedTo (Not(Eq(Add(N(3), N(4)), N(2))))

				" 2 > 3 " should beParsedTo (Greater(N(2), N(3)))
				" 2 > 3 + 4" should beParsedTo (Greater(N(2), Add(N(3), N(4))))
				" 3 + 4 > 2" should beParsedTo (Greater(Add(N(3), N(4)), N(2)))

				" 2 >= 3 " should beParsedTo (GreaterOrEq(N(2), N(3)))
				" 2 >= 3 + 4" should beParsedTo (GreaterOrEq(N(2), Add(N(3), N(4))))
				" 3 + 4 >= 2" should beParsedTo (GreaterOrEq(Add(N(3), N(4)), N(2)))

				" 2 < 3 " should beParsedTo (Lesser(N(2), N(3)))
				" 2 < 3 + 4" should beParsedTo (Lesser(N(2), Add(N(3), N(4))))
				" 3 + 4 < 2" should beParsedTo (Lesser(Add(N(3), N(4)), N(2)))

				" 2 <= 3 " should beParsedTo (LesserOrEq(N(2), N(3)))
				" 2 <= 3 + 4" should beParsedTo (LesserOrEq(N(2), Add(N(3), N(4))))
				" 3 + 4 <= 2" should beParsedTo (LesserOrEq(Add(N(3), N(4)), N(2)))

				" ! 2 > 3 " should beParsedTo (Not(Greater(N(2), N(3))))
				" ! 2 != 3 " should beParsedTo (Not(Not(Eq(N(2), N(3)))))
				" 2 != 3 && ! 5 > 7 " should beParsedTo (And(Not(Eq(N(2), N(3))), Not(Greater(N(5), N(7)))))
				" 2 != 3 && ! 5 > 7 || 6 < 3 " should beParsedTo (Or(And(Not(Eq(N(2), N(3))), Not(Greater(N(5), N(7)))), Lesser(N(6), N(3))))

				" 2 * 3 + 4" should beParsedTo (Add(Mul(N(2), N(3)), N(4)))
				" (2 * 3) + 4" should beParsedTo (Add(Mul(N(2), N(3)), N(4)))
				" 2 * (3 + 4)" should beParsedTo (Mul(N(2), Add(N(3), N(4))))
				" 4 + 2 * 3" should beParsedTo (Add(N(4), Mul(N(2), N(3))))
				" 4 + (2 * 3)" should beParsedTo (Add(N(4), Mul(N(2), N(3))))
				" (4 + 2) * 3" should beParsedTo (Mul(Add(N(4), N(2)), N(3)))

				" 10 + x == y * 5 + 2 * 3" should beParsedTo (Eq(Add(N(10), R('x)), Add(Mul(R('y), N(5)), Mul(N(2), N(3)))))
				" 10 + -x == y * (5 + -2) * 3" should beParsedTo (Eq(Add(N(10), Mul(N(-1), R('x))), Mul(Mul(R('y), Add(N(5), N(-2))), N(3))))
				" 1 * 2 + 3 / 4 != 5 - 6 && 7 - 8 >= -9 * (2 + 3)" should beParsedTo (And(Not(Eq(Add(Mul(N(1), N(2)), Div(N(3), N(4))), Sub(5, 6))), GreaterOrEq(Sub(N(7), N(8)), Mul(N(-9), Add(N(2), N(3))))))
			}

			"if" in {
				implicit val parser = ifExp

				""" if(x < 3) { x + 4; } """ should beParsedTo (If(Lesser(R('x), N(3)), Add(R('x), N(4)) :: Nil, Nil))
				""" if(x < 3) { x + 4; } else { x - 1; } """ should beParsedTo (If(Lesser(R('x), N(3)), Add(R('x), N(4)) :: Nil, Sub(R('x), N(1)) :: Nil))
			}

			"while" in {
				implicit val parser = whileExp

				""" while(x < 3) { x + 4; x - 1; } """ should beParsedTo (While(Lesser(R('x), N(3)), Add(R('x), N(4)) :: Sub(R('x), N(1)) :: Nil))
			}

			"assign" in {
				implicit val parser = assign

				""" x = y; """ should beParsedTo (Assign('x, R('y)))
				""" x = x - 1; """ should beParsedTo (Assign('x, Sub(R('x), N(1))))
			}

			"message chain" in {
				implicit val parser = messageChain

				""" x """ should beParsedTo (R('x))
				""" x.m() """ should beParsedTo (Send(R('x), 'm, Nil))
				""" x.m(1) """ should beParsedTo (Send(R('x), 'm, N(1) :: Nil))
				""" x.m(1,2) """ should beParsedTo (Send(R('x), 'm, N(1) :: N(2) :: Nil))
				""" x.m(1,2,y.n(z)) """ should beParsedTo (Send(R('x), 'm, N(1) :: N(2) :: Send(R('y), 'n, R('z) :: Nil) :: Nil))
				""" x.m(1).n(y) """ should beParsedTo (Send(Send(R('x), 'm, N(1) :: Nil), 'n, R('y) :: Nil))
			}

			"array length" in {}
			"attribute" - {
				"get" in {}
				"set" in {}
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