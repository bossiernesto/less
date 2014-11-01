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

				""" _ """ should beParsedTo ('_)
				""" x """ should beParsedTo ('x)
				""" foo """ should beParsedTo ('foo)
				""" foo_bar """ should beParsedTo ('foo_bar)
			}

			"numbers" in {
				implicit val parser = number

				""" 0 """ should beParsedTo (0)
				""" 192 """ should beParsedTo (192)
				""" -10 """ should beParsedTo (-10)
			}

			"arrays" - {

				"at" in {
					implicit val parser = arrayAt

					""" #[] """ should beParsedTo (%())
					""" #[1] """ should beParsedTo (%(1))
					""" #[1,2,3] """ should beParsedTo (%(1, 2, 3))
					""" #[1,1+1,3] """ should beParsedTo (%(1, Add(1, 1), 3))
					""" #[#[1,2],#[3,4]] """ should beParsedTo (%(%(1, 2), %(3, 4)))
					""" #[#[]] """ should beParsedTo (%(%()))
					""" #[#[],#[]] """ should beParsedTo (%(%(), %()))

					""" #[1,2,3][4] """ should beParsedTo (At(%(1, 2, 3), 4))
					""" #[1,2,3][4 + 5] """ should beParsedTo (At(%(1, 2, 3), Add(4, 5)))
					""" #[#[1,2]][0][1] """ should beParsedTo (At(At(%(%(1, 2)), 0), 1))

					""" #[].length """ should beParsedTo (Length(%()))
					""" #[1,1+1,3].length """ should beParsedTo (Length(%(1, Add(1, 1), 3)))
					""" #[#[1,2]][0][1].length """ should beParsedTo (Length(At(At(%(%(1, 2)), 0), 1)))
				}

				"put" in {
					implicit val parser = arrayPut

					""" #[1,2,3][4] = 5 + 6; """ should beParsedTo (Put(%(1, 2, 3), 4, Add(5, 6)))
				}

			}

			"aritmethic expressions" in {
				implicit val parser = expression

				" 2 && 3 " should beParsedTo (And(2, 3))
				" 2 && 3 && 4" should beParsedTo (And(And(2, 3), 4))
				" (2 && 3) && 4" should beParsedTo (And(And(2, 3), 4))
				" 2 && (3 && 4)" should beParsedTo (And(2, And(3, 4)))

				" 2 || 3 " should beParsedTo (Or(2, 3))
				" 2 || 3 || 4" should beParsedTo (Or(Or(2, 3), 4))
				" (2 || 3) || 4" should beParsedTo (Or(Or(2, 3), 4))
				" 2 || (3 || 4)" should beParsedTo (Or(2, Or(3, 4)))

				" 2 + 3 " should beParsedTo (Add(2, 3))
				" 2 + 3 + 4" should beParsedTo (Add(Add(2, 3), 4))
				" (2 + 3) + 4" should beParsedTo (Add(Add(2, 3), 4))
				" 2 + (3 + 4)" should beParsedTo (Add(2, Add(3, 4)))

				" 2 * 3 " should beParsedTo (Mul(2, 3))
				" 2 * 3 * 4" should beParsedTo (Mul(Mul(2, 3), 4))
				" (2 * 3) * 4" should beParsedTo (Mul(Mul(2, 3), 4))
				" 2 * (3 * 4)" should beParsedTo (Mul(2, Mul(3, 4)))

				" 2 / 3 " should beParsedTo (Div(2, 3))
				" 2 / 3 / 4" should beParsedTo (Div(Div(2, 3), 4))
				" (2 / 3) / 4" should beParsedTo (Div(Div(2, 3), 4))
				" 2 / (3 / 4)" should beParsedTo (Div(2, Div(3, 4)))

				" 2 == 3 " should beParsedTo (Eq(2, 3))
				" 2 == 3 + 4" should beParsedTo (Eq(2, Add(3, 4)))
				" 3 + 4 == 2" should beParsedTo (Eq(Add(3, 4), 2))

				" 2 != 3 " should beParsedTo (Not(Eq(2, 3)))
				" 2 != 3 + 4" should beParsedTo (Not(Eq(2, Add(3, 4))))
				" 3 + 4 != 2" should beParsedTo (Not(Eq(Add(3, 4), 2)))

				" 2 > 3 " should beParsedTo (Greater(2, 3))
				" 2 > 3 + 4" should beParsedTo (Greater(2, Add(3, 4)))
				" 3 + 4 > 2" should beParsedTo (Greater(Add(3, 4), 2))

				" 2 >= 3 " should beParsedTo (GreaterOrEq(2, 3))
				" 2 >= 3 + 4" should beParsedTo (GreaterOrEq(2, Add(3, 4)))
				" 3 + 4 >= 2" should beParsedTo (GreaterOrEq(Add(3, 4), 2))

				" 2 < 3 " should beParsedTo (Lesser(2, 3))
				" 2 < 3 + 4" should beParsedTo (Lesser(2, Add(3, 4)))
				" 3 + 4 < 2" should beParsedTo (Lesser(Add(3, 4), 2))

				" 2 <= 3 " should beParsedTo (LesserOrEq(2, 3))
				" 2 <= 3 + 4" should beParsedTo (LesserOrEq(2, Add(3, 4)))
				" 3 + 4 <= 2" should beParsedTo (LesserOrEq(Add(3, 4), 2))

				" ! 2 > 3 " should beParsedTo (Not(Greater(2, 3)))
				" ! 2 != 3 " should beParsedTo (Not(Not(Eq(2, 3))))
				" 2 != 3 && ! 5 > 7 " should beParsedTo (And(Not(Eq(2, 3)), Not(Greater(5, 7))))
				" 2 != 3 && ! 5 > 7 || 6 < 3 " should beParsedTo (Or(And(Not(Eq(2, 3)), Not(Greater(5, 7))), Lesser(6, 3)))

				" 2 * 3 + 4" should beParsedTo (Add(Mul(2, 3), 4))
				" (2 * 3) + 4" should beParsedTo (Add(Mul(2, 3), 4))
				" 2 * (3 + 4)" should beParsedTo (Mul(2, Add(3, 4)))
				" 4 + 2 * 3" should beParsedTo (Add(4, Mul(2, 3)))
				" 4 + (2 * 3)" should beParsedTo (Add(4, Mul(2, 3)))
				" (4 + 2) * 3" should beParsedTo (Mul(Add(4, 2), 3))

				" 10 + x == y * 5 + 2 * 3" should beParsedTo (Eq(Add(10, 'x), Add(Mul('y, 5), Mul(2, 3))))
				" 10 + -x == y * (5 + -2) * 3" should beParsedTo (Eq(Add(10, Mul(-1, 'x)), Mul(Mul('y, Add(5, -2)), 3)))
				" 1 * 2 + 3 / 4 != 5 - 6 && 7 - 8 >= -9 * (2 + 3)" should beParsedTo (And(Not(Eq(Add(Mul(1, 2), Div(3, 4)), Sub(5, 6))), GreaterOrEq(Sub(7, 8), Mul(-9, Add(2, 3)))))
			}

			"if" in {
				implicit val parser = ifExp

				""" if(x < 3) { x + 4; } """ should beParsedTo (If(Lesser('x, 3), Add('x, 4) :: Nil, Nil))
				""" if(x < 3) { x + 4; } else { x - 1; } """ should beParsedTo (If(Lesser('x, 3), Add('x, 4) :: Nil, Sub('x, 1) :: Nil))
			}

			"while" in {
				implicit val parser = whileExp

				""" while(x < 3) { x + 4; x - 1; } """ should beParsedTo (While(Lesser('x, 3), Add('x, 4) :: Sub('x, 1) :: Nil))
			}

			"assign" in {
				implicit val parser = assign

				""" x = y; """ should beParsedTo (Assign('x, 'y))
				""" x = x - 1; """ should beParsedTo (Assign('x, Sub('x, 1)))
			}

			"message chain" in {
				implicit val parser = messageChain

				""" x """ should beParsedTo ('x)

				""" x.a """ should beParsedTo(Get('x, 'a))
				""" x.a.b """ should beParsedTo(Get(Get('x, 'a), 'b))

				""" x.m() """ should beParsedTo (Send('x, 'm, Nil))
				""" x.m(1) """ should beParsedTo (Send('x, 'm, List(1)))
				""" x.m(1,2) """ should beParsedTo (Send('x, 'm, List(1, 2)))
				""" x.m(1,2,y.n(z)) """ should beParsedTo (Send('x, 'm, List(1, 2, Send('y, 'n, 'z :: Nil))))
				""" x.m(1).n(y) """ should beParsedTo (Send(Send('x, 'm, 1 :: Nil), 'n, 'y :: Nil))

				""" x.m(1).a.n(2 + 3) """ should beParsedTo (Send(Get(Send('x, 'm, List(1)), 'a), 'n, Add(2, 3) :: Nil))
			}

			"attribute set" in {
				implicit val parser = attributeSet

				""" x.a = 1; """ should beParsedTo(Set('x, 'a, 1))
				""" x.m().a = 1 + 2; """ should beParsedTo (Set(Send('x, 'm, Nil), 'a, Add(1, 2)))
				""" x.m().a = y.n(1) + 2; """ should beParsedTo (Set(Send('x, 'm, Nil), 'a, Add(Send('y, 'n, 1 :: Nil), 2)))
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