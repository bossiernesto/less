package org.uqbar.less.parser

import org.uqbar.less.SemanticModel._
import org.uqbar.less.Preety._
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

				""" _ """ should beParsedTo ('_: Sentence)
				""" x """ should beParsedTo ('x: Sentence)
				""" x1 """ should beParsedTo ('x1: Sentence)
				""" foo """ should beParsedTo ('foo: Sentence)
				""" foo_bar """ should beParsedTo ('foo_bar: Sentence)
			}

			"numbers" in {
				implicit val parser = number

				""" 0 """ should beParsedTo (0: Sentence)
				""" 192 """ should beParsedTo (192: Sentence)
				""" -10 """ should beParsedTo (-10: Sentence)
			}

			"arrays" - {

				"at" in {
					implicit val parser = sentence

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

				}

				"put" in {
					implicit val parser = putOrSet

					""" #[1,2,3][4] = 5 + 6; """ should beParsedTo (Put(%(1, 2, 3), 4, Add(5, 6)))
				}

			}

			"expressions" in {
				implicit val parser = sentence

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
				""" if(1) { r = this.a; } """ should beParsedTo (If(1, Seq(Assign('r, Get('this, 'a))), Nil))

			}

			"while" in {
				implicit val parser = whileExp

				""" while(x < 3) { x + 4; x - 1; } """ should beParsedTo (While(Lesser('x, 3), Add('x, 4) :: Sub('x, 1) :: Nil))
			}

			"assign" in {
				implicit val parser = assign

				""" x = y; """ should beParsedTo (Assign('x, 'y))
				""" x = x - 1; """ should beParsedTo (Assign('x, Sub('x, 1)))
				""" r = this.a; """ should beParsedTo (Assign('r, Get('this, 'a)))
			}

			"message chain" in {
				implicit val parser = accessChain

				""" x """ should beParsedTo ('x: Sentence)

				""" x.a """ should beParsedTo(Get('x, 'a))
				""" x.a.b """ should beParsedTo(Get(Get('x, 'a), 'b))

				""" x.m() """ should beParsedTo (Send('x, 'm, Nil))
				""" x.m(1) """ should beParsedTo (Send('x, 'm, List(1)))
				""" x.m(1,2) """ should beParsedTo (Send('x, 'm, List(1, 2)))
				""" x.m(1,2,y.n(z)) """ should beParsedTo (Send('x, 'm, List(1, 2, Send('y, 'n, 'z :: Nil))))
				""" x.m(1).n(y) """ should beParsedTo (Send(Send('x, 'm, 1 :: Nil), 'n, 'y :: Nil))

				""" x.m(1).a.n(2 + 3) """ should beParsedTo (Send(Get(Send('x, 'm, List(1)), 'a), 'n, Add(2, 3) :: Nil))

				""" #[].length """ should beParsedTo (Get(%(), 'length))
				""" #[1,1+1,3].length """ should beParsedTo (Get(%(1, Add(1, 1), 3), 'length))
				""" #[#[1,2]][0][1].length """ should beParsedTo (Get(At(At(%(%(1, 2)), 0), 1), 'length))
			}

			"attribute set" in {
				implicit val parser = putOrSet

				""" x.a = 1; """ should beParsedTo (Set('x, 'a, 1))
				""" x.m().a = 1 + 2; """ should beParsedTo (Set(Send('x, 'm, Nil), 'a, Add(1, 2)))
				""" x.m().a = y.n(1) + 2; """ should beParsedTo (Set(Send('x, 'm, Nil), 'a, Add(Send('y, 'n, 1 :: Nil), 2)))
			}

			"method" in {
				implicit val parser = methodDef

				""" def m() {} """ should beParsedTo (M('m, Nil, Nil))
				""" def m() { 5; } """ should beParsedTo (M('m, Nil, List(5)))
				""" def m(x) { 5; } """ should beParsedTo (M('m, List('x), List(5)))
				""" def m(x,y) { 5; } """ should beParsedTo (M('m, List('x, 'y), List(5)))
				""" def m() { if(this.a > 4) { r = this.a; } else {r = this.a + 4;} r; } """ should beParsedTo (
					M('m, Seq(), Seq(
						If(Greater(Get('this, 'a), 4), Seq(Assign('r, Get('this, 'a))), Seq(Assign('r, Add(Get('this, 'a), 4)))),
						'r
					)))
			}

			"object" in {
				implicit val parser = objectDef

				""" object X {} """ should beParsedTo (O('X, Nil))

				""" object X { def m(){ 5; } } """ should beParsedTo (O('X, List(M('m, Nil, List(5)))))
				"""
					object X {
						def m(){ 5; }
						def n(){ 6; }
				} """ should beParsedTo (O('X, List(M('m, Nil, List(5)), M('n, Nil, List(6)))))

				""" object X { this.a = 5; } """ should beParsedTo (O('X, List(Set('this, 'a, 5))))
			}

			"program" in {
				implicit val parser = program

				"""""" should beParsedTo (Nil)

				"""
					object X { }
				""" should beParsedTo (List(O('X, Nil)))

				"""
					object O {
						this.a = 5;
						def m(){ 5; }
					}
				""" should beParsedTo (List(
					O('O, Seq(
						Set('this, 'a, 5),
						M ('m, Nil, List(5))
					))
				))

				"""
					object O {
						this.a = 5;
						
						def m1() {
							if(this.a > 4) { r = this.a; } else {r = this.a + 4;}
							r;
						}
						def m2(i) { this.a + i; }
						def m3(i,j) { this.m2(#[1,2,3,4][i]) - j; }
					}
					O.a = 3;
					O.m1() + O.m3(2 + 1, 3);
				""" should beParsedTo (
					List(
						O('O, Seq(
							Set('this, 'a, 5),
							M('m1, Seq(), Seq(
								If(Greater(Get('this, 'a), 4), Seq(Assign('r, Get('this, 'a))), Seq(Assign('r, Add(Get('this, 'a), 4)))),
								'r
							)),
							M('m2, Seq('i), Seq(
								Add(Get('this, 'a), 'i)
							)),
							M('m3, Seq('i, 'j), Seq(
								Sub(Send('this, 'm2, Seq(At(%(1, 2, 3, 4), 'i))), 'j)
							))
						)),
						Set('O, 'a, 3),
						Add(Send('O, 'm1, Nil), Send('O, 'm3, Seq(Add(2, 1), 3)))
					)
				)

			}
		}
	}

	//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
	// MATCHERS
	//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

	case class beParsedTo[T](expected: Any)(implicit parser: Parser[T]) extends Matcher[String] {
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