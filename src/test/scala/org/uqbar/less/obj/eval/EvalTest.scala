package org.uqbar.less.obj.eval

import org.uqbar.less.obj.eval._
import org.uqbar.less.obj.Preety._
import org.scalatest.BeforeAndAfter
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

class EvalTest extends FreeSpec with Matchers with BeforeAndAfter {

	"Bytecode Instruction" - {
		"LOAD" - {
			"should push a local into the stack" in {
				val result = Eval(locals = Map('x -> 7))(LOAD('x))

				result.locals('x) should be (7)
				result.stack should be (7 :: Nil)
			}
		}

		"STORE" - {
			"should pop stack and save into locals" in {
				val result = Eval(stack = 7 :: Nil)(STORE('x))

				result.locals('x) should be (7)
				result.stack should be (Nil)
			}
		}

		"EQ" - {
			"should pop two entries and push a 1 if they are identical" in {
				val result = Eval(stack = 7 :: 7 :: Nil)(EQ)

				result.stack should be (1 :: Nil)
			}
			"should pop two entries and push a 0 if they are not identical" in {
				val result = Eval(stack = 7 :: 2 :: Nil)(EQ)

				result.stack should be (0 :: Nil)
			}
		}

		"GET" - {
			"should pop an object reference from the stack and push back the content of a slot" in {
				val myObject = Map('foo -> 9)
				val result = Eval(stack = 'abc :: Nil, memory = Memory(Map('abc -> myObject)))(GET('foo))

				result.stack should be (9 :: Nil)
			}
		}

		"SET" - {
			"should pop an object reference and a value and set it into an unexistent slot" in {
				val myObject = Map('foo -> 9)
				val result = Eval(stack = 'abc :: 2 :: Nil, memory = Memory(Map('abc -> myObject)))(SET('bar))

				result.stack should be (Nil)
				result.memory should be (Memory(Map('abc -> myObject.updated('bar, 2))))
			}

			"should pop an object reference and a value and set it into an existent slot, overriding it" in {
				val myObject = Map('foo -> 9)
				val result = Eval(stack = 'abc :: 2 :: Nil, memory = Memory(Map('abc -> myObject)))(SET('foo))

				result.stack should be (Nil)
				result.memory should be (Memory(Map('abc -> myObject.updated('foo, 2))))
			}
		}

		"SEND" - {
			"should pop an object reference from the stack and all required arguments and send a message to it, pushing back the result" in {
				val myObject = Map('foo -> Seq(PUSHN(5), LOAD('$1), ADD))
				val result = Eval(stack = 'abc :: 2 :: Nil, memory = Memory(Map('abc -> myObject)))(SEND('foo, 1))

				result.stack should be (7 :: Nil)
			}
		}

		"MKA" - {
			"should create an array of the specified size in memory and push a reference to it into the stack" in {
				val result = Eval()(MKA(3))

				result.stack should have size 1
				result.memory[O](result.stack.head.asInstanceOf[Symbol]) should have size 3
				result.memory[O](result.stack.head.asInstanceOf[Symbol]).keys should be (Set(Symbol("0"), Symbol("1"), Symbol("2")))
			}
		}

		"LENGTH" - {
			"should pop an array and push it's length" in {
				val result = Eval(stack = 'abc :: Nil, memory = Memory(Map('abc -> Map(Symbol("0") -> null, Symbol("1") -> null))))(LENGTH)

				result.stack should be (2 :: Nil)
			}
		}

		"AT" - {
			"should pop an array and an index and push the array content at that index" in {
				val result = Eval(stack = 'abc :: 1 :: Nil, memory = Memory(Map('abc -> Map(Symbol("0") -> null, Symbol("1") -> 9))))(AT)

				result.stack should be (9 :: Nil)
			}
		}

		"PUT" - {
			"should pop an array, a value and an index and set the array at that index to that value" in {
				val result = Eval(stack = 'abc :: 2 :: 1 :: Nil, memory = Memory(Map('abc -> Map(Symbol("0") -> null, Symbol("1") -> 9))))(PUT)

				result.memory should be (Memory(Map('abc -> Map(Symbol("0") -> null, Symbol("1") -> 2))))
			}
		}

		"ADD" - {
			"should pop two numbers and push it's addition to the stack" in {
				val result = Eval(stack = 3 :: 4 :: Nil)(ADD)

				result.stack should be (7 :: Nil)
			}
		}

		"MUL" - {
			"should pop two numbers and push it's multiplication to the stack" in {
				val result = Eval(stack = 3 :: 4 :: Nil)(MUL)

				result.stack should be (12 :: Nil)
			}
		}

		"GRTHN" - {
			"should pop two numbers and push a 1 if the first is greater than the second" in {
				val result = Eval(stack = 5 :: 2 :: Nil)(GRTHN)

				result.stack should be (1 :: Nil)
			}

			"should pop two numbers and push a 0 if the first is lesser than the second" in {
				val result = Eval(stack = 2 :: 5 :: Nil)(GRTHN)

				result.stack should be (0 :: Nil)
			}

			"should pop two numbers and push a 0 if the first is equal than the second" in {
				val result = Eval(stack = 2 :: 2 :: Nil)(GRTHN)

				result.stack should be (0 :: Nil)
			}
		}

		"PUSHN" - {
			"should push a constant number to the stack" in {
				val result = Eval()(PUSHN(5))

				result.stack should be (5 :: Nil)
			}
		}

		"PUSHR" - {
			"should push a constant reference to the stack" in {
				val result = Eval()(PUSHR('abc))

				result.stack should be ('abc :: Nil)
			}
		}

		"IFNZ" - {
			"should pop a number and increase the pc by the given ammount + 1 if it is not zero" in {
				val result = Eval(stack = 2 :: Nil)(IFNZ(10))

				result.stack should be (Nil)
				result.pc should be (11)
			}

			"should pop a number and increase the pc only by 1 if it is zero" in {
				val result = Eval(stack = 0 :: Nil)(IFNZ(10))

				result.stack should be (Nil)
				result.pc should be (1)
			}
		}

		"GOTO" - {
			"should increase the pc by the given ammount" in {
				val result = Eval()(GOTO(10))

				result.pc should be (11)
			}
		}
	}
}