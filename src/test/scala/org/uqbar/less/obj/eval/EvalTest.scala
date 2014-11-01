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
				val myObject = O(Map('foo -> 9))
				val result = Eval(stack = 3 :: Nil, memory = Memory(Map(3 -> myObject)))(GET('foo))

				result.stack should be (9 :: Nil)
			}
		}

		"SET" - {
			"should pop an object reference and a value and set it into an unexistent slot" in {
				val myObject = O(Map('foo -> 9))
				val result = Eval(stack = 3 :: 2 :: Nil, memory = Memory(Map(3 -> myObject)))(SET('bar))

				result.stack should be (Nil)
				result.memory should be (Memory(Map(3 -> myObject.updated('bar, 2))))
			}

			"should pop an object reference and a value and set it into an existent slot, overriding it" in {
				val myObject = O(Map('foo -> 9))
				val result = Eval(stack = 3 :: 2 :: Nil, memory = Memory(Map(3 -> myObject)))(SET('foo))

				result.stack should be (Nil)
				result.memory should be (Memory(Map(3 -> myObject.updated('foo, 2))))
			}
		}

		"SEND" - {
			"should pop an object reference from the stack and all required arguments and send a message to it, pushing back the result" in {
				val myObject = O(Map('foo -> 7))
				val myMethod = M(Seq(PUSH(5), LOAD('$1), ADD))
				val result = Eval(stack = 3 :: 2 :: Nil, memory = Memory(Map(3 -> myObject, 7 -> myMethod)))(SEND('foo, 1))

				result.stack should be (7 :: Nil)
			}
		}

		"MKA" - {
			"should create an array of the specified size in memory and push a reference to it into the stack" in {
				val result = Eval()(MKA(3))

				result.stack should have size 1
				result.memory[O](result.stack.head).slots should have size 3
				result.memory[O](result.stack.head).slots.keys should be (Set(Symbol("0"), Symbol("1"), Symbol("2")))
			}
		}

		"LENGTH" - {
			"should pop an array and push it's length" in {
				val myArray = O(Map(Symbol("0") -> 2, Symbol("1") -> 9))
				val result = Eval(stack = 3 :: Nil, memory = Memory(Map(3 -> myArray)))(LENGTH)

				result.stack should be (2 :: Nil)
			}
		}

		"AT" - {
			"should pop an array and an index and push the array content at that index" in {
				val myArray = O(Map(Symbol("0") -> 2, Symbol("1") -> 9))
				val result = Eval(stack = 3 :: 1 :: Nil, memory = Memory(Map(3 -> myArray)))(AT)

				result.stack should be (9 :: Nil)
			}
		}

		"PUT" - {
			"should pop an array, a value and an index and set the array at that index to that value" in {
				val myArray = O(Map(Symbol("0") -> 5, Symbol("1") -> 9))
				val result = Eval(stack = 3 :: 2 :: 1 :: Nil, memory = Memory(Map(3 -> myArray)))(PUT)

				result.memory should be (Memory(Map(3 -> O(Map(Symbol("0") -> 5, Symbol("1") -> 2)))))
			}
		}

		"ADD" - {
			"should pop two numbers and push it's addition to the stack" in {
				val result = Eval(stack = 3 :: 4 :: Nil)(ADD)

				result.stack should be (7 :: Nil)
			}
		}

		"SUB" - {
			"should pop two numbers and push it's subtraction to the stack" in {
				val result = Eval(stack = 3 :: 4 :: Nil)(SUB)

				result.stack should be (-1 :: Nil)
			}
		}

		"MUL" - {
			"should pop two numbers and push it's multiplication to the stack" in {
				val result = Eval(stack = 3 :: 4 :: Nil)(MUL)

				result.stack should be (12 :: Nil)
			}
		}

		"DIV" - {
			"should pop two numbers and push it's division to the stack" in {
				val result = Eval(stack = 12 :: 3 :: Nil)(DIV)

				result.stack should be (4 :: Nil)
			}
		}

		"AND" - {
			"should pop two numbers and push to the stack a 0 if none of them is different from 0" in {
				val result = Eval(stack = 0 :: 0 :: Nil)(AND)

				result.stack should be (0 :: Nil)
			}
			"should pop two numbers and push to the stack a 0 if the first is different from 0" in {
				val result = Eval(stack = 1 :: 0 :: Nil)(AND)

				result.stack should be (0 :: Nil)
			}
			"should pop two numbers and push to the stack a 0 if the second is different from 0" in {
				val result = Eval(stack = 0 :: 1 :: Nil)(AND)

				result.stack should be (0 :: Nil)
			}
			"should pop two numbers and push to the stack a 1 if booth are different from 0" in {
				val result = Eval(stack = 1 :: 1 :: Nil)(AND)

				result.stack should be (1 :: Nil)
			}
		}

		"OR" - {
			"should pop two numbers and push to the stack a 0 if none of them is different from 0" in {
				val result = Eval(stack = 0 :: 0 :: Nil)(OR)

				result.stack should be (0 :: Nil)
			}
			"should pop two numbers and push to the stack a 1 if the first is different from 0" in {
				val result = Eval(stack = 1 :: 0 :: Nil)(OR)

				result.stack should be (1 :: Nil)
			}
			"should pop two numbers and push to the stack a 1 if the second is different from 0" in {
				val result = Eval(stack = 0 :: 1 :: Nil)(OR)

				result.stack should be (1 :: Nil)
			}
			"should pop two numbers and push to the stack a 1 if booth are different from 0" in {
				val result = Eval(stack = 1 :: 1 :: Nil)(OR)

				result.stack should be (1 :: Nil)
			}
		}

		"NOT" - {
			"should pop one number and push to the stack a 0 if it is different from 0" in {
				val result = Eval(stack = 1 :: Nil)(NOT)

				result.stack should be (0 :: Nil)
			}
			"should pop one number and push to the stack a 1 if it is 0" in {
				val result = Eval(stack = 0 :: Nil)(NOT)

				result.stack should be (1 :: Nil)
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
		"GREQ" - {
			"should pop two numbers and push a 1 if the first is greater than the second" in {
				val result = Eval(stack = 5 :: 2 :: Nil)(GREQ)

				result.stack should be (1 :: Nil)
			}

			"should pop two numbers and push a 0 if the first is lesser than the second" in {
				val result = Eval(stack = 2 :: 5 :: Nil)(GREQ)

				result.stack should be (0 :: Nil)
			}

			"should pop two numbers and push a 0 if the first is equal than the second" in {
				val result = Eval(stack = 2 :: 2 :: Nil)(GREQ)

				result.stack should be (1 :: Nil)
			}
		}
		"LSTHN" - {
			"should pop two numbers and push a 0 if the first is greater than the second" in {
				val result = Eval(stack = 5 :: 2 :: Nil)(LSTHN)

				result.stack should be (0 :: Nil)
			}

			"should pop two numbers and push a 1 if the first is lesser than the second" in {
				val result = Eval(stack = 2 :: 5 :: Nil)(LSTHN)

				result.stack should be (1 :: Nil)
			}

			"should pop two numbers and push a 0 if the first is equal than the second" in {
				val result = Eval(stack = 2 :: 2 :: Nil)(LSTHN)

				result.stack should be (0 :: Nil)
			}
		}
		"LSEQ" - {
			"should pop two numbers and push a 0 if the first is greater than the second" in {
				val result = Eval(stack = 5 :: 2 :: Nil)(LSEQ)

				result.stack should be (0 :: Nil)
			}

			"should pop two numbers and push a 1 if the first is lesser than the second" in {
				val result = Eval(stack = 2 :: 5 :: Nil)(LSEQ)

				result.stack should be (1 :: Nil)
			}

			"should pop two numbers and push a 1 if the first is equal than the second" in {
				val result = Eval(stack = 2 :: 2 :: Nil)(LSEQ)

				result.stack should be (1 :: Nil)
			}
		}

		"PUSH" - {
			"should push a constant number to the stack" in {
				val result = Eval()(PUSH(5))

				result.stack should be (5 :: Nil)
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