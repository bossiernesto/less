package org.uqbar.less.compiler

import scala.language.implicitConversions
import org.uqbar.less.obj.eval._
import org.uqbar.less.Preety._
import org.scalatest.BeforeAndAfter
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import org.uqbar.less.Bytecode._
import org.uqbar.less.SemanticModel._

class CompilerTest extends FreeSpec with Matchers with BeforeAndAfter {

  "Compile" - {
    "Should be able to compile" - {

      "references to LOAD" in {
        toInput('_) should beCompiledTo(LOAD('_))

        toInput('x) should beCompiledTo(LOAD('x))
        toInput('y) should beCompiledTo(LOAD('y))
        toInput('foo) should beCompiledTo(LOAD('foo))
        toInput('bar) should beCompiledTo(LOAD('bar))
      }

      "numerics to PUSH" in {
        toInput(42) should beCompiledTo(PUSH(42))
        toInput(-42) should beCompiledTo(PUSH(-42))
      }

      "multiple references to LOAD" in {
        toInput('x, 'y) should beCompiledTo(LOAD('x), LOAD('y))

      }

      "multiple numerics to PUSH" in {
        toInput(0, 33) should beCompiledTo(PUSH(0), PUSH(33))
      }

      "mixed numeric,statement LOAD,PUSH" in {
        toInput('foo, 32) should beCompiledTo(LOAD('foo), PUSH(32))
      }

      "array" - {
        "at compile to PUSH,NEWA,PUT,AT" in {
          toInput(%()) should beCompiledTo(NEWA(0))
          toInput(%(1)) should beCompiledTo(PUSH(1), NEWA(1), PUSH(0), PUT)
          toInput(%(1, 2, 3)) should beCompiledTo(PUSH(3), PUSH(2), PUSH(1), NEWA(3), PUSH(0), PUT,
            PUSH(1), PUT, PUSH(2), PUT)
          toInput(At(%(1, 2, 3), Add(2, 7))) should beCompiledTo(PUSH(7), PUSH(2), ADD, PUSH(3), PUSH(2), PUSH(1),
            NEWA(3), PUSH(0), PUT, PUSH(1), PUT, PUSH(2), PUT, AT)
          toInput(At(%(1, 2, 3), 4)) should beCompiledTo(
            PUSH(4), PUSH(3), PUSH(2), PUSH(1), NEWA(3), PUSH(0), PUT, PUSH(1), PUT, PUSH(2), PUT, AT)
        }

        "put compile to ADD,PUSH,NEWA,PUT" in {
          toInput(Put(%(1, 2), 2, 1)) should beCompiledTo(PUSH(1), PUSH(2), PUSH(1), NEWA(2), PUSH(0), PUT, PUSH(1), PUT, PUSH(2), PUT)
          toInput(Put(%(1, 2, 3), 4, Add(5, 6))) should beCompiledTo(PUSH(6), PUSH(5), ADD, PUSH(3), PUSH(2), PUSH(1), NEWA(3), PUSH(0), PUT, PUSH(1), PUT, PUSH(2), PUT, PUSH(4), PUT)
        }

      }

      "expressions" - {
        "compile boolean " in {
          toInput(And(2, 3)) should beCompiledTo(PUSH(3), PUSH(2), AND)
          toInput(Or(2, 4)) should beCompiledTo(PUSH(4), PUSH(2), OR)
          toInput(And(1, And(3, 4))) should beCompiledTo(PUSH(4), PUSH(3), AND, PUSH(1), AND)
          toInput(Or(5, Or(2, 1))) should beCompiledTo(PUSH(1), PUSH(2), OR, PUSH(5), OR)
        }

        "compile mixed boolean" in {
          toInput(And(2, Or(3, 5))) should beCompiledTo(PUSH(5), PUSH(3), OR, PUSH(2), AND)
        }

        "compile algebraic" in {
          toInput(Add(1, 2)) should beCompiledTo(PUSH(2), PUSH(1), ADD)
          toInput(Add(1, 2), Add(3, 4)) should beCompiledTo(PUSH(2), PUSH(1), ADD, PUSH(4), PUSH(3), ADD)
          toInput(Add(Add(2, 3), 4)) should beCompiledTo(PUSH(4), PUSH(3), PUSH(2), ADD, ADD)

          toInput(Div(2, 2)) should beCompiledTo(PUSH(2), PUSH(2), DIV)
          toInput(Div(Div(8, 2), 2)) should beCompiledTo(PUSH(2), PUSH(2), PUSH(8), DIV, DIV)

          toInput(Mul(1, 2)) should beCompiledTo(PUSH(2), PUSH(1), MUL)
          toInput(Mul(Mul(2, 3), 4)) should beCompiledTo(PUSH(4), PUSH(3), PUSH(2), MUL, MUL)
        }

        "compile comparators" in {
          toInput(Eq(2, 3)) should beCompiledTo(PUSH(3), PUSH(2), EQ)
          toInput(Eq(2, Add(1, 1))) should beCompiledTo(PUSH(1), PUSH(1), ADD, PUSH(2), EQ)
          toInput(Not(Eq(2, 3))) should beCompiledTo(PUSH(3), PUSH(2), EQ, NOT)

          toInput(Greater(3, 5)) should beCompiledTo(PUSH(5), PUSH(3), GRTHN)
          toInput(Greater(Mul(1, 2), 1)) should beCompiledTo(PUSH(1), PUSH(2), PUSH(1), MUL, GRTHN)

          toInput(GreaterOrEq(Mul(4, 3), 5)) should beCompiledTo(PUSH(5), PUSH(3), PUSH(4), MUL, GREQ)

          toInput(Lesser(4, 3)) should beCompiledTo(PUSH(3), PUSH(4), LSTHN)
          toInput(Lesser(2, Add(3, 5))) should beCompiledTo(PUSH(5), PUSH(3), ADD, PUSH(2), LSTHN)

          toInput(LesserOrEq(3, Add(2, 2))) should beCompiledTo(PUSH(2), PUSH(2), ADD, PUSH(3), LSEQ)
        }

        "compile complex operators" in {
          toInput((And(Not(Eq(Add(Mul(1, 2), Div(3, 4)), Sub(5, 6))), GreaterOrEq(Sub(7, 8), Mul(-9, Add(2, 3)))))) should
            beCompiledTo(PUSH(3), PUSH(2), ADD, PUSH(-9), MUL, PUSH(8), PUSH(7), SUB, GREQ, PUSH(6), PUSH(5), SUB, PUSH(4), PUSH(3), DIV, PUSH(2), PUSH(1), MUL, ADD, EQ, NOT, AND)

        }

        "compile if" in {
          toInput((If(Lesser('x, 3), Add('x, 4) :: Nil, Nil))) should beCompiledTo(PUSH(3), LOAD('x), LSTHN, IFNZ(3), PUSH(4), LOAD('x), ADD)
        }

        "compile while" in {
          toInput(While(Lesser('x, 3), Add('x, 4) :: Sub('x, 1) :: Nil)) should beCompiledTo(PUSH(3), LOAD('x), LSTHN, IFNZ(6), PUSH(4), LOAD('x), ADD, PUSH(1), LOAD('x), SUB, GOTO(-10))
        }

        "compile assign" in {
          toInput(Assign('x, 'y)) should beCompiledTo(LOAD('y), STORE('x))
          toInput(Assign('x, Sub('x, 1))) should beCompiledTo(PUSH(1), LOAD('x), SUB, STORE('x))
          toInput(Assign('r, Get('this, 'a))) should beCompiledTo(LOAD('this), GET('a), STORE('r))
        }

        "compile message chain" in {
          toInput('x) should beCompiledTo(LOAD('x))
          toInput(Get('x, 'a)) should beCompiledTo(LOAD('x), GET('a))
          toInput(Get(Get('x, 'a), 'b)) should beCompiledTo(LOAD('x), GET('a), GET('b))
          toInput(Send('x, 'm, Nil)) should beCompiledTo(LOAD('x), SEND('m,0))
          toInput(Send('x, 'm, List(1, 2, Send('y, 'n, 'z :: Nil)))) should beCompiledTo(PUSH(1), PUSH(2), LOAD('z), LOAD('y), SEND('n,1), LOAD('x), SEND('m,3))
        }
        
        "compile method" in {
          toInput(M('m, List('x, 'y), List(5))) should beCompiledTo(DUP, NEWM('m,List(PUSH(5))))
        }
        
        "compile object" in {
          toInput(O('A, List(M('m, Nil, List(5)), M('n, Nil, List(6))))) should beCompiledTo(NEW, DUP, STORE('A), DUP, NEWM('m,List(PUSH(5))), DUP, NEWM('n,List(PUSH(6))), POP)
        }
        
      }

    }

  }

  //▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
  // HELPER Matcher CLASSES
  //▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
  case class beCompiledTo[T](expect: T*) extends Matcher[Seq[Sentence]] {
    def apply(left: Seq[Sentence]) = {
      val result = Compile.apply(left)

      MatchResult(result == expect,
        s"Compile ${result} did not equal $expect",
        s"Compile ${result} was equal to $expect");
    }
  }

  implicit def toInput(left: Sentence*): Seq[Sentence] = left

}