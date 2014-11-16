package org.uqbar.less.integration

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
import org.uqbar.less.parser._
import org.uqbar.less.eval.Eval
import java.lang.RuntimeException
import org.uqbar.less.eval._

class IntegrationTest extends FreeSpec with Matchers with BeforeAndAfter {

	"Integration" - {
		"Should parse and compile " - {
			"simple program" in {
				"""object B {}""" should
					parseAndCompile(NEW, DUP, STORE('B), POP);
			}

			"complex object" in {
				"""object O {
					this.a = 5;
					def m(){ 5; }
			}""" should parseAndCompile(NEW, DUP, STORE('O), PUSH(5), LOAD('this), SET('a), DUP, NEWM('m, List(PUSH(5))), POP);
			}

			"Multiple objects" in {
				"""object O {
					this.a = 5;
					def m(){ 5; }
			}
            object A {}
            """ should parseAndCompile(NEW, DUP, STORE('A), NEW, DUP, STORE('O), PUSH(5), LOAD('this), SET('a), DUP, NEWM('m, List(PUSH(5))), POP, POP);
			}

			"Program with method" in {
				"""object Pepita{
				def volar(){
					this.energia = this.energia - 10;
				}
        
				def comer(){
					this.energia = this.energia + 40;
				}
    	  	}
    	  Pepita.energia = 10;
    	  Pepita.comer();""" should parseAndCompile(NEW, DUP, STORE('Pepita), DUP, NEWM('volar, List(PUSH(10), LOAD('$0), GET('energia), SUB, LOAD('$0), SET('energia))), DUP, NEWM('comer, List(PUSH(40), LOAD('$0), GET('energia), ADD, LOAD('$0), SET('energia))), POP, PUSH(10), LOAD('Pepita), SET('energia), LOAD('Pepita), SEND('comer, 0));

			}
		}

		"Should not parse and compile" - {
			"Invalid class format" in {
				a[RuntimeException] shouldBe thrownBy {
					"""object P{
    		  def something{
    	     }
    	  """ should parseAndCompile(List())
				}

			}
		}

		"Should Evaluate" - {
			"simple program" in {
				"""object B {}""" should
					evaluateTo(State(Map('B -> 1), List(), 4, List(NEW, DUP, STORE('B), POP), Memory(Map(1 -> MO(Map())))));
			}
		}

		"evaluate multiple objects" in {
			"""object O {
				def m(){ this.a = 5; }
		  }
          object A {}""" should evaluateTo(State(Map('A -> 1, 'O -> 2), List(), 10, List(NEW, DUP, STORE('A), NEW, DUP, STORE('O), DUP, NEWM('m, List(PUSH(5), LOAD('$0), SET('a))), POP, POP), Memory(Map(1 -> MO(Map()), 2 -> MO(Map('m -> 3)), 3 -> MM(List(PUSH(5), LOAD('$0), SET('a)))))));
		}

		"program with method" in {
			"""object Pepita{
    		def volar(){
				this.energia = this.energia - 10;
			}
        
			def comer(){
				this.energia = this.energia + 40;
				}
    	  	}
    	 	Pepita.energia = 10;
    	 	Pepita.comer();""" should evaluateTo(State(Map('Pepita -> 1), List(), 13, List(NEW, DUP, STORE('Pepita), DUP, NEWM('volar, List(PUSH(10), LOAD('$0), GET('energia), SUB, LOAD('$0), SET('energia))), DUP, NEWM('comer, List(PUSH(40), LOAD('$0), GET('energia), ADD, LOAD('$0), SET('energia))), POP, PUSH(10), LOAD('Pepita), SET('energia), LOAD('Pepita), SEND('comer, 0)), Memory(Map(1 -> MO(Map('volar -> 2, 'comer -> 3, 'energia -> 10)), 2 -> MM(List(PUSH(10), LOAD('$0), GET('energia), SUB, LOAD('$0), SET('energia))), 3 -> MM(List(PUSH(40), LOAD('$0), GET('energia), ADD, LOAD('$0), SET('energia)))))));
		}
	}

	//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
	// MATCHERS
	//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

	case class parseAndCompile[T >: Sentence](expected: T*) extends Matcher[String] {
		def apply(left: String) = {
			val parsed = Parse.apply(left).get;
			val result = Compile(parsed);

			MatchResult(result == expected,
				s"Compile ${result} did not equal $expected",
				s"Compile ${result} was equal to $expected");
		}
	}

	case class evaluateTo[T >: Sentence](expected: Any) extends Matcher[String] {
		def apply(left: String) = {
			val compiled = Compile(Parse.apply(left).get);
			val result = Eval.apply()(compiled: _*);

			MatchResult(result == expected,
				s"Compile ${result} did not equal $expected",
				s"Compile ${result} was equal to $expected");
		}

	}

}