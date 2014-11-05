package org.uqbar.less.parser

import scala.language.implicitConversions
import scala.util.parsing.combinator._
import org.uqbar.less.SemanticModel._
import org.uqbar.less.Syntax

object Parse extends Parse { val syntax = Syntax }
trait Parse extends RegexParsers {
	def syntax: Syntax

	implicit def Symbol_to_Parser(key: Symbol): Parser[String] = { syntax(key) }

	def apply(target: String) = parse(program, target)

	protected lazy val identifier = 'identifier ^^ { id => ID(Symbol(id)) }
	protected lazy val number = 'number ^^ { n => N(n.toInt) }
	protected lazy val array = 'arrayOpen ~> repsep(sentence, 'sentenceSep) <~ 'arrayClose ^^ { A(_) }
	protected lazy val reference = identifier ^^ { R(_) }

	protected lazy val codeBlock = 'codeOpen ~> line.* <~ 'codeClose
	protected lazy val program = line.*

	protected lazy val line = ifExp | whileExp | objectDef | assign | putOrSet | sentence <~ 'lineSep

	protected lazy val assign = (identifier <~ 'assignOp).? ~ sentence <~ 'lineSep ^^ {
		case maybeId ~ value => maybeId.fold(value)(Assign(_, value))
	}

	protected lazy val boolOp =
		'andOp ^^ { _ => And } |
			'orOp ^^ { _ => Or }

	protected lazy val eqOp =
		'eqOp ^^ { _ => Eq } |
			'neqOp ^^ { _ => (l: Sentence, r: Sentence) => Not(Eq(l, r)) } |
			'geOp ^^ { _ => GreaterOrEq } |
			'leOp ^^ { _ => LesserOrEq } |
			'gtOp ^^ { _ => Greater } |
			'ltOp ^^ { _ => Lesser }

	protected lazy val addOp =
		'addOp ^^ { _ => Add } |
			'subOp ^^ { _ => Sub }

	protected lazy val mulOp =
		'mulOp ^^ { _ => Mul } |
			'divOp ^^ { _ => Div }

	protected def opChain(op: Parser[(Sentence, Sentence) => Sentence], subLevel: Parser[Sentence]) =
		subLevel ~ (op ~ subLevel).* ^^ { case l ~ r => (l /: r){ case (l, o ~ r) => o(l, r) } }

	protected lazy val sentence: Parser[Sentence] = boolExp | array
	protected lazy val boolExp = opChain(boolOp, eqExp)
	protected lazy val eqExp = 'notOp.? ~ opChain(eqOp, addExp) ^^ { case not ~ exp => not.fold(exp){ _ => Not(exp) } }
	protected lazy val addExp = opChain(addOp, mulExp)
	protected lazy val mulExp = opChain(mulOp, primaryExp)
	protected lazy val primaryExp: Parser[Sentence] = 'parensOpen ~> sentence <~ 'parensClose | number | accessChain | ('subOp ~> primaryExp ^^ { Mul(N(-1), _) })

	protected lazy val args = 'argOpen ~> repsep(sentence, 'sentenceSep) <~ 'argClose
	protected lazy val at = 'atOpen ~> sentence <~ 'atClose ^^ { arg => (arr: Sentence) => At(arr, arg) }
	protected lazy val messageOrSlot = 'access ~> identifier ~ args.? ^^ {
		case msg ~ args => { (prev: Sentence) => args.fold(Get(prev, msg): Sentence)(Send(prev, msg, _)) }
	}
	protected lazy val accessChain = (reference | array) ~ (at | messageOrSlot).* ^^ {
		case e ~ as => ((e: Sentence) /: as) { (p, a) => a(p) }
	}

	protected lazy val putOrSet = accessChain ~ ('assignOp ~> sentence) <~ 'lineSep ^^ {
		case At(a, k) ~ v => Put(a, k, v)
		case Get(o, a) ~ v => Set(o, a, v)
	}

	protected lazy val ifExp: Parser[Sentence] = ('ifKeyword ~> 'argOpen ~> sentence <~ 'argClose) ~ codeBlock ~ ('elseKeyword ~> codeBlock).? ^^ {
		case c ~ t ~ f => If(c, t, f.getOrElse(Nil))
	}
	protected lazy val whileExp: Parser[Sentence] = ('whileKeyword ~> 'argOpen ~> sentence <~ 'argClose) ~ codeBlock ^^ { case c ~ b => While(c, b) }

	protected lazy val methodDef: Parser[M] = 'defKeyword ~> identifier ~ ('argOpen ~> repsep(identifier, 'sentenceSep) <~ 'argClose) ~ codeBlock ^^ { case n ~ as ~ b => M(n, as, b) }

	protected lazy val objectDef = 'objectKeyword ~> identifier ~ ('codeOpen ~> (methodDef | putOrSet).* <~ 'codeClose) ^^ { case n ~ b => O(n, b) }

}