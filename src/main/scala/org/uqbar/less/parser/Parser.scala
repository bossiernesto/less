package org.uqbar.less.parser

import scala.util.parsing.combinator._
import org.uqbar.less.SemanticModel._
import org.uqbar.less.Syntax

object Parse extends Parse
trait Parse extends RegexParsers {

	def apply(target: String) = parse(program, target)

	protected lazy val identifier = Syntax.identifier.r ^^ { id => ID(Symbol(id)) }
	protected lazy val number = Syntax.number.r ^^ { n => N(n.toInt) }
	protected lazy val array = Syntax.arrayOpen ~> repsep(sentence, Syntax.sentenceSep) <~ Syntax.arrayClose ^^ { A(_) }
	protected lazy val reference = identifier ^^ { R(_) }

	protected lazy val codeBlock = Syntax.codeOpen ~> line.* <~ Syntax.codeClose
	protected lazy val program = line.*

	protected lazy val line = ifExp | whileExp | objectDef | assign | putOrSet | sentence <~ Syntax.lineSep

	protected lazy val assign = (identifier <~ Syntax.assignOp).? ~ sentence <~ Syntax.lineSep ^^ {
		case maybeId ~ value => maybeId.fold(value)(Assign(_, value))
	}

	protected lazy val boolOp =
		Syntax.andOp ^^ { _ => And } |
			Syntax.orOp ^^ { _ => Or }

	protected lazy val eqOp =
		Syntax.eqOp ^^ { _ => Eq } |
			Syntax.neqOp ^^ { _ => (l: Sentence, r: Sentence) => Not(Eq(l, r)) } |
			Syntax.geOp ^^ { _ => GreaterOrEq } |
			Syntax.leOp ^^ { _ => LesserOrEq } |
			Syntax.gtOp ^^ { _ => Greater } |
			Syntax.ltOp ^^ { _ => Lesser }

	protected lazy val addOp =
		Syntax.addOp ^^ { _ => Add } |
			Syntax.subOp ^^ { _ => Sub }

	protected lazy val mulOp =
		Syntax.mulOp ^^ { _ => Mul } |
			Syntax.divOp ^^ { _ => Div }

	protected def opChain(op: Parser[(Sentence, Sentence) => Sentence], subLevel: Parser[Sentence]) =
		subLevel ~ (op ~ subLevel).* ^^ { case l ~ r => (l /: r){ case (l, o ~ r) => o(l, r) } }

	protected lazy val sentence: Parser[Sentence] = boolExp | array
	protected lazy val boolExp = opChain(boolOp, eqExp)
	protected lazy val eqExp = Syntax.notOp.? ~ opChain(eqOp, addExp) ^^ { case not ~ exp => not.fold(exp){ _ => Not(exp) } }
	protected lazy val addExp = opChain(addOp, mulExp)
	protected lazy val mulExp = opChain(mulOp, primaryExp)
	protected lazy val primaryExp: Parser[Sentence] = Syntax.parensOpen ~> sentence <~ Syntax.parensClose | number | accessChain | (Syntax.subOp ~> primaryExp ^^ { Mul(N(-1), _) })

	protected lazy val args = Syntax.argOpen ~> repsep(sentence, Syntax.sentenceSep) <~ Syntax.argClose
	protected lazy val at = Syntax.atOpen ~> sentence <~ Syntax.atClose ^^ { arg => (arr: Sentence) => At(arr, arg) }
	protected lazy val messageOrSlot = Syntax.access ~> identifier ~ args.? ^^ {
		case msg ~ args => { (prev: Sentence) => args.fold(Get(prev, msg): Sentence)(Send(prev, msg, _)) }
	}
	protected lazy val accessChain = (reference | array) ~ (at | messageOrSlot).* ^^ {
		case e ~ as => ((e: Sentence) /: as) { (p, a) => a(p) }
	}

	protected lazy val putOrSet = accessChain ~ (Syntax.assignOp ~> sentence) <~ Syntax.lineSep ^^ {
		case At(a, k) ~ v => Put(a, k, v)
		case Get(o, a) ~ v => Set(o, a, v)
	}

	protected lazy val ifExp: Parser[Sentence] = (Syntax.ifKeyword ~> Syntax.argOpen ~> sentence <~ Syntax.argClose) ~ codeBlock ~ (Syntax.elseKeyword ~> codeBlock).? ^^ {
		case c ~ t ~ f => If(c, t, f.getOrElse(Nil))
	}
	protected lazy val whileExp: Parser[Sentence] = (Syntax.whileKeyword ~> Syntax.argOpen ~> sentence <~ Syntax.argClose) ~ codeBlock ^^ { case c ~ b => While(c, b) }

	protected lazy val methodDef: Parser[M] = Syntax.defKeyword ~> identifier ~ (Syntax.argOpen ~> repsep(identifier, Syntax.sentenceSep) <~ Syntax.argClose) ~ codeBlock ^^ { case n ~ as ~ b => M(n, as, b) }

	protected lazy val objectDef = Syntax.objectKeyword ~> identifier ~ (Syntax.codeOpen ~> (methodDef | putOrSet).* <~ Syntax.codeClose) ^^ { case n ~ b => O(n, b) }

}