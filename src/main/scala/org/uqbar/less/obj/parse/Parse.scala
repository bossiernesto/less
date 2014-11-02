package org.uqbar.less.obj.parse

import scala.util.parsing.combinator._
import org.uqbar.less.obj._

object Parse extends Parse
trait Parse extends RegexParsers {

	protected lazy val identifier = "[a-z_A-Z][a-z_A-Z0-9]*".r ^^ { id => ID(Symbol(id)) }
	protected lazy val sentenceSep = ","
	protected lazy val assignOp = "="
	protected lazy val lineSep = ";"

	protected lazy val number = "-?[0-9]+".r ^^ { n => N(n.toInt) }
	protected lazy val array = "#[" ~> repsep(sentence, sentenceSep) <~ "]" ^^ { A(_) }
	protected lazy val reference = identifier ^^ { R(_) }

	protected lazy val codeBlock = "{" ~> line.* <~ "}"
	protected lazy val program = line.*

	protected lazy val line = ifExp | whileExp | objectDef | assign | putOrSet | sentence <~ lineSep

	protected lazy val assign = (identifier <~ assignOp).? ~ sentence <~ lineSep ^^ { case maybeId ~ value => maybeId.fold(value)(Assign(_, value)) }

	protected lazy val boolOp =
		"&&" ^^ { _ => And } |
			"||" ^^ { _ => Or }

	protected lazy val eqOp =
		"==" ^^ { _ => Eq } |
			"!=" ^^ { _ => (l: Sentence, r: Sentence) => Not(Eq(l, r)) } |
			">=" ^^ { _ => GreaterOrEq } |
			"<=" ^^ { _ => LesserOrEq } |
			">" ^^ { _ => Greater } |
			"<" ^^ { _ => Lesser }

	protected lazy val addOp =
		"+" ^^ { _ => Add } |
			"-" ^^ { _ => Sub }

	protected lazy val mulOp =
		"*" ^^ { _ => Mul } |
			"/" ^^ { _ => Div }

	protected def opChain(op: Parser[(Sentence, Sentence) => Sentence], subLevel: Parser[Sentence]) =
		subLevel ~ (op ~ subLevel).* ^^ { case l ~ r => (l /: r){ case (l, o ~ r) => o(l, r) } }

	protected lazy val sentence: Parser[Sentence] = boolExp | array
	protected lazy val boolExp = opChain(boolOp, eqExp)
	protected lazy val eqExp = "!".? ~ opChain(eqOp, addExp) ^^ { case not ~ exp => not.fold(exp){ _ => Not(exp) } }
	protected lazy val addExp = opChain(addOp, mulExp)
	protected lazy val mulExp = opChain(mulOp, primaryExp)
	protected lazy val primaryExp: Parser[Sentence] = "(" ~> sentence <~ ")" | number | accessChain | ("-" ~> primaryExp ^^ { Mul(N(-1), _) })

	protected lazy val args = "(" ~> repsep(sentence, sentenceSep) <~ ")"
	protected lazy val at = "[" ~> sentence <~ "]" ^^ { arg => (arr: Sentence) => At(arr, arg) }
	protected lazy val messageOrSlot = "." ~> identifier ~ args.? ^^ {
		case msg ~ args => { (prev: Sentence) => args.fold(Get(prev, msg): Sentence)(Send(prev, msg, _)) }
	}
	protected lazy val accessChain = (reference | array) ~ (at | messageOrSlot).* ^^ {
		case e ~ as => ((e: Sentence) /: as) { (p, a) => a(p) }
	}

	protected lazy val putOrSet = accessChain ~ (assignOp ~> sentence) <~ lineSep ^^ {
		case At(a, k) ~ v => Put(a, k, v)
		case Get(o, a) ~ v => Set(o, a, v)
	}

	protected lazy val ifExp: Parser[Sentence] = ("if" ~> "(" ~> sentence <~ ")") ~ codeBlock ~ ("else" ~> codeBlock).? ^^ {
		case c ~ t ~ f => If(c, t, f.getOrElse(Nil))
	}
	protected lazy val whileExp: Parser[Sentence] = ("while" ~> "(" ~> sentence <~ ")") ~ codeBlock ^^ { case c ~ b => While(c, b) }

	protected lazy val methodDef: Parser[M] = "def" ~> identifier ~ ("(" ~> repsep(identifier, sentenceSep) <~ ")") ~ codeBlock ^^ { case n ~ as ~ b => M(n, as, b) }

	protected lazy val objectDef = "object" ~> identifier ~ ("{" ~> (methodDef | putOrSet).* <~ "}") ^^ { case n ~ b => O(n, b) }

}