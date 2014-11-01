package org.uqbar.less.obj.parse

import scala.util.parsing.combinator._
import org.uqbar.less.obj._

object Parse extends Parse
trait Parse extends RegexParsers {

	protected lazy val identifier = "[a-z_A-Z]+".r ^^ { id => ID(Symbol(id)) }
	protected lazy val sentenceSep = ","
	protected lazy val assignOp = "="
	protected lazy val lineSep = ";"

	protected lazy val number = "-?[0-9]+".r ^^ { n => N(n.toInt) }
	protected lazy val array = "#[" ~> repsep(sentence, sentenceSep) <~ "]" ^^ { A(_) }
	protected lazy val reference = identifier ^^ { R(_) }

	protected lazy val boolOp: Parser[(Sentence, Sentence) => Sentence] =
		"&&" ^^ { _ => And } |
			"||" ^^ { _ => Or }

	protected lazy val eqOp: Parser[(Sentence, Sentence) => Sentence] =
		"==" ^^ { _ => Eq } |
			"!=" ^^ { _ => (l: Sentence, r: Sentence) => Not(Eq(l, r)) } |
			">=" ^^ { _ => GreaterOrEq } |
			"<=" ^^ { _ => LesserOrEq } |
			">" ^^ { _ => Greater } |
			"<" ^^ { _ => Lesser }

	protected lazy val addOp: Parser[(Sentence, Sentence) => Sentence] =
		"+" ^^ { _ => Add } |
			"-" ^^ { _ => Sub }

	protected lazy val mulOp: Parser[(Sentence, Sentence) => Sentence] =
		"*" ^^ { _ => Mul } |
			"/" ^^ { _ => Div }

	protected def opChain(op: Parser[(Sentence, Sentence) => Sentence], subLevel: Parser[Sentence]) =
		subLevel ~ (op ~ subLevel).* ^^ { case l ~ r => (l /: r){ case (l, o ~ r) => o(l, r) } }

	protected lazy val expression = boolExp
	protected lazy val boolExp = opChain(boolOp, eqExp)
	protected lazy val eqExp = "!".? ~ opChain(eqOp, addExp) ^^ { case not ~ exp => not.fold(exp){ _ => Not(exp) } }
	protected lazy val addExp = opChain(addOp, mulExp)
	protected lazy val mulExp = opChain(mulOp, primaryExp)
	protected lazy val primaryExp: Parser[Sentence] = "(" ~> expression <~ ")" | number | reference | ("-" ~> primaryExp ^^ { Mul(N(-1), _) })

	protected lazy val arrayPut = array ~ (("[" ~> sentence <~ "]") ~ (assignOp ~> sentence).?).? ^^ {
		case a ~ kv => kv.fold(a: Sentence){ case k ~ v => v.fold(At(a, k): Sentence){ Put(a, k, _) } }
	}

	protected lazy val ifExp: Parser[Sentence] = ("if" ~> "(" ~> sentence <~ ")") ~ codeBlock ~ ("else" ~> codeBlock).? ^^ {
		case c ~ t ~ f => If(c, t, f.getOrElse(Nil))
	}
	protected lazy val whileExp: Parser[Sentence] = ("while" ~> "(" ~> sentence <~ ")") ~ codeBlock ^^ { case c ~ b => While(c, b) }

	protected lazy val assign = (identifier <~ assignOp).? ~ sentence <~ lineSep ^^ { case maybeId ~ value => maybeId.fold(value)(Assign(_, value)) }

	protected lazy val line = ifExp | whileExp | assign | sentence <~ lineSep
	protected lazy val codeBlock = "{" ~> line.* <~ "}"
	protected lazy val sentence: Parser[Sentence] = expression | arrayPut

	protected lazy val params = "(" ~> repsep(sentence, sentenceSep) <~ ")"
	protected lazy val messageSend = (sentence <~ ".") ~ identifier ~ params ^^ { case obj ~ msg ~ params => Send(obj, msg, params) }

}