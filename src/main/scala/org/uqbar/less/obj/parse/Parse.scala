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
	protected lazy val primaryExp: Parser[Sentence] = "(" ~> expression <~ ")" | number | messageChain | ("-" ~> primaryExp ^^ { Mul(N(-1), _) })

	protected lazy val arrayAt: Parser[Sentence] = ("(" ~> arrayAt <~ ")" | array | messageChain) ~ ("[" ~> sentence <~ "]").* ~ (".length").? ^^ {
		case a ~ ks ~ l =>
			val ar = (a /: ks)(At(_, _))
			l.fold(ar: Sentence){ _ => Length(ar) }
	}

	protected lazy val arrayPut = sentence ~ ("[" ~> sentence <~ "]") ~ (assignOp ~> sentence) <~ lineSep ^^ {
		case a ~ k ~ v => Put(a, k, v)
	}

	protected lazy val ifExp: Parser[Sentence] = ("if" ~> "(" ~> sentence <~ ")") ~ codeBlock ~ ("else" ~> codeBlock).? ^^ {
		case c ~ t ~ f => If(c, t, f.getOrElse(Nil))
	}
	protected lazy val whileExp: Parser[Sentence] = ("while" ~> "(" ~> sentence <~ ")") ~ codeBlock ^^ { case c ~ b => While(c, b) }

	protected lazy val assign = (identifier <~ assignOp).? ~ sentence <~ lineSep ^^ { case maybeId ~ value => maybeId.fold(value)(Assign(_, value)) }

	protected lazy val codeBlock = "{" ~> line.* <~ "}"
	protected lazy val line = ifExp | whileExp | arrayPut | assign | attributeSet | sentence <~ lineSep
	protected lazy val sentence: Parser[Sentence] = array | expression

	protected lazy val args = "(" ~> repsep(sentence, sentenceSep) <~ ")"
	protected lazy val messageChain = reference ~ ("." ~> identifier ~ args.?).* ^^ {
		case obj ~ msgs => ((obj: Sentence) /: msgs) { case (prev, msg ~ args) => args.fold(Get(prev, msg): Sentence)(Send(prev, msg, _)) }
	}
	protected lazy val attributeSet = messageChain ~ (assignOp ~> sentence) <~ lineSep ^^ {
		case Get(o, a) ~ v => Set(o, a, v)
	}

}