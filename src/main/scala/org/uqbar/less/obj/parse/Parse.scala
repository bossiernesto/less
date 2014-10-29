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
	protected lazy val array = "[" ~> repsep(sentence, sentenceSep) <~ "]" ^^ { A(_) }
	protected lazy val reference = identifier ^^ { R(_) }

	protected lazy val expression = eqExp
	protected lazy val eqExp = addExp ~ ("==" ~> addExp).* ^^ { case l ~ r => (l /: r)(Eq) }
	protected lazy val addExp = mulExp ~ ("+" ~> mulExp).* ^^ { case l ~ r => (l /: r)(Add) }
	protected lazy val mulExp = primaryExp ~ ("*" ~> primaryExp).* ^^ { case l ~ r => (l /: r)(Mul) }
	protected lazy val primaryExp: Parser[Sentence] = "(" ~> expression <~ ")" | number | reference | ("-" ~> primaryExp ^^ { Mul(N(-1), _) })

	protected lazy val asign = (identifier <~ assignOp).? ~ sentence <~ lineSep ^^ { case maybeId ~ value => maybeId.fold(value)(Assign(_, value)) }

	protected lazy val sentence: Parser[Sentence] = expression | array

	protected lazy val params = "(" ~> repsep(sentence, sentenceSep) <~ ")"
	protected lazy val messageSend = (sentence <~ ".") ~ identifier ~ params ^^ { case obj ~ msg ~ params => Send(obj, msg, params) }

}