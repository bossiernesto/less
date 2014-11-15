package org.uqbar.less.encoder

import scala.language.implicitConversions
import org.uqbar.less.SemanticModel._
import org.uqbar.less.Syntax

//TODO: numbers, precedence and identifiers ignores syntax rules
object Encode extends Encode { def _syntax = Syntax }
trait Encode {
	def apply(sentences: Sentence*)(implicit prefs: PreferenceFixture) = {
		val r = sentences.map(s => encode(s)(prefs) ++ lineSeparator(s)).mkString("")
		if (r.endsWith("\n")) r.init else r
	}

	def _syntax: Syntax
	def syntax(key: Symbol) = _syntax(key).replace("""\""", """""")

	protected def encode(sentence: Sentence, level: Int = 0)(implicit prefs: PreferenceFixture): String = {
		val tabulation = (if (prefs("Tabulation", "Tabulate with Tabs")) "\t" else " " * prefs[Int]("Tabulation", "Tabulation Size")) * level
		def s(preferenceName: String, otherCondition: Boolean = true) = if (prefs[Boolean]("Spacing", preferenceName) && otherCondition) " " else ""

		val content = sentence match {
			case R(id) => id.value.name

			case O(id, body) =>
				syntax('objectKeyword) ++ " " ++ id.value.name ++ s("After object name") ++ syntax('codeOpen) ++ "\n" ++
					body.flatMap(s => encode(s, level + 1) ++ lineSeparator(s)) ++
					tabulation ++ syntax('codeClose)

			case M(id, args, body) =>
				syntax('defKeyword) ++ " " ++ id.value.name ++ s("After method name") ++
					args.map(_.value.name).mkString(syntax('parensOpen) ++ s("Before each method argument", args.nonEmpty), s("Before each method argument") ++ syntax('sentenceSep) ++ s("After each method argument"), syntax('parensClose)) ++ s("After method arguments") ++
					syntax('codeOpen) ++ "\n" ++
					body.flatMap(s => encode(s, level + 1) ++ lineSeparator(s)) ++
					tabulation ++ syntax('codeClose)

			case A(values) => values.map(encode(_)).mkString(syntax('arrayOpen), s("Before each array argument") ++ syntax('sentenceSep) ++ s("After each array argument"), syntax('arrayClose))

			case N(value) =>
				value.toString

			case Assign(target, value) => target.value.name ++ s("Before assign") ++ syntax('assignOp) ++ s("After assign") ++ encode(value)

			case Eq(left, right) => encode(left) ++ s("Before operator") ++ syntax('eqOp) ++ s("After operator") ++ encode(right)

			case Get(targetR, slotName) => encode(targetR) ++ syntax('access) ++ slotName.value.name
			case Set(targetR, slotName, value) => encode(targetR) ++ syntax('access) ++ slotName.value.name ++ " " ++ syntax('assignOp) ++ " " ++ encode(value)
			case Send(targetR, messageName, arguments) => encode(targetR) ++ syntax('access) ++ messageName.value.name ++ s("After message name") ++
				arguments.map(encode(_)).mkString(syntax('argOpen) ++ s("Before each message argument", arguments.nonEmpty), s("Before each message argument") ++ syntax('sentenceSep) ++ s("After each message argument"), syntax('argClose))

			case At(targetA, indexN) => encode(targetA) ++ syntax('atOpen) ++ s("Before array index") ++ encode(indexN) ++ s("After array index") ++ syntax('atClose)
			case Put(targetA, indexN, value) => encode(targetA) ++ syntax('atOpen) ++ s("Before array index") ++ encode(indexN) ++ s("After array index") ++ syntax('atClose) ++ s("Before assign") ++ syntax('assignOp) ++ s("After assign") ++ encode(value)

			case Add(leftN, rightN) => encode(leftN) ++ s("Before operator") ++ syntax('addOp) ++ s("After operator") ++ encode(rightN)
			case Sub(leftN, rightN) => encode(leftN) ++ s("Before operator") ++ syntax('subOp) ++ s("After operator") ++ encode(rightN)
			case Mul(leftN, rightN) => encode(leftN) ++ s("Before operator") ++ syntax('mulOp) ++ s("After operator") ++ encode(rightN)
			case Div(leftN, rightN) => encode(leftN) ++ s("Before operator") ++ syntax('divOp) ++ s("After operator") ++ encode(rightN)
			case Greater(leftN, rightN) => encode(leftN) ++ s("Before operator") ++ syntax('gtOp) ++ s("After operator") ++ encode(rightN)
			case GreaterOrEq(leftN, rightN) => encode(leftN) ++ s("Before operator") ++ syntax('geOp) ++ s("After operator") ++ encode(rightN)
			case Lesser(leftN, rightN) => encode(leftN) ++ s("Before operator") ++ syntax('ltOp) ++ s("After operator") ++ encode(rightN)
			case LesserOrEq(leftN, rightN) => encode(leftN) ++ s("Before operator") ++ syntax('leOp) ++ s("After operator") ++ encode(rightN)

			case Not(conditionN) => syntax('notOp) ++ s("After not") ++ encode(conditionN)
			case Or(leftN, rightN) => encode(leftN) ++ s("Before operator") ++ syntax('orOp) ++ s("After operator") ++ encode(rightN)
			case And(leftN, rightN) => encode(leftN) ++ s("Before operator") ++ syntax('andOp) ++ s("After operator") ++ encode(rightN)
			case If(conditionN, bodyTrue, bodyFalse) => syntax('ifKeyword) ++ s("After if") ++ syntax('argOpen) ++ s("Before if condition") ++ encode(conditionN) ++ s("After if condition") ++ syntax('argClose) ++ s("After if argument") ++
				syntax('codeOpen) ++ "\n" ++ bodyTrue.map(s => encode(s, level + 1) ++ lineSeparator(s)).mkString("") ++ tabulation ++ syntax('codeClose) ++ s("Before else") ++
				syntax('elseKeyword) ++ s("After else") ++ syntax('codeOpen) ++ "\n" ++ bodyFalse.map(s => encode(s, level + 1) ++ lineSeparator(s)).mkString("") ++ "\n" ++ tabulation ++ syntax('codeClose)
			case While(conditionN, body) => syntax('whileKeyword) ++ s("After while") ++ syntax('argOpen) ++ s("Before while condition") ++ encode(conditionN) ++ s("After while condition") ++ syntax('argClose) ++ s("After while argument") ++
				syntax('codeOpen) ++ "\n" ++ body.map(s => encode(s, level + 1) ++ lineSeparator(s)).mkString("") ++ "\n" ++ tabulation ++ syntax('codeClose)
		}

		tabulation ++ content
	}

	protected def lineSeparator(s: Sentence) = s match {
		case (_: O | _: M | _: If | _: While) => "\n"
		case _ => syntax('lineSep) ++ "\n"
	}
}