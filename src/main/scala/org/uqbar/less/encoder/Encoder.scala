package org.uqbar.less.encoder

import scala.language.implicitConversions
import org.uqbar.less.SemanticModel._
import org.uqbar.less.Syntax

//TODO: numbers, precedence and identifiers ignores syntax rules
object Encoder extends Encoder { def _syntax = Syntax }
trait Encoder {
	def apply(sentences: Sentence*) = sentences.map(encode(_)).mkString("\n")

	def _syntax: Syntax
	def syntax(key: Symbol) = _syntax(key).replace("""\""", """""")

	protected def encode(sentence: Sentence, level: Int = 0): String = {
		val tabulation = ("" /: (0 until level)){ (a, _) => a ++ " " }
		val content = sentence match {
			case R(id) => id.value.name

			case O(id, body) =>
				syntax('objectKeyword) ++ " " ++ id.value.name ++ " " ++ syntax('codeOpen) ++ "\n" ++
					body.flatMap(encode(_, level + 1)) ++ "\n" ++
					syntax('codeClose)

			case M(id, args, body) =>
				syntax('defKeyword) ++ " " ++ id.value.name ++
					args.map(_.value.name).mkString(syntax('parensOpen), syntax('sentenceSep) ++ " ", syntax('parensClose)) ++
					syntax('codeOpen) ++ "\n" ++
					body.flatMap(encode(_, level + 1)) ++ "\n" ++
					syntax('codeClose)

			case A(values) => values.map(encode(_)).mkString(syntax('arrayOpen), syntax('sentenceSep) ++ " ", syntax('arrayClose))

			case N(value) => value.toString

			case Assign(target, value) => target.value.name ++ " " ++ syntax('assignOp) ++ " " ++ encode(value) ++ syntax('lineSep)

			case Eq(left, right) => encode(left) ++ " " ++ syntax('eqOp) ++ " " ++ encode(right)

			case Get(targetR, slotName) => encode(targetR) ++ syntax('access) ++ slotName.value.name
			case Set(targetR, slotName, value) => encode(targetR) ++ syntax('access) ++ slotName.value.name ++ " " ++ syntax('assignOp) ++ " " ++ encode(value) ++ syntax('lineSep)
			case Send(targetR, messageName, arguments) => encode(targetR) ++ syntax('access) ++ messageName.value.name ++
				arguments.map(encode(_)).mkString(syntax('argOpen), syntax('sentenceSep) ++ " ", syntax('argClose))

			case At(targetA, indexN) => encode(targetA) ++ syntax('atOpen) ++ encode(indexN) ++ syntax('atClose)
			case Put(targetA, indexN, value) => encode(targetA) ++ syntax('atOpen) ++ encode(indexN) ++ syntax('atClose) ++ " " ++ syntax('assignOp) ++ " " ++ encode(value) ++ syntax('lineSep)

			case Add(leftN, rightN) => encode(leftN) ++ " " ++ syntax('addOp) ++ " " ++ encode(rightN)
			case Sub(leftN, rightN) => encode(leftN) ++ " " ++ syntax('subOp) ++ " " ++ encode(rightN)
			case Mul(leftN, rightN) => encode(leftN) ++ " " ++ syntax('mulOp) ++ " " ++ encode(rightN)
			case Div(leftN, rightN) => encode(leftN) ++ " " ++ syntax('divOp) ++ " " ++ encode(rightN)
			case Greater(leftN, rightN) => encode(leftN) ++ " " ++ syntax('gtOp) ++ " " ++ encode(rightN)
			case GreaterOrEq(leftN, rightN) => encode(leftN) ++ " " ++ syntax('geOp) ++ " " ++ encode(rightN)
			case Lesser(leftN, rightN) => encode(leftN) ++ " " ++ syntax('ltOp) ++ " " ++ encode(rightN)
			case LesserOrEq(leftN, rightN) => encode(leftN) ++ " " ++ syntax('leOp) ++ " " ++ encode(rightN)

			case Not(conditionN) => syntax('notOp) ++ " " ++ encode(conditionN)
			case Or(leftN, rightN) => encode(leftN) ++ " " ++ syntax('orOp) ++ " " ++ encode(rightN)
			case And(leftN, rightN) => encode(leftN) ++ " " ++ syntax('andOp) ++ " " ++ encode(rightN)
			case If(conditionN, bodyTrue, bodyFalse) => syntax('ifKeyword) ++ syntax('argOpen) ++ encode(conditionN) ++ syntax('argClose) ++
				bodyTrue.map(encode(_, level + 1)).mkString(syntax('codeOpen) ++ "\n", syntax('lineSep) ++ "\n", "\n" ++ syntax('codeClose) ++ "\n") ++
				syntax('elseKeyword) ++ " " ++ bodyFalse.map(encode(_, level + 1)).mkString(syntax('codeOpen) ++ "\n", syntax('lineSep) ++ "\n", "\n" ++ syntax('codeClose))
			case While(conditionN, body) => syntax('whileKeyword) ++ syntax('argOpen) ++ encode(conditionN) ++ syntax('argClose) ++
				body.map(encode(_, level + 1)).mkString(syntax('codeOpen) ++ "\n", syntax('lineSep) ++ "\n", "\n" ++ syntax('codeClose))
		}

		tabulation ++ content
	}
}