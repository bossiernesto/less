package org.uqbar.less

import scala.language.implicitConversions

object SemanticModel {

	case class ID(value: Symbol)

	sealed trait Sentence

	sealed trait Entity extends Sentence
	case class R(id: ID) extends Entity
	case class O(id: ID, body: Seq[Sentence]) extends Entity
	case class M(id: ID, args: Seq[ID], body: Seq[Sentence]) extends Entity
	case class A(values: Seq[Sentence]) extends Entity
	case class N(value: Int) extends Entity

	case class Assign(target: ID, value: Sentence) extends Sentence
	case class Eq(left: Sentence, right: Sentence) extends Sentence

	case class Get(targetR: Sentence, slotName: ID) extends Sentence
	case class Set(targetR: Sentence, slotName: ID, value: Sentence) extends Sentence
	case class Send(targetR: Sentence, messageName: ID, arguments: Seq[Sentence]) extends Sentence

	case class At(targetA: Sentence, indexN: Sentence) extends Sentence
	case class Put(targetA: Sentence, indexN: Sentence, value: Sentence) extends Sentence

	case class Add(leftN: Sentence, rightN: Sentence) extends Sentence
	case class Sub(leftN: Sentence, rightN: Sentence) extends Sentence
	case class Mul(leftN: Sentence, rightN: Sentence) extends Sentence
	case class Div(leftN: Sentence, rightN: Sentence) extends Sentence
	case class Greater(leftN: Sentence, rightN: Sentence) extends Sentence
	case class GreaterOrEq(leftN: Sentence, rightN: Sentence) extends Sentence
	case class Lesser(leftN: Sentence, rightN: Sentence) extends Sentence
	case class LesserOrEq(leftN: Sentence, rightN: Sentence) extends Sentence

	case class Not(conditionN: Sentence) extends Sentence
	case class Or(leftN: Sentence, rightN: Sentence) extends Sentence
	case class And(leftN: Sentence, rightN: Sentence) extends Sentence
	case class If(conditionN: Sentence, bodyTrue: Seq[Sentence], bodyFalse: Seq[Sentence]) extends Sentence
	case class While(conditionN: Sentence, body: Seq[Sentence]) extends Sentence
}

object Preety {
	import SemanticModel._

	implicit def Symbol_to_ID(s: Symbol) = ID(s)
	implicit def Symbol_to_R(s: Symbol) = R(ID(s))
	implicit def Int_to_N(n: Int) = N(n)
	def %(ss: Sentence*) = new A(ss)
}