package org.uqbar.less.ide

case class PreferenceFixture(preferences: Map[Preference[_], _]) {
	lazy val categories = preferences.keySet.map(_.category).toSeq.sorted
	def apply[T](category: String, name: String) = preferences.collect{ case (p, v) if p.category == category && p.name == name => v }.head.asInstanceOf[T]
	def updated(preference: Preference[_], value: Any) = copy(preferences.updated(preference, value))
}

trait Preference[T] {
	def name: String
	def category: String
	def default: T

	def apply(value: Any) = value.asInstanceOf[T]
}
case class BooleanPreference(name: String, category: String, default: Boolean = false) extends Preference[Boolean]
case class IntPreference(name: String, category: String, default: Int = 0, range: Option[Range] = None) extends Preference[Int]
case class StringPreference(name: String, category: String, default: String = "", validValues: Option[Seq[String]] = None) extends Preference[String]