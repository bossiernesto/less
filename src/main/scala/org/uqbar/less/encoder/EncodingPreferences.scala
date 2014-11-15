package org.uqbar.less.encoder

//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// PREFERENCES
//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════

trait EncodingPreference {
	def category: String
	def name: String
}

case class BooleanPreference(category: String, name: String) extends EncodingPreference {
	def apply(value: Any) = value.asInstanceOf[Boolean]
}
case class IntPreference(category: String, name: String, range: Option[Range] = None) extends EncodingPreference {
	def apply(value: Any) = value.asInstanceOf[Int]
}

case class StringPreference(category: String, name: String, validValues: Option[Seq[String]] = None) extends EncodingPreference {
	def apply(value: Any) = value.asInstanceOf[String]
}

//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// PREFERENCE FIXTURES
//═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════

case class PreferenceFixture(entries: Map[EncodingPreference, Any]) {

	lazy val categories = entries.keySet.map(_.category).toSeq.sorted

	def get[T](category: String, name: String): Option[T] = entries.keys.find(p => p.category == category && p.name == name).flatMap(get[T])
	def get[T](preference: EncodingPreference) = entries.get(preference).map(_.asInstanceOf[T])

	def apply[T](category: String, name: String) = get[T](category, name).get
	def apply[T](preference: EncodingPreference) = get[T](preference).get

	def categoryPreferences(category: String) = entries.filter(_._1.category == category)

	def <=>(fixture: PreferenceFixture) = entries.keySet == fixture.entries.keySet

	def updated(preference: EncodingPreference, value: Any) = copy(entries.updated(preference, value))
}

object PreferenceFixture {
	val Default = PreferenceFixture(Map(
		IntPreference("Tabulation", "Tabulation Size", Some(1 to 12)) -> 2,
		BooleanPreference("Tabulation", "Tabulate with Tabs") -> true,
		BooleanPreference("Spacing", "After if") -> false,
		BooleanPreference("Spacing", "Before if condition") -> false,
		BooleanPreference("Spacing", "After if condition") -> false,
		BooleanPreference("Spacing", "After if argument") -> false,
		BooleanPreference("Spacing", "Before else") -> false,
		BooleanPreference("Spacing", "After else") -> false,
		BooleanPreference("Spacing", "After while") -> false,
		BooleanPreference("Spacing", "Before while condition") -> false,
		BooleanPreference("Spacing", "After while condition") -> false,
		BooleanPreference("Spacing", "After while argument") -> true,
		BooleanPreference("Spacing", "After not") -> false,
		BooleanPreference("Spacing", "After object name") -> false,
		BooleanPreference("Spacing", "After method name") -> false,
		BooleanPreference("Spacing", "After method arguments") -> false,
		BooleanPreference("Spacing", "Before each method argument") -> false,
		BooleanPreference("Spacing", "After each method argument") -> true,
		BooleanPreference("Spacing", "Before each array argument") -> false,
		BooleanPreference("Spacing", "After each array argument") -> true,
		BooleanPreference("Spacing", "Before assign") -> true,
		BooleanPreference("Spacing", "After assign") -> true,
		BooleanPreference("Spacing", "Before operator") -> true,
		BooleanPreference("Spacing", "After operator") -> true,
		BooleanPreference("Spacing", "Before array index") -> false,
		BooleanPreference("Spacing", "After array index") -> false,
		BooleanPreference("Spacing", "After message name") -> false,
		BooleanPreference("Spacing", "Before each message argument") -> false,
		BooleanPreference("Spacing", "After each message argument") -> true
	))
}