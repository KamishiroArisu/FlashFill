package flash.fill.data

sealed trait CharacterClass {
    def matches(c: Char): Boolean
    val size: Int
}

case object NumericDigits extends CharacterClass {
    override def matches(c: Char): Boolean = c.isDigit
    override def toString: String = "0-9"

    override val size: Int = 10
}

case object Alphabets extends CharacterClass {
    override def matches(c: Char): Boolean = c.isLetter
    override def toString: String = "a-zA-Z"

    override val size: Int = 52
}

case object LowercaseAlphabets extends CharacterClass {
    override def matches(c: Char): Boolean = c.isLower
    override def toString: String = "a-z"

    override val size: Int = 26
}

case object UppercaseAlphabets extends CharacterClass {
    override def matches(c: Char): Boolean = c.isUpper
    override def toString: String = "A-Z"

    override val size: Int = 26
}

case object Alphanumeric extends CharacterClass {
    override def matches(c: Char): Boolean = c.isLetterOrDigit
    override def toString: String = "a-zA-Z0-9"

    override val size: Int = 62
}

case object Whitespace extends CharacterClass {
    override def matches(c: Char): Boolean = c == ' ' || c == '\n' || c == '\r' || c == '\t'
    override def toString: String = " \\n\\r\\t"

    override val size: Int = 4
}

object CharacterClass {
    lazy val getAll = Set(NumericDigits, Alphabets, LowercaseAlphabets, UppercaseAlphabets, Alphanumeric, Whitespace)
}