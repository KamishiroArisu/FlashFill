package flash.fill.data

sealed trait Token {
    def matchUntil(s: String, begin: Int): Int
}

case class SpecialToken(c: Char) extends Token {
    override def matchUntil(s: String, begin: Int): Int =
        if (s.charAt(begin) == c) begin + 1 else begin

    override def toString: String = c.toString
}

case class CPlus(characterClass: CharacterClass) extends Token {
    override def matchUntil(s: String, begin: Int): Int = {
        if (begin >= s.length || !characterClass.matches(s.charAt(begin)))
            begin
        else
            matchUntil(s, begin + 1)
    }

    override def toString: String = s"[$characterClass]+"
}

case class NotCPlus(characterClass: CharacterClass) extends Token {
    override def matchUntil(s: String, begin: Int): Int = {
        if (begin >= s.length || characterClass.matches(s.charAt(begin)))
            begin
        else
            matchUntil(s, begin + 1)
    }

    override def toString: String = s"[^$characterClass]+"
}

object Token {
    lazy val getAll: Set[Token] =
        Set('-', '.', ';', ':', ',', '/', '\\', '(', ')', '[', ']').map(SpecialToken) ++
            CharacterClass.getAll.map(CPlus) ++
            CharacterClass.getAll.map(NotCPlus)
}