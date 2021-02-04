package flash.fill.test

import flash.fill.data.NumericDigits
import org.scalatest.funsuite.AnyFunSuite

class CharacterClassTest extends AnyFunSuite {
    test("NumericDigits") {
        assert(('0' to '9').forall(NumericDigits.matches))
        assert(!('a' to 'z').exists(NumericDigits.matches))
        assert(!('A' to 'Z').exists(NumericDigits.matches))
        assert(!NumericDigits.matches(' '))
        assert(!NumericDigits.matches('-'))
    }
}
