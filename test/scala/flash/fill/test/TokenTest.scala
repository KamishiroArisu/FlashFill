package flash.fill.test

import flash.fill.data.{CPlus, NumericDigits}
import org.scalatest.funsuite.AnyFunSuite

class TokenTest extends AnyFunSuite {
    test("CPlus") {
        val token = CPlus(NumericDigits)
        assert(token.matchUntil("123456", 0) == 6)
        assert(token.matchUntil("123456", 3) == 6)
        assert(token.matchUntil("12345stop", 0) == 5)
        assert(token.matchUntil("12345stop", 2) == 5)
        assert(token.matchUntil("no", 0) == 0)
        assert(token.matchUntil("no", 1) == 1)
    }


}
