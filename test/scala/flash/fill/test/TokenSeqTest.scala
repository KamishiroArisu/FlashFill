package flash.fill.test

import flash.fill.data._
import org.scalatest.funsuite.AnyFunSuite

class TokenSeqTest extends AnyFunSuite {
    test("findAllMatches") {
        val result1 = TokenSeq(List(
            CPlus(NumericDigits),
            SpecialToken('-'),
            CPlus(Alphabets),
            SpecialToken('-'),
            CPlus(NumericDigits)).map(Set(_))).
            findAllMatches("123-qwer-456-stop-7890")
        assert(result1.size == 1)
        assert(result1.head == (0, 12))

        val result2 = TokenSeq(List(
            CPlus(NumericDigits),
            SpecialToken('-'),
            CPlus(Alphabets)).map(Set(_))).
            findAllMatches("123-qwer-4567-more-890")
        assert(result2.size == 2)
        assert(result2(0) == (0, 8))
        assert(result2(1) == (9, 18))

        val result3 = TokenSeq(List(
            CPlus(Alphabets),
            CPlus(NumericDigits)).map(Set(_))).
            findAllMatches("123qwer4567 more890")
        assert(result3.size == 2)
        assert(result3(0) == (3, 11))
        assert(result3(1) == (12, 19))

        val result4 = TokenSeq(List(
            CPlus(NumericDigits),
            NotCPlus(Alphanumeric)).map(Set(_))).
            findAllMatches("192.168.0.1")
        assert(result4.size == 3)
        assert(result4(0) == (0, 4))
        assert(result4(1) == (4, 8))
        assert(result4(2) == (8, 10))
    }

    test("size") {
        assert(TokenSeq(List(Set(CPlus(NumericDigits)))).size == 1)
        assert(TokenSeq(List(Set(CPlus(NumericDigits), NotCPlus(Alphabets)))).size == 2)
        assert(TokenSeq(List(Set(CPlus(NumericDigits), NotCPlus(Alphabets)), Set(SpecialToken('-'), CPlus(UppercaseAlphabets)), Set(CPlus(Whitespace)))).size == 4)
    }
}
