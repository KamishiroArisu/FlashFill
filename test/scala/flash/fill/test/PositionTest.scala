package flash.fill.test

import flash.fill.data._
import org.scalatest.funsuite.AnyFunSuite

class PositionTest extends AnyFunSuite {
    test("eval1") {
        val string = "192.168.0.1"
        val left = TokenSeq(List(Set(CPlus(NumericDigits)), Set(SpecialToken('.'))))
        val right = TokenSeq(List(Set(CPlus(NumericDigits))))

        assert(Pos(left, right, ConstInt(1)).eval(string).contains(4))
        assert(Pos(left, right, ConstInt(2)).eval(string).contains(10))
        assert(Pos(left, right, ConstInt(-2)).eval(string).contains(4))
        assert(Pos(left, right, ConstInt(-1)).eval(string).contains(10))
        assert(Pos(left, right, ConstInt(0)).eval(string).isEmpty)
        assert(Pos(left, right, ConstInt(3)).eval(string).isEmpty)
        assert(Pos(left, right, ConstInt(-3)).eval(string).isEmpty)
    }

    test("eval2") {
        val string = "192.168.0.1"
        val left = TokenSeq(List(Set(CPlus(NumericDigits)), Set(SpecialToken('.'))))
        val right = TokenSeq(List())

        assert(Pos(left, right, ConstInt(1)).eval(string).contains(4))
        assert(Pos(left, right, ConstInt(2)).eval(string).contains(8))
        assert(Pos(left, right, ConstInt(3)).eval(string).contains(10))
        assert(Pos(left, right, ConstInt(-3)).eval(string).contains(4))
        assert(Pos(left, right, ConstInt(-2)).eval(string).contains(8))
        assert(Pos(left, right, ConstInt(-1)).eval(string).contains(10))
        assert(Pos(left, right, ConstInt(0)).eval(string).isEmpty)
    }

    test("size") {
        val r1 = TokenSeq(List(Set(CPlus(NumericDigits))))
        val r2 = TokenSeq(List(Set(CPlus(NumericDigits), NotCPlus(Alphabets))))
        val r3 = TokenSeq(List(Set(CPlus(NumericDigits), NotCPlus(Alphabets)), Set(SpecialToken('-'), CPlus(UppercaseAlphabets)), Set(CPlus(Whitespace))))

        assert(CPos(-1).size == 1)
        assert(CPos(127).size == 1)
        assert(Pos(r1, r2, ConstInt(2)).size == 2)
        assert(Pos(r2, r3, ConstInt(-1)).size == 8)
    }
}
