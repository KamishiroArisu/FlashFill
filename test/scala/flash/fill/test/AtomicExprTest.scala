package flash.fill.test

import flash.fill.data._
import org.scalatest.funsuite.AnyFunSuite

class AtomicExprTest extends AnyFunSuite {
    test("intersect1") {
        val ss1 = SubStr(0, List(CPos(3), CPos(-5)), List(CPos(7), CPos(-1)))
        val ss2 = SubStr(0, List(CPos(3), CPos(-9)), List(CPos(7), CPos(-5)))
        val result = ss1.intersect(ss2)
        assert(result.nonEmpty)
        result.get match {
            case SubStr(vi, p1, p2) =>
                assert(vi == 0)
                assert(p1.size == 1)
                assert(p1.head == CPos(3))
                assert(p2.size == 1)
                assert(p2.head == CPos(7))
        }
    }

    test("intersect2") {
        val re1 = TokenSeq(List(Set(SpecialToken('-'), NotCPlus(NumericDigits))))
        val re2 = TokenSeq(List(Set(CPlus(NumericDigits), CPlus(Alphanumeric))))
        val re3 = TokenSeq(List(Set(NotCPlus(Alphanumeric), NotCPlus(NumericDigits))))
        val re4 = TokenSeq(List(Set(CPlus(NumericDigits))))

        val p1 = Pos(re1, re2, ConstInt(1))
        val p2 = Pos(re2, re1, ConstInt(-1))
        val p3 = Pos(re3, re4, ConstInt(1))
        val p4 = Pos(re4, re3, ConstInt(-1))

        val ss1 = SubStr(0, List(CPos(4), p1), List(CPos(7), p2))
        val ss2 = SubStr(0, List(CPos(4), CPos(-9), p3), List(CPos(7), CPos(-6), p4))
        val result = ss1.intersect(ss2)
        assert(result.nonEmpty)
        result.get match {
            case SubStr(vi, p1, p2) =>
                assert(vi == 0)
                assert(p1.size == 2)
                assert(p1.contains(CPos(4)))
                assert(p1.contains(Pos(TokenSeq(List(Set(NotCPlus(NumericDigits)))), TokenSeq(List(Set(CPlus(NumericDigits)))), ConstInt(1))))
                assert(p2.size == 2)
                assert(p2.contains(CPos(7)))
                assert(p2.contains(Pos(TokenSeq(List(Set(CPlus(NumericDigits)))), TokenSeq(List(Set(NotCPlus(NumericDigits)))), ConstInt(-1))))
        }
    }

    test("size") {
        val re1 = TokenSeq(List(Set(SpecialToken('-'), NotCPlus(NumericDigits))))
        val re2 = TokenSeq(List(Set(CPlus(NumericDigits), CPlus(Alphanumeric))))
        val re3 = TokenSeq(List(Set(NotCPlus(Alphanumeric), NotCPlus(NumericDigits))))
        val re4 = TokenSeq(List(Set(CPlus(NumericDigits))))

        val p1 = Pos(re1, re2, ConstInt(1))
        val p2 = Pos(re2, re1, ConstInt(-1))
        val p3 = Pos(re3, re4, ConstInt(1))
        val p4 = Pos(re4, re3, ConstInt(-1))

        val ss1 = SubStr(0, List(CPos(4), p1), List(CPos(7), p2))
        val ss2 = SubStr(0, List(CPos(4), CPos(-9), p3), List(CPos(7), CPos(-6), p4))

        assert(ss1.size == 25)
        assert(ss2.size == 16)

        assert(ConstStr("123").size == 1)
    }
}
