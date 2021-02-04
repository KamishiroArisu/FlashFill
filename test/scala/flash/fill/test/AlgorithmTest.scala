package flash.fill.test

import flash.fill.Algorithm
import flash.fill.data._
import org.scalatest.funsuite.AnyFunSuite

class AlgorithmTest extends AnyFunSuite {
    test("distinguish1") {
        val algorithm = Algorithm()
        val result = algorithm.distinguish(".")
        assert(result.size == 2)
        val set = result.values.find(_.contains(SpecialToken('.'))).get
        assert(set.size == 7)
        assert(set.contains(SpecialToken('.')))
        assert(set.contains(NotCPlus(Alphabets)))
        assert(set.contains(NotCPlus(Alphanumeric)))
        assert(set.contains(NotCPlus(UppercaseAlphabets)))
        assert(set.contains(NotCPlus(LowercaseAlphabets)))
        assert(set.contains(NotCPlus(NumericDigits)))
        assert(set.contains(NotCPlus(Whitespace)))
    }

    test("distinguish2") {
        val algorithm = Algorithm()
        val result = algorithm.distinguish("ip 127.0.0.1")
        assert(result.size == 11)
        val set1 = result.values.find(_.contains(SpecialToken('.'))).get
        assert(set1.size == 1)

        val set2 = result.values.find(_.contains(SpecialToken(']'))).get
        assert(set2.size == 11)
        assert(set2.contains(CPlus(UppercaseAlphabets)))

        val set3 = result.values.find(_.contains(CPlus(Alphabets))).get
        assert(set3.size == 2)
        assert(set3.contains(CPlus(LowercaseAlphabets)))
    }

    test("findAllRegularExpressionMatchesTo") {
        val algorithm = Algorithm()
        val tokens: Set[Token] = Set(CPlus(NumericDigits), CPlus(LowercaseAlphabets), SpecialToken('-'))
        val result = algorithm.findAllRegularExpressionMatchesTo("127-0-0-1", tokens)
        assert(result.size == 7)
        assert(result.exists(t => t._1 == TokenSeq(List(Set(CPlus(NumericDigits)))) && t._2 == 2))
        assert(result.exists(t => t._1 == TokenSeq(List(Set(CPlus(NumericDigits)), Set(SpecialToken('-')))) && t._2 == 3))
        assert(result.exists(t => t._1 == TokenSeq(List(Set(CPlus(NumericDigits)), Set(SpecialToken('-')),
            Set(CPlus(NumericDigits)))) && t._2 == 4))
        assert(result.exists(t => t._1 == TokenSeq(List(Set(CPlus(NumericDigits)), Set(SpecialToken('-')),
            Set(CPlus(NumericDigits)), Set(SpecialToken('-')))) && t._2 == 5))
        assert(result.exists(t => t._1 == TokenSeq(List(Set(CPlus(NumericDigits)), Set(SpecialToken('-')),
            Set(CPlus(NumericDigits)), Set(SpecialToken('-')), Set(CPlus(NumericDigits)))) && t._2 == 6))
    }

    test("generatePosition") {
        val algorithm = Algorithm()
        val string = "192.168.0.1"

        for (i <- 0 until string.length) {
            val result = algorithm.generatePosition(string, i)
            assert(result.map(p => p.eval(string)).forall(opt => opt.contains(i)))
        }
    }

    test("generateSubstring") {
        val algorithm = Algorithm()
        val string = "2021-02-13 12:48"
        val extract = "02"

        val result = algorithm.generateSubstring(List(string), extract)
        assert(result.forall(ae => ae.eval(List(string)).contains(extract)))
    }

    test("generateStr") {
        val algorithm = Algorithm()
        val string = "2022-07-18 12:48"
        val extract = "07-18"

        val opt = algorithm.generateStr(List(string), extract)
        assert(opt.nonEmpty)
        val result = opt.get
        assert(result.source == IntNode(0))
        assert(result.target == IntNode(extract.length))
        val n = extract.length
        assert(result.edges.size == (n + 1) * n / 2)
        assert(result.w.keySet.size == result.edges.size)

        assert(result.edges.forall {
            case PairNode(IntNode(left), IntNode(right)) =>
                val list = result.w(PairNode(IntNode(left), IntNode(right)))
                list.forall(ae => ae.eval(List(string)).contains(extract.substring(left, right)))
            case _ => false
        })
    }
}
