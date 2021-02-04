package flash.fill.test

import flash.fill.Algorithm
import flash.fill.data._
import org.scalatest.funsuite.AnyFunSuite

class TraceExprTest extends AnyFunSuite {
    test("size") {
        val nodes: Set[Node] = Set.range(0, 3).map(IntNode)
        val source = IntNode(0)
        val target = IntNode(3)
        val edges = for {
            i <- 0 to 2
            j <- i to 3
        } yield PairNode(IntNode(i), IntNode(j))

        val r1 = TokenSeq(List(Set(CPlus(NumericDigits))))
        val r2 = TokenSeq(List(Set(CPlus(NumericDigits), NotCPlus(Alphabets))))
        val r3 = TokenSeq(List(Set(CPlus(NumericDigits), NotCPlus(Alphabets)), Set(SpecialToken('-'), CPlus(UppercaseAlphabets)), Set(CPlus(Whitespace))))

        val p1 = Pos(r1, r2, ConstInt(2))
        val p2 = Pos(r2, r3, ConstInt(-1))

        val w: Map[PairNode, Set[AtomicExpr]] = Map(
            PairNode(IntNode(0), IntNode(1)) -> Set(ConstStr("a"), SubStr(0, List(CPos(0), CPos(-3)), List(CPos(1), CPos(-2)))),
            PairNode(IntNode(0), IntNode(2)) -> Set(ConstStr("ab"), SubStr(0, List(CPos(11), p1), List(p2))),
            PairNode(IntNode(0), IntNode(3)) -> Set(ConstStr("abc"), SubStr(0, List(p1), List(p2))),
            PairNode(IntNode(1), IntNode(2)) -> Set(ConstStr("b"), SubStr(0, List(p1), List(p2))),
            PairNode(IntNode(1), IntNode(3)) -> Set(ConstStr("bc"), SubStr(0, List(CPos(11), p1), List(p2))),
            PairNode(IntNode(2), IntNode(3)) -> Set(ConstStr("c"), SubStr(0, List(p1), List(p2)))
        )

        val dag = Dag(source, target, edges.toSet, w)
        assert(dag.size == 2012)
    }

    test("getConcatenate") {
        val algorithm = Algorithm()
        val string1 = "2022-07-18"
        val extract1 = "07-18"
        val result1 = algorithm.generateStr(List(string1), extract1).get

        val string2 = "2021-12-30"
        val extract2 = "12-30"
        val result2 = algorithm.generateStr(List(string2), extract2).get

        val result = result1.intersect(result2)
        assert(result.nonEmpty)
        val dag = result.get
        assert(dag.asInstanceOf[Dag].getConcatenate.get.eval(List("1992-08-15")).contains("08-15"))
    }
}
