package flash.fill

import flash.fill.data.{AtomicExpr, CPlus, CPos, ConstStr, NotCPlus, Pos, Position, SpecialToken, SubStr, Token}

object Ranking {
    val positionRanking: Ordering[Position] = new Ordering[Position] {
        override def compare(x: Position, y: Position): Int = (x, y) match {
            case (CPos(l), CPos(r)) => l - r
            case (CPos(_), Pos(_, _, _)) => 1
            case (Pos(_, _, _), CPos(_)) => -1
            case _ => if (y.size > x.size) 1 else -1
        }
    }

    val tokenRanking: Ordering[Token] = new Ordering[Token] {
        override def compare(x: Token, y: Token): Int = (x, y) match {
            case (SpecialToken(l), SpecialToken(r)) => l - r
            case (SpecialToken(_), _) => 1
            case (_, SpecialToken(_)) => -1
            case (NotCPlus(l), NotCPlus(r)) => r.size - l.size
            case (CPlus(l), CPlus(r)) => l.size - r.size
            case (CPlus(_), _) => 1
            case (_, CPlus(_)) => -1
        }
    }

    val atomicExprListRanking: Ordering[List[AtomicExpr]] = new Ordering[List[AtomicExpr]] {
        override def compare(x: List[AtomicExpr], y: List[AtomicExpr]): Int = {
            if (x.length < y.length)
                1
            else if (x.length > y.length)
                -1
            else
                x.count(ae => ae.isInstanceOf[SubStr]) - y.count(ae => ae.isInstanceOf[SubStr])
        }
    }

    val atomicExprRanking: Ordering[AtomicExpr] = new Ordering[AtomicExpr] {
        override def compare(x: AtomicExpr, y: AtomicExpr): Int = (x, y) match {
            case (ConstStr(l), ConstStr(r)) => l.length - r.length
            case (SubStr(_, _, _), ConstStr(_)) => 1
            case (ConstStr(_), SubStr(_, _, _)) => -1
            case (SubStr(_, p1, p2), SubStr(_, p3, p4)) => if ((p1.head.size + p2.head.size) > (p3.head.size + p4.head.size)) 1 else -1
        }
    }
}
