package flash.fill.data

import flash.fill.Ranking

sealed trait AtomicExpr {
    type InputState = List[String]

    def intersect(ae: AtomicExpr): Option[AtomicExpr]
    def size: Long
    def eval(inputState: InputState): Option[String]
    def minimize: AtomicExpr
}

case class SubStr(vi: Int, p1: List[Position], p2: List[Position]) extends AtomicExpr {
    override def intersect(ae: AtomicExpr): Option[AtomicExpr] = ae match {
        case SubStr(vj, q1, q2) if vi == vj =>
            def fun(l1: List[Position], l2: List[Position]): List[Position] = for {
                x <- l1
                y <- l2
                p <- x.intersect(y)
            } yield p
            val pos1 = fun(p1, q1)
            val pos2 = fun(p2, q2)
            if (pos1.isEmpty || pos2.isEmpty) None else Some(SubStr(vi, pos1, pos2))
        case _ => None
    }

    lazy val size: Long = p1.map(_.size).sum * p2.map(_.size).sum

    override def eval(inputState: InputState): Option[String] = {
        for {
            from <- p1.head.eval(inputState(vi))
            to <- p2.head.eval(inputState(vi))
        } yield inputState(vi).substring(from, to)
    }

    override def minimize: AtomicExpr = SubStr(vi, List(p1.map(_.minimize).max(Ranking.positionRanking)), List(p2.map(_.minimize).max(Ranking.positionRanking)))
}

case class ConstStr(s: String) extends AtomicExpr {
    override def intersect(ae: AtomicExpr): Option[AtomicExpr] = ae match {
        case ConstStr(s2) if s2 == s => Some(ConstStr(s))
        case _ => None
    }

    override def size: Long = 1

    override def eval(inputState: InputState): Option[String] = Some(s)

    override def minimize: AtomicExpr = this
}

// unsupported yet
case class Loop(e: TraceExpr) extends AtomicExpr {
    override def intersect(ae: AtomicExpr): Option[AtomicExpr] = None

    override def size: Long = 1

    override def eval(inputState: InputState): Option[String] = None

    override def minimize: AtomicExpr = this
}