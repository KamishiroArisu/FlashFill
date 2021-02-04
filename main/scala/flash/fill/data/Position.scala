package flash.fill.data

sealed trait Position {
    type InputState = List[String]

    def intersect(p: Position): Option[Position]
    def size: Long
    def eval(s: String): Option[Int]
    def minimize: Position
}

case class CPos(k: Int) extends Position {
    override def intersect(p: Position): Option[Position] = p match {
        case CPos(k2) if k2 == k => Some(CPos(k))
        case _ => None
    }

    val size: Long = 1

    override def eval(s: String): Option[Int] =
        if (k >= 0) Some(k) else Some(s.length + k)

    override def minimize: Position = this
}

case class Pos(r1: RegularExpression, r2: RegularExpression, c: IntegerExpr) extends Position {
    override def intersect(p: Position): Option[Position] = p match {
        case Pos(r3, r4, d) => for {
            x <- r1.intersect(r3)
            y <- r2.intersect(r4)
            e <- c.intersect(d)
        } yield Pos(x, y, e)
        case _ => None
    }

    lazy val size: Long = r1.size * r2.size * c.size

    override def eval(s: String): Option[Int] = {
        val positions = (r1, r2) match {
            case (TokenSeq(l1), TokenSeq(l2)) if l1.isEmpty && l2.isEmpty =>
                List.range(0, s.length)   // use CPos(x) instead
            case (TokenSeq(l1), TokenSeq(_)) if l1.isEmpty =>
                r2.findAllMatches(s).map(_._1).sorted
            case (TokenSeq(_), TokenSeq(l2)) if l2.isEmpty =>
                r1.findAllMatches(s).map(_._2).sorted
            case _ =>
                val candidates = for {
                    (begin, left) <- r1.findAllMatches(s)
                    (right, end) <- r2.findAllMatches(s)
                    if left == right
                } yield (begin, left, end)

                // eliminate the false matches because of the overlap
                val eliminated = candidates.foldLeft(List.empty[(Int, Int, Int)])((z, c) => if (z.isEmpty || c._1 >= z.head._3) c :: z else z)
                eliminated.map(_._2).sorted
        }

        val k = c.eval
        if (0 < k && k <= positions.size)
            Some(positions(k - 1))
        else if (0 < -k && -k <= positions.size)
            Some(positions(positions.size + k))
        else
            None
    }

    override def minimize: Position = (r1, r2) match {
        case (l: TokenSeq, r: TokenSeq) => Pos(l.minimize, r.minimize, c)
    }
}
