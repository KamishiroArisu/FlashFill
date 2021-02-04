package flash.fill.data

sealed trait IntegerExpr {
    def intersect(ie: IntegerExpr): Option[IntegerExpr]
    def size: Long
    def eval: Int
}

case class ConstInt(k: Int) extends IntegerExpr {
    override def intersect(ie: IntegerExpr): Option[IntegerExpr] = ie match {
        case ConstInt(k2) if k2 == k => Some(ConstInt(k))
        case _ => None
    }

    override def size: Long = 1

    override def eval: Int = k
}

// unsupported yet
case class LoopInt(k1: Int, k2: Int) extends IntegerExpr {
    override def intersect(ie: IntegerExpr): Option[IntegerExpr] = None

    override def size: Long = 1

    override def eval: Int = 0
}