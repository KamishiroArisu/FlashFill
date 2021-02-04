package flash.fill.data

sealed trait Predicate {
    type InputState = List[String]
    def eval(inputState: InputState): Boolean
}

case class Match(vi: Int, r: RegularExpression, k: Int) extends Predicate {
    override def eval(inputState: InputState): Boolean =
        r.findAllMatches(inputState(vi)).size == k
}

case class NotMatch(vi: Int, r: RegularExpression, k: Int) extends Predicate {
    override def eval(inputState: InputState): Boolean =
        r.findAllMatches(inputState(vi)).size != k
}
