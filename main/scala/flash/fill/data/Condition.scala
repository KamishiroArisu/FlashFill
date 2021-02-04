package flash.fill.data

sealed trait Bool {
    type InputState = List[String]
    def eval(inputState: InputState): Boolean
}

case class Condition(conjuncts: List[Conjunct]) extends Bool {
    def eval(inputState: InputState): Boolean =
        conjuncts.exists(c => c.eval(inputState))
}

case object True extends Bool {
    override def eval(inputState: InputState): Boolean = true
}
