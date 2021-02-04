package flash.fill.data

case class Conjunct(predicates: List[Predicate]) {
    type InputState = List[String]

    def eval(inputState: InputState): Boolean = {
        predicates.forall(p => p.eval(inputState))
    }
}
