package flash.fill.data

case class Switch(cases: List[(Bool, TraceExpr)]) {
    lazy val getStringExpr: Option[StringExpr] = {
        val cons = cases.map { case (bool, dag: Dag) => (bool, dag.getConcatenate) }
        if (cons.exists(_._2.isEmpty))
            None
        else
            Some(StringExpr(cons.map { case (bool, optConcatenate) => (bool, optConcatenate.get) }))
    }
}

case class StringExpr(cases: List[(Bool, Concatenate)]) {
    type InputState = List[String]

    def eval(inputState: InputState): Option[String] = {
        val selected: Option[(Bool, Concatenate)] = cases.find(c => c._1.eval(inputState))
        selected.flatMap(_._2.eval(inputState))
    }
}
