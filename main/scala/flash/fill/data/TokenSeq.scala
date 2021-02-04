package flash.fill.data

import flash.fill.Ranking

sealed trait RegularExpression {
    def findAllMatches(s: String): List[(Int, Int)]
    def append(re: RegularExpression): RegularExpression
    def intersect(re: RegularExpression): Option[RegularExpression]
    def size: Long
    def minimize: RegularExpression
}

case class TokenSeq(tokenSets: List[Set[Token]]) extends RegularExpression {
    override def findAllMatches(s: String): List[(Int, Int)] = {
        def fun(from: Int, tokens: List[Token], result: List[(Int, Int)]): List[(Int, Int)] = {
            // return None if tks can't match s.substring(i),
            def tryMatch(i: Int, tks: List[Token]): Option[Int] = {
                if (tks.isEmpty)
                    Some(i)
                else if (i >= s.length)
                    None
                else {
                    val until = tks.head.matchUntil(s, i)
                    if (until == i) None else tryMatch(until, tks.tail)
                }
            }

            if (from < s.length) {
                tryMatch(from, tokens) match {
                    case Some(end) => fun(end, tokens, (from, end) :: result)
                    case None => fun(from + 1, tokens, result)
                }
            } else
                result
        }

        if (tokenSets.isEmpty)
            List()      // epsilon
        else
            fun(0, tokenSets.map(_.head), List()).sortBy(_._1)
    }

    override def append(re: RegularExpression): RegularExpression = re match {
        case TokenSeq(tokens) => TokenSeq(tokenSets ++ tokens)
    }

    override def toString: String = tokenSets.mkString("{", "", "}")

    override def intersect(re: RegularExpression): Option[RegularExpression] = re match {
        case TokenSeq(tokens) if tokens.size == tokenSets.size =>
            val x = tokenSets.zip(tokens).map { case (tks1, tks2) => tks1.intersect(tks2) }
            if (x.exists(_.isEmpty)) None else Some(TokenSeq(x))
        case _ => None
    }

    lazy val size: Long = tokenSets.foldLeft(1)((z, s) => z * s.size)

    override def minimize: RegularExpression = TokenSeq(tokenSets.map(tks => Set(tks.max(Ranking.tokenRanking))))
}