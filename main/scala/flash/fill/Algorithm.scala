package flash.fill

import flash.fill.data.{AtomicExpr, CPos, Condition, Conjunct, ConstInt, ConstStr, Dag, IntNode, Match, NotMatch, PairNode, Pos, Position, Predicate, RegularExpression, StringExpr, SubStr, Switch, Token, TokenSeq, TraceExpr, True}

class Algorithm {
    type InputState = List[String]

    var optExpr: Option[StringExpr] = None

    def learn(input: Set[(InputState, String)]): Unit = {
        val generated = generateStringProgram(input)
        optExpr = generated.flatMap(sw => sw.getStringExpr)
    }

    def answer(input: InputState): Option[String] = {
        optExpr.flatMap(expr => expr.eval(input))
    }

    def generateStringProgram(input: Set[(InputState, String)]): Option[Switch] = {
        // start with singleton partitions that contain one input each
        val singleton = input.map { case (inputState, output) => (Set(inputState), generateStr(inputState, output)) }.toMap
        if (singleton.exists(_._2.isEmpty))
            None
        else {
            val partitioned = generatePartition(singleton.map(t => (t._1, t._2.get)))
            val allInput: Set[InputState] = input.map(_._1)

            partitioned.size match {
                case 0 => None
                case 1 =>
                    Some(data.Switch(List((True, partitioned.head._2))))
                case _ =>
                    val list = partitioned.toList.map {
                        case (sigma, expr) =>
                            val opt = generateBoolClassifier(sigma, allInput -- sigma)
                            (opt, expr)
                    }
                    if (list.exists(_._1.isEmpty)) None else Some(data.Switch(list.map(t => (t._1.get, t._2))))
            }
        }
    }

    def generatePartition[T](part: Map[Set[InputState], TraceExpr]): Map[Set[InputState], TraceExpr] = {
        val ordered = part.toList
        val table = (for {
            i <- 0 until ordered.size - 1
            j <- (i + 1) until ordered.size
            (_, dag1) = ordered(i)
            (_, dag2) = ordered(j)
            result <- dag1.intersect(dag2)
        } yield ((i, j), result)).toMap

        def comp(i: Int, j: Int): Boolean =
            table.isDefinedAt(i, j)

        def cs1(i: Int, j: Int): Int = {
            val ks = for { k <- ordered.indices if k != i && k != j } yield k
            ks.count(k => comp(i, k) == comp(j, k) && table(i, j).intersect(ordered(k)._2).nonEmpty)
        }

        def cs2(i: Int, j: Int): Double = {
            val numerator = table(i, j).size
            val denominator = ordered(i)._2.size max ordered(j)._2.size
            numerator.toDouble / denominator
        }

        case class CS(cs1: Int, cs2: Double) extends Ordered[CS] {
            override def compare(that: CS): Int = if (cs1 > that.cs1 || ((cs1 == that.cs1) && cs2 > that.cs2)) 1 else -1
        }

        if (table.isEmpty)
            part
        else {
            val (x, y) = table.keySet.map { case (i, j) => ((i, j), CS(cs1(i, j), cs2(i, j))) }.maxBy(_._2)._1
            val intersect = table(x, y)
            val rest = part.filter(p => ordered(x) !=p && ordered(y) != p)
            generatePartition(rest.updated(ordered(x)._1 ++ ordered(y)._1, intersect))
        }
    }

    def generateBoolClassifier(sigma1: Set[InputState], sigma2: Set[InputState]): Option[Condition] = {
        val predicates: Set[Predicate] = {
            def fun(vi: Int, s: String): Set[Predicate] = {
                val tokens = Token.getAll
                val matches = for {
                    i <- s.indices
                    (r, _) <- findAllRegularExpressionMatchesTo(s.substring(i), tokens)
                } yield Match(vi, r, r.findAllMatches(s).size)

                // construct NotMatch Predicates from Match
                matches.toSet ++ matches.map { case Match(vi, r, k) => NotMatch(vi, r, k) }
            }

            (sigma1 ++ sigma2).flatMap(in => in.zipWithIndex.flatMap { case (str, i) => fun(i, str) })
        }

        def csp(p: Predicate, sig1: Set[InputState], sig2: Set[InputState]): Int =  {
            def negate[T](f: T => Boolean): T => Boolean =
                t => !f(t)

            sig1.count(p.eval) * sig2.count(negate(p.eval))
        }

        @annotation.tailrec
        def outerLoop(s1: Set[InputState], s2: Set[InputState], result: List[Conjunct]): Option[Condition] = {
            if (s1.isEmpty)
                Some(data.Condition(result))
            else {
                val invoke = innerLoop(s1, s2, List())
                invoke match {
                    case Some((eliminated, c)) if eliminated.nonEmpty => outerLoop(s1 -- eliminated, s2, c :: result)
                    case _ => None
                }
            }
        }

        @annotation.tailrec
        def innerLoop(s1: Set[InputState], s2: Set[InputState], result: List[Predicate]): Option[(Set[InputState], Conjunct)] = {
            if (s2.isEmpty)
                Some((s1, data.Conjunct(result)))
            else {
                val predicate = predicates.maxBy(candidate => csp(candidate, s1, s2))   //TODO: ranking
                val s1match = s1.filter(predicate.eval)
                val s2match = s2.filter(predicate.eval)
                if (s2match.size == s2.size) None else innerLoop(s1match, s2match, predicate :: result)
            }
        }

        outerLoop(sigma1, sigma2, List())
    }

    def generateStr(sigma: InputState, s: String): Option[Dag] = {
        if (s.nonEmpty) {
            val source = IntNode(0)
            val target = IntNode(s.length)
            val edges = for {
                i <- 0 until s.length
                j <- (i + 1) to s.length
            } yield (i, j)

            val w = edges.map(edge => (PairNode(IntNode(edge._1), IntNode(edge._2)),
                generateSubstring(sigma, s.substring(edge._1, edge._2)) + ConstStr(s.substring(edge._1, edge._2)))).toMap
            Dag(source, target, edges.map(edge => (PairNode(IntNode(edge._1), IntNode(edge._2)))).toSet, w).minimize()
        } else {
            Some(Dag(IntNode(0), IntNode(1), Set(PairNode(IntNode(0), IntNode(1))), Map(PairNode(IntNode(0), IntNode(1)) -> Set(ConstStr("")))))
        }
    }

    def generateSubstring(sigma: InputState, s: String): Set[AtomicExpr] = {
        (for {
            vi <- sigma.indices
            start <- 0 to sigma(vi).length - s.length
            if sigma(vi).substring(start, start + s.length) == s
        } yield SubStr(vi, generatePosition(sigma(vi), start), generatePosition(sigma(vi), start + s.length))).toSet
    }

    def generatePosition(s: String, k: Int): List[Position]  = {
        val iParts: Map[Token, Set[Token]] = distinguish(s)
        val reps: Set[Token] = iParts.keySet

        def generateRegex(r: RegularExpression): RegularExpression = r match {
            case TokenSeq(tokenSets) => TokenSeq(tokenSets.map(set => iParts(set.head)))
        }

        //find all (r2, k2) such that r2 matches s[k, k2]
        def getRegularExpressionsMatchSuffix(): List[(RegularExpression, Int)] = {
            findAllRegularExpressionMatchesTo(s.substring(k), reps).map(t => (t._1, t._2 + k)).
                filter { case (re, i) => re.findAllMatches(s).contains((k, i + 1)) }
        }

        //find all (r1, k1) such that r1 matches s[k1, k - 1]
        def getRegularExpressionsMatchPrefix(): List[(RegularExpression, Int)] = {
            def reverseRegularExpression(r: RegularExpression): RegularExpression = r match {
                case TokenSeq(tokenSets) => TokenSeq(tokenSets.reverse)
            }

            findAllRegularExpressionMatchesTo(s.substring(0, k).reverse, reps).map(t => (reverseRegularExpression(t._1), k - 1 - t._2)).
                filter { case (re, i) => re.findAllMatches(s).contains((i, k)) }
        }

        val prefixes = (TokenSeq(List()), k) :: getRegularExpressionsMatchPrefix()
        val suffixes = (TokenSeq(List()), k - 1) :: getRegularExpressionsMatchSuffix()
        val positions = CPos(k) :: prefixes.flatMap { case (r1, k1) =>
            suffixes.flatMap { case (r2, k2) =>
                val matches = r1.append(r2).findAllMatches(s)
                val c = matches.indexOf((k1, k2 + 1)) + 1
                // c equals 0 indicates that s[k1, k2] can't be match by r1+r2, often caused by overlap
                if (c > 0) {
                    val cp = matches.size
                    List(Pos(generateRegex(r1), generateRegex(r2), ConstInt(c)), data.Pos(generateRegex(r1), generateRegex(r2), ConstInt(-(cp - c + 1))))
                } else
                    List()
            }
        }

        if (k < s.length)
            CPos(-(s.length - k)) :: positions      // handle case k == s.length which leads to an invalid CPos(0)
        else
            positions
    }

    // partition all tokens into Indistinguishable Sets
    def distinguish(s: String): Map[Token, Set[Token]] = {
        Token.getAll.groupBy(t => TokenSeq(List(Set(t))).findAllMatches(s)).map{ case (_, set) => (set.head, set) }
    }

    //find all (r', k') such that r' matches string[0, k']
    def findAllRegularExpressionMatchesTo(string: String, usableTokens: Set[Token]): List[(RegularExpression, Int)] = {
        def h(i: Int, tokens: List[Set[Token]]): List[(RegularExpression, Int)] = {
            if (i >= string.length)
                List()
            else {
                usableTokens.toList.flatMap(token => {
                    val next = token.matchUntil(string, i)
                    if (next == i)
                        List()
                    else {
                        val trace = tokens :+ Set(token)
                        (TokenSeq(trace), next - 1) :: h(next, trace)
                    }
                })
            }
        }

        h(0, List())
    }
}

object Algorithm {
    def apply(): Algorithm = new Algorithm()
}
