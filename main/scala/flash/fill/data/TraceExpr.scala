package flash.fill.data

import flash.fill.Ranking

import scala.collection.mutable

sealed trait TraceExpr {
    type InputState = List[String]
    def intersect(te: TraceExpr): Option[TraceExpr]
    def size: Long
}

sealed trait Node
case class IntNode(i: Int) extends Node
case class PairNode(left: Node, right: Node) extends Node

case class Dag(source: Node, target: Node, edges: Set[PairNode], w: Map[PairNode, Set[AtomicExpr]]) extends TraceExpr {
    override def intersect(te: TraceExpr): Option[TraceExpr] = te match {
        case Dag(source2, target2, edges2, w2) =>
            // create edges in new Dag
            def f(l1: Set[PairNode], l2: Set[PairNode]): Set[PairNode] = for {
                PairNode(from1, to1) <- l1
                PairNode(from2, to2) <- l2
            } yield PairNode(PairNode(from1, from2), PairNode(to1, to2))

            val wp = (for {
                PairNode(a1, b1) <- w.keySet
                PairNode(a2, b2) <- w2.keySet
                fs = for {
                    f1 <- w(PairNode(a1, b1))
                    f2 <- w2(PairNode(a2, b2))
                    f <- f1.intersect(f2)
                } yield f
                if fs.nonEmpty
            } yield (PairNode(PairNode(a1, a2), PairNode(b1, b2)), fs)).toMap

            val src = PairNode(source, source2)
            val tgt = PairNode(target, target2)
            val egs = f(edges, edges2).filter(e => wp.isDefinedAt(e))
            Dag(src, tgt, egs, wp).minimize()
    }

    // try to minimize the dag by dropping nodes which not on any path from source to target
    def minimize(): Option[Dag] = {
        pathAlongEdgesToTarget(source, target, edges) match {
            case Some(paths) =>
                val es: Set[PairNode] = paths.flatten.map(t => PairNode(t._1, t._2))
                val ws = es.map(e => (e, w(e))).toMap
                Some(Dag(source, target, es, ws))
            case None => None
        }
    }

    private def pathAlongEdgesToTarget(from: Node, to: Node, es: Set[PairNode]): Option[Set[List[(Node, Node)]]] = {
        val successors: Map[Node, Set[Node]] = es.groupBy(edge => edge.left).map {
            case (node, set) => (node, set.map(p => p.right))
        }

        if (from == to)
            Some(Set(List()))
        else {
            successors.get(from) match {
                case Some(sn) =>
                    val x: Set[(Node, Option[Set[List[(Node, Node)]]])] = sn.map(z => (z, pathAlongEdgesToTarget(z, to, es)))
                    val y: Set[(Node, Set[List[(Node, Node)]])] = x.filter { case (_, option) => option.nonEmpty }.map(t => (t._1, t._2.get))
                    if (y.isEmpty) None else Some(y.flatMap {
                        case (node, result) => result.map(l => (from, node) :: l)
                    })
                case _ => None
            }
        }
    }

    lazy val size: Long = {
        val edgesSize = w.mapValues(list => list.map(_.size).sum)
        val memoize = new mutable.HashMap[Node, Long]()
        memoize(source) = 1

        def fun(n: Node): Long = {
            if (memoize.isDefinedAt(n))
                memoize(n)
            else {
                val from = for { PairNode(f, t) <- w.keySet if t == n } yield f
                from.map(f => fun(f) * edgesSize(PairNode(f, n))).sum
            }
        }

        fun(target)
    }

    lazy val getConcatenate: Option[Concatenate] = {
        val x: Option[Set[List[(Node, Node)]]] = pathAlongEdgesToTarget(source, target, edges)
        val y: Option[Set[List[AtomicExpr]]] = x.map(paths => paths.map(path => path.map{ case (from, to) => w(PairNode(from, to)).max(Ranking.atomicExprRanking) }))
        y.map(paths => Concatenate(paths.map(path => path.map(_.minimize)).max(Ranking.atomicExprListRanking)))
    }
}

case class Concatenate(fs: List[AtomicExpr]) {
    type InputState = List[String]

    def eval(inputState: InputState): Option[String] = {
        val results = fs.map(ae => ae.eval(inputState))
        if (results.exists(_.isEmpty)) None else Some(results.foldLeft("")((z, r) => z + r.get))
    }
}
