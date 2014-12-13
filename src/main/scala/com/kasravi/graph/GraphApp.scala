package com.kasravi.graph

import scala.reflect.runtime.universe._
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.GraphTraversal.DepthFirst
import scalax.collection.io.dot._

object GraphApp extends App {

  case class Memo(sorted: List[Any] = Nil, grey: Graph[Any, DiEdge] = Graph[Any, DiEdge](), black: Graph[Any, DiEdge] = Graph[Any, DiEdge]())

  def right[N: TypeTag, E[X] <: EdgeLikeIn[X]](m: Memo): Either[N, Memo] = Right(m)

  def dfs[N: TypeTag, E[X] <: EdgeLikeIn[X]](node: Graph[N, E]#NodeT, memo: Memo): Either[N, Memo] = {
    if (memo.grey.contains(OuterNode(node.value))) Left[N, Memo](node.value)
    else if (memo.black.contains(OuterNode(node.value))) right(memo)
    else searchAll[N, E](node.outNeighbors.toIterable, memo.copy(grey = memo.grey + node))
      .right.map(a => Memo(node.value :: a.sorted, memo.grey, a.black + node))
  }

  def searchAll[N: TypeTag, E[X] <: EdgeLikeIn[X]](nodes: Iterable[Graph[N, E]#NodeT], memo: Memo): Either[N, Memo] = {
    (right(memo) /: nodes)((accu, node) => accu.right.flatMap(m => dfs[N, E](node, m)))
  }

  def topologicallySort[N: TypeTag, E[X] <: EdgeLikeIn[X]](g: Graph[N,E]): Either[N, List[N]] =
    searchAll[N, E](g.nodes, Memo()).right.map(_.sorted.map(_.value)).asInstanceOf[Either[N, List[N]]]


  val typicalDay = Graph[String,DiEdge](
    "coffee" ~> "coding",
    "inspiration" ~> "coding",
    "shopping" ~> "coffee",
    "coding" ~> "sleeping",
    "supper" ~> "sleeping",
    "gaming" ~> "sleeping",
    "making music" ~> "sleeping",
    "inspiration" ~> "making music",
    "shopping" ~> "supper",
    "driving home" ~> "supper",
    "driving home" ~> "sleeping",
    "coding" ~> "driving home",
    "driving to work" ~> "coding",
    "driving to work" ~> "driving home",
    "driving home" ~> "gaming",
    "listening to music")
  println(topologicallySort(typicalDay))

  println(typicalDay.topologicalSort)

  val dotRoot = DotRootGraph (directed = true,
    id       = None,
    kvList   = Seq(DotAttr("attr_1", """"one""""),
      DotAttr("attr_2", "<two>")))
  val dotGraph = Graph[String, DiEdge](
    "Entry" ~> "A",
    "A" ~> "B",
    "B" ~> "C",
    "B" ~> "D",
    "D" ~> "F",
    "F" ~> "E",
    "E" ~> "F",
    "E" ~> "C",
    "C" ~> "A",
    "C" ~> "Exit")
  println(dotGraph.toDot(dotRoot, _ => None))

}
