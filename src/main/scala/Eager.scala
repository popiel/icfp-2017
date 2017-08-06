package com.wolfskeep.icfp2017

import spray.json._
import spray.json.DefaultJsonProtocol._

class Sleepy extends Strategy {
  def setup(s: Setup): Ready = {
    Ready(s.punter, Some(JsNumber(s.punter)), None)
  }
  def play(moves: Moves, state: Option[JsValue]): Move = {
    val myId = state.map(_.convertTo[Int]).getOrElse(0)
    Move(None, Some(Pass(myId)), state)
  }
}

case class GameState(id: Int, punters: Int, remaining: Int, sites: Vector[Long], rMap: Vector[Set[Int]], mines: Vector[(Int,Vector[Int])], rivers: Vector[(Int,Int,Int)]) {
  def + (claim: Claim) = {
    val from = sites.indexOf(claim.source)
    val to   = sites.indexOf(claim.target)
    val river = (rMap(from) intersect rMap(to)).find(r => rivers(r)._3 < 0)
    if (river != None) {
      val r = rivers(river.get)
      copy(rivers = rivers.updated(river.get, (r._1, r._2, claim.punter)))
    } else this
  }

  def reachable(from: Int)(filter: ((Int,Int,Int)) => Boolean): Set[Int] = {
    val open = scala.collection.mutable.Queue(from)
    var closed = Set.empty[Int]
    while (open.nonEmpty) {
      val f = open.dequeue
      for {
        rIndex <- rMap(f)
        r = rivers(rIndex)
        t = r._1 + r._2 - f
        if !closed(t)
        if filter(r)
      } {
        closed += t
        open.enqueue(t)
      }
    }
    closed
  }

  def score(punter: Int) = 
    (for { m <- mines } yield {
      reachable(m._1)(_._3 == punter).map(m._2).map(x => x * x).sum
    }).sum

  lazy val scores = (0 until punters).map(score)
  lazy val myScore = score(id)
}
object GameState {
  def fromSetup(s: Setup): GameState = {
    val sites = s.map.sites.map(_.id)
    val rivers = s.map.rivers.map(r => (sites.indexOf(r.source), sites.indexOf(r.target), -1))
    val rm = rivers.zipWithIndex.foldLeft(scala.collection.immutable.Map.empty[Int,Set[Int]].withDefaultValue(Set.empty[Int])){ (m, r) =>
      m + (r._1._1 -> (m(r._1._1) + r._2)) + (r._1._2 -> (m(r._1._2) + r._2))
    }
    val rMap = (0 until sites.length).map(rm).toVector
    val mines = s.map.mines.map { mId =>
      val m = sites.indexOf(mId)
      (m, minDist(m, rMap, rivers))
    }
    GameState(s.punter, s.punters, (rivers.length + s.punters - s.punter) / s.punters, sites, rMap, mines, rivers)
  }

  def minDist(from: Int, rMap: Vector[Set[Int]], rivers: Vector[(Int,Int,Int)]): Vector[Int] = {
    val arr = Array.fill[Int](rMap.length)(rMap.length)
    arr(from) = 0
    val open = scala.collection.mutable.Queue(from)
    while (open.nonEmpty) {
      val f = open.dequeue
      for {
        rIndex <- rMap(f)
        r = rivers(rIndex)
        t = r._1 + r._2 - f
        if arr(t) > arr(f) + 1
      } {
        arr(t) = arr(f) + 1
        open.enqueue(t)
      }
    }
    arr.toVector
  }
}

class Eager extends Strategy {
  implicit val GameStateFormat = jsonFormat(GameState.apply, "id", "punters", "remaining", "sites", "rMap", "mines", "rivers")

  var state: GameState = _

  def setup(s: Setup): Ready = {
    state = GameState.fromSetup(s)
    Ready(s.punter, Some(state.toJson), None)
  }
  def play(moves: Moves, s: Option[JsValue]): Move = {
    if (s != None) state = s.get.convertTo[GameState]
    moves.moves.map(m => m.claim.foreach(c => state += c))
    state = state.copy(remaining = state.remaining - moves.moves.length / state.punters)

    // Find move with greatest guaranteed point increase
    // Find move which reduces risk the most
    // Find move that blocks opponents the most
    val picks = state.rivers.filter(_._3 < 0)
    val claims = picks.map(r => Claim(state.id, state.sites(r._1), state.sites(r._2)))
    val states = claims.map(c => c -> (state + c))
    val claim = states.sortBy(_._2.myScore).reverse.headOption
    claim match {
      case None            => Move(None, Some(Pass(state.id)), s.map(_ => state.toJson))
      case Some((c, newS)) => Move(Some(c), None, s.map(_ => newS.toJson))
    }
  }
}
