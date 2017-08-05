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

case class GameState(id: Int, punters: Int, remaining: Int, sites: Vector[Long], rMap: Vector[Set[Int]], mines: Vector[Int], rivers: Vector[(Int,Int,Int)]) {
  def + (claim: Claim) = {
    val from = sites.indexOf(claim.source)
    val to   = sites.indexOf(claim.target)
    val river = (rMap(from) intersect rMap(to)).find(r => rivers(r)._3 < 0)
    if (river != None) {
      val r = rivers(river.get)
      copy(rivers = rivers.updated(river.get, (r._1, r._2, claim.punter)))
    } else this
  }
}
object GameState {
  def fromSetup(s: Setup): GameState = {
    val sites = s.map.sites.map(_.id)
    val mines = s.map.mines.map(sites.indexOf(_))
    val rivers = s.map.rivers.map(r => (sites.indexOf(r.source), sites.indexOf(r.target), -1))
    val rMap = rivers.zipWithIndex.foldLeft(scala.collection.immutable.Map.empty[Int,Set[Int]].withDefaultValue(Set.empty[Int])){ (m, r) =>
      m + (r._1._1 -> (m(r._1._1) + r._2)) + (r._1._2 -> (m(r._1._2) + r._2))
    }

    GameState(s.punter, s.punters, (rivers.length + s.punters - s.punter) / s.punters, sites, (0 until sites.length).map(rMap).toVector, mines, rivers)
  }
}

class Eager extends Strategy {
  implicit val GameStateFormat = jsonFormat7(GameState.apply)

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
    val river = state.rivers.find(_._3 < 0)
    if (river == None) 
      Move(None, Some(Pass(state.id)), s.map(_ => state.toJson))
    else {
      val r = river.get
      Move(Some(Claim(state.id, state.sites(r._1), state.sites(r._2))), None, s.map(_ => state.toJson))
    }
  }
}
