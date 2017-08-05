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

case class GameState(id: Int, sites: Vector[Long], mines: Vector[Int], rivers: Vector[(Int,Int,Int)]) {
  def + (claim: Claim) = {
    val from = sites.indexOf(claim.source)
    val to   = sites.indexOf(claim.target)
    val river = rivers.indexWhere(x => x._3 < 0 && ((x._1 == from && x._2 == to) || (x._2 == from && x._1 == to)))
    if (river >= 0) {
      copy(rivers = rivers.updated(river, (from, to, claim.punter)))
    } else this
  }
}

class Eager extends Strategy {
  implicit val GameStateFormat = jsonFormat4(GameState.apply)

  var state: GameState = _

  def setup(s: Setup): Ready = {
    val sites = s.map.sites.map(_.id)
    val mines = s.map.mines.map(sites.indexOf(_))
    val rivers = s.map.rivers.map(r => (sites.indexOf(r.source), sites.indexOf(r.target), -1))
    state = GameState(s.punter, sites, mines, rivers)
    Ready(s.punter, Some(state.toJson), None)
  }
  def play(moves: Moves, s: Option[JsValue]): Move = {
    if (s != None) state = s.get.convertTo[GameState]
    moves.moves.map(m => m.claim.foreach(c => state += c))
    val river = state.rivers.find(_._3 < 0)
    if (river == None) 
      Move(None, Some(Pass(state.id)), s.map(_ => state.toJson))
    else {
      val r = river.get
      Move(Some(Claim(state.id, state.sites(r._1), state.sites(r._2))), None, s.map(_ => state.toJson))
    }
  }
}
