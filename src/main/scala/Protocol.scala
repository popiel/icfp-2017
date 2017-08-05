package com.wolfskeep.icfp2017

import spray.json._

case class Hail(me: String)
case class WellMet(you: String)
case class Setup(punter: Int, punters: Int, map: Graph, settings: Option[Settings])
case class Settings(futures: Boolean)
case class Ready(ready: Int, state: Option[JsValue], futures: Option[Vector[River]])
case class Graph(sites: Vector[Site], rivers: Vector[River], mines: Vector[Long])
case class Site(id: Long)
case class River(source: Long, target: Long) { def canonical = if (source <= target) this else River(target, source) }
case class Turn(move: Option[Moves], stop: Option[Stop], state: Option[JsValue])
case class Moves(moves: Vector[Move])
case class Move(claim: Option[Claim], pass: Option[Pass], state: Option[JsValue])
case class Claim(punter: Int, source: Long, target: Long)
case class Pass(punter: Int)
case class Stop(moves: Vector[Move], scores: Vector[Score])
case class Score(punter: Int, score: Long)

object PunterJsonProtocol {
  import DefaultJsonProtocol._

  implicit val ScoreFormat = jsonFormat2(Score.apply)
  implicit val PassFormat = jsonFormat1(Pass.apply)
  implicit val ClaimFormat = jsonFormat3(Claim.apply)
  implicit val MoveFormat = jsonFormat3(Move.apply)
  implicit val StopFormat = jsonFormat2(Stop.apply)
  implicit val MovesFormat = jsonFormat1(Moves.apply)
  implicit val TurnFormat = jsonFormat3(Turn.apply)
  implicit val RiverFormat = jsonFormat2(River.apply)
  implicit val SiteFormat = jsonFormat1(Site.apply)
  implicit val GraphFormat = jsonFormat3(Graph.apply)
  implicit val ReadyFormat = jsonFormat3(Ready.apply)
  implicit val SettingsFormat = jsonFormat1(Settings.apply)
  implicit val SetupFormat = jsonFormat4(Setup.apply)
  implicit val WellMetFormat = jsonFormat1(WellMet.apply)
  implicit val HailFormat = jsonFormat1(Hail.apply)
}
