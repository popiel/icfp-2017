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
