package com.wolfskeep.icfp2017

import java.io.{DataInput, DataInputStream, InputStream, OutputStream}
import java.net.Socket
import scala.annotation.tailrec
import scala.util.Try
import spray.json._

object Punter {
  def main(args: Array[String]) {
    if (args.length < 1) {
      System.err.println("Usage: punter <strategy> [[<host>] <port>]")
      System.exit(1)
    }
    val sClass = Try { Class.forName(args(0)) } getOrElse { Class.forName("com.wolfskeep.icfp2017." + args(0)) }
    val strategy = sClass.newInstance.asInstanceOf[Strategy]
    if (args.length == 3) online(strategy, args(1), args(2).toInt)
    else if (args.length == 2) online(strategy, "punter.inf.ed.ac.uk", args(1).toInt)
    else offline(strategy)
  }

  def online(strategy: Strategy, host: String, port: Int) {
    val socket = new Socket(host, port)
    val punter = new Punter(strategy, new DataInputStream(socket.getInputStream), socket.getOutputStream)
    punter.handshake()
    while (punter.play()) ()
  }

  def offline(strategy: Strategy) {
    val punter = new Punter(strategy, new DataInputStream(System.in), System.out)
    punter.handshake()
    punter.play()
  }
}

class Punter(strategy: Strategy, in: DataInput, out: OutputStream) {
  import DefaultJsonProtocol._
  import PunterJsonProtocol._

  def send[T:JsonWriter](msg: T) {
    val bytes = msg.toJson.compactPrint.getBytes
    val lengthMark = (bytes.length.toString + ":").getBytes
    System.err.print("Sending: ")
    System.err.write(lengthMark)
    System.err.write(bytes)
    System.err.println()
    out.write(lengthMark)
    out.write(bytes)
    out.flush()
  }

  var readBuf = new Array[Byte](1024 * 1024)
  @tailrec final def recvLen(acc: Int = 0): Int = {
    val c = in.readByte()
    if (c == ':') acc
    else if (c >= '0' && c <= '9') recvLen(acc * 10 + c - '0')
    else throw new IllegalArgumentException("Invalid character in message length: " + c)
  }
  def recv: JsValue = {
    System.err.println("Reading:")
    val len = recvLen()
    System.err.println("  len: " + len)
    if (readBuf.length < len) readBuf = new Array[Byte](len)
    in.readFully(readBuf, 0, len)
    JsonParser(new ParserInput.ByteArrayBasedParserInput(readBuf) { override def length = len })
  }

  def handshake(name: String = strategy.getClass.getSimpleName) {
    send(Hail(name))
    if (recv.convertTo[WellMet].you != name) throw new IllegalStateException("Server cannot say my name properly.")
  }

  var myId = -1
  def play(): Boolean = {
    recv.convertTo[Either[Turn,Setup]] match {
      case Right(setup: Setup) =>
        myId = setup.punter
        System.err.println("I am punter " + myId)
        send(strategy.setup(setup))
        true
      case Left(Turn(Some(moves), _, state)) =>
        send(strategy.play(moves, state))
        true
      case Left(Turn(_, Some(stop), state)) =>
        val scores = stop.scores
        System.err.println("Punter: Score")
        scores.sortBy(_.score).reverse.foreach { score => System.err.println(score.punter + ": " + score.score) }
        if (myId >= 0) {
          val myScore = scores.find(_.punter == myId).get.score
          val myPoints = scores.length - scores.filter(_.score > myScore).length
          System.err.println("I got " + myPoints + " points!")
        }
        false
      case Left(turn) =>
        System.err.println("Illegal request from server: " + turn)
        false
    }
  }
}

trait Strategy {
  def setup(s: Setup): Ready
  def play(moves: Moves, state: Option[JsValue]): Move
}
