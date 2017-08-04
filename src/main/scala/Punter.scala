package com.wolfskeep.icfp2017

import java.io.{DataInput, DataInputStream, InputStream, OutputStream}
import java.net.Socket
import scala.annotation.tailrec
import spray.json._

object Punter {
  def main(args: Array[String]) {
    if (args.length == 2) online(args(0), args(1).toInt)
    else if (args.length == 1) online("punter.inf.ed.ac.uk", args(0).toInt)
    else offline()
  }

  def online(host: String, port: Int) {
    val socket = new Socket(host, port)
    val punter = new Punter(new DataInputStream(socket.getInputStream), socket.getOutputStream)
    punter.handshake("InvisibleImp")
    while (punter.play()) ()
  }

  def offline() {
    val punter = new Punter(new DataInputStream(System.in), System.out)
    punter.play()
  }
}

class Punter(in: DataInput, out: OutputStream) {
  import DefaultJsonProtocol._
  import PunterJsonProtocol._

  def send[T:JsonWriter](msg: T) {
    val bytes = msg.toJson.compactPrint.getBytes
    val lengthMark = (bytes.length.toString + ":").getBytes
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
    val len = recvLen()
    if (readBuf.length < len) readBuf = new Array[Byte](len)
    in.readFully(readBuf, 0, len)
    JsonParser(new ParserInput.ByteArrayBasedParserInput(readBuf) { override def length = len })
  }

  def handshake(name: String) {
    send(Hail(name))
    if (recv.convertTo[WellMet].you != name) throw new IllegalStateException("Server cannot say my name properly.")
  }

  def play(): Boolean = {
    false
  }
}
