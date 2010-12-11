package edu.stanford.junction.sample.tower_defense
import scala.actors.Actor
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import scala.util.control.Breaks._
import scala.util.Random._

import edu.stanford.junction.JunctionException
import edu.stanford.junction.JunctionMaker
import edu.stanford.junction.Junction
import edu.stanford.junction.api.activity.JunctionActor
import edu.stanford.junction.api.activity.JunctionExtra
import edu.stanford.junction.api.activity.ActivityScript
import edu.stanford.junction.api.messaging.MessageHeader
import edu.stanford.junction.props2.IPropChangeListener
import edu.stanford.junction.provider.xmpp.XMPPSwitchboardConfig
import org.json.JSONObject
import java.util.ArrayList
import java.util.Random
import java.util.Date
import java.util.UUID
import java.net._

case class Wakeup()

import Data._
import Helpers._

object Main {

  def main(args: Array[String]) {
    if (args.length < 2) {
      "Usage: PROGRAM url num_users"
      System.exit(0)
    }
    val url = new URI(args(0))
    val numUsers = args(1).toInt
    println("Connecting to " + url + " with " + numUsers + " users.")
    val monitor = new SimMonitor(url, numUsers)
    monitor.start
  }
}

class SimMonitor(url: URI, numUsers: Int) extends Actor {
  val users: ListBuffer[SimUser] = ListBuffer()
  def act() {
    val rng = new Random()
    ActorPing.scheduleAtFixedRate(this, Wakeup(), 100, 1000)
    loop {
      try {
        react {
          case Wakeup() => {
            doWithProb((0.25, { () =>
              if (users.size() < numUsers) {
                val id = UUID.randomUUID.toString()
                val u = new SimUser(id, url)
                users += u
                u.start
              }
            }), (0.04, { () =>
              val randUser = shuffle(users).headOption
              for (u <- randUser) {
                u ! 'kill
                users.remove(u)
              }
            }))
            println("------------")
            println("Status: " + users.length + " users.")
            println("------------")
          }
          case msg => {
            println("Unexpected msg: " + msg)
          }
        }
      } catch {
        case e: Exception => {
          println("Error in msg loop, " + e)
        }
      }
    }
  }
}

class SimUser(id: String, url: URI) extends Actor {
  val me = new User("user" + id, 2, 5)
  val prop = new BoardProp("Board", 0, 0, 500, 800)
  prop.addChangeListener(new IPropChangeListener() {
    def getType = "sync"
    def onChange(data: Object) = {
      prop.getUser(me.id) match {
        case Some(u) => {}
        case None => {
          println("--------------------------")
          println("SYNCED. Adding...")
          println("--------------------------")
          prop.addUser(me)
        }
      }
    }
  })
  val jxActor = new JunctionActor("participant") {
    override def onActivityJoin() {
      println("--------------------------")
      println("SimUser " + id + " joined activity!")
      println("--------------------------")
    }
    override def onActivityCreate() {
      println("SimUser " + id + " created activity!")
    }
    override def onMessageReceived(header: MessageHeader, msg: JSONObject) {
      println("SimUser " + id + " got message!")
    }

    override def getInitialExtras(): java.util.List[JunctionExtra] = {
      val l = new ArrayList[JunctionExtra]()
      l.add(prop)
      l
    }
  }

  def initJunction(actor: JunctionActor, url: URI) {
    try {
      val sb = JunctionMaker.getDefaultSwitchboardConfig(url)
      val jxMaker = JunctionMaker.getInstance(sb)
      val jx = jxMaker.newJunction(url, actor)
      jx
    } catch {
      case e: JunctionException => {
        println("Failed to connect to junction activity!")
        e.printStackTrace(System.err)
        null
      }
      case e: Exception => {
        println("Failed to connect to junction activity!")
        e.printStackTrace(System.err)
        null
      }
    }
  }

  def act() {
    initJunction(jxActor, url)
    val rng = new Random()
    ActorPing.scheduleAtFixedRate(this, Wakeup(), rng.nextInt(1000), 1000)
    loop {
      try {
        react {
          case Wakeup() => {
            doWithProb((0.3, { () =>
              prop.moveUser(me, Board.Dir.random)
            }))
          }
          case 'kill => {
            jxActor.leave()
            exit
          }
          case msg => {
            println("Unexpected msg: " + msg)
          }
        }
      } catch {
        case e: Exception => {
          println("Error in msg loop, " + e)
        }
      }
    }
  }
}
