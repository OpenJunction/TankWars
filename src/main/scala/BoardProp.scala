package edu.stanford.junction.sample.tower_defense
import org.json._
import java.util.Random
import java.util.UUID
import edu.stanford.junction.props2._
import scala.collection.mutable.ListBuffer
import scala.collection.immutable._
import scala.math._
import Board.Dir._

class BoardProp(propName: String, winX: Int, winY: Int, winW: Int, winH: Int) extends Prop(propName, new BoardState()) {

  def withBoardState[T](op: (BoardState => T)): T = {
    return withState(new IWithStateAction[T]() {
	def run(state: IPropState): T = {
          op(state.asInstanceOf[BoardState])
	}
      });
  }

  def getUser(userId: String): Option[User] = {
    withBoardState(s => s.getUser(userId))
  }

  def getAllUsers(): List[User] = {
    withBoardState(s => s.getAllUsers())
  }

  def userAt(x: Int, y: Int): Option[User] = {
    withBoardState(s => s.userAt(x,y))
  }

  def objectAt(x: Int, y: Int): Option[Any] = {
    withBoardState(s => s.objectAt(x,y))
  }

  def eachUser(iter: (User => Unit)) = {
    withBoardState(s => s.eachUser(iter))
  }

  def eachObstacle(iter: (Obstacle => Unit)) = {
    withBoardState(s => s.eachObstacle(iter))
  }

  def getFireTarget(userId: String) = {
    withBoardState(s => s.getFireTarget(userId))
  }

  def addUser(u: User) {
    addOperation(newAddUserOp(u.name, u.id, (u.boardX, u.boardY), u.color))
  }

  def moveUser(u: User, from: (Int, Int), to: (Int, Int)) {
    if (Board.hexCenterInRect(to._1, to._2, winX, winY, winW, winH)) {
      val dir = Board.hexPointsToDir(from, to)
      objectAt(to._1, to._2) match {
        case Some(_) => println("Oops, blocked!")
        case None => addOperation(newMoveUserOp(u.id, from, to, dir))
      }
    } else {
      println("Oops, can't move offscreen!")
    }
  }

  def moveUser(u: User, d: Dir) {
    getUser(u.id) match {
      case Some(u: User) => {
        val s = Board.Dir.step(d)
        moveUser(u, (u.boardX, u.boardY), (u.boardX + s._1, u.boardY + s._2))
      }
      case None => println("Oops, no user found.");
    }
  }

  def rotateUser(u: User, d: Dir) {
    addOperation(newMoveUserOp(u.id, (u.boardX, u.boardY), (u.boardX, u.boardY), d))
  }

  def fire(user: User) {
    getUser(user.id) match {
      case Some(user: User) => {
        getFireTarget(user.id) match {
          case Some(u: User) => {
            addOperation(newFireHitOp(user.id, u.id))
          }
          case Some(u: Obstacle) => {
            addOperation(newFireMissOp(user.id))
          }
          case None => {
            addOperation(newFireMissOp(user.id))
          }
        }
      }
      case None => println("Oops, no user found.");
    }
  }

  def kill(user: User) {
    getUser(user.id) match {
      case Some(user: User) => {
        addOperation(newDieOp(user.id))
      }
      case None => println("Oops, no user found.");
    }
  }

  override def newFresh() = new BoardProp(getPropName, winX, winY, winW, winH)

  override def reifyState(obj: JSONObject): IPropState = {
    try {
      val t = obj.optString("type")
      if (t == "BoardState") {
        val a = obj.getJSONArray("users")
        val users = new ListBuffer[User]()
        for (i <- 0 until a.length()) {
          User.fromJSON(a.getJSONObject(i)) match {
            case u: User => users += u
            case _ =>
          }
        }
        val o = obj.getJSONArray("obstacles")
        val obstacles = new ListBuffer[Obstacle]()
        for (i <- 0 until o.length()) {
          Obstacle.fromJSON(o.getJSONObject(i)) match {
            case obs: Obstacle => obstacles += obs
            case _ =>
          }
        }
        new BoardState(users.toList, obstacles.toList)
      } else {
        new BoardState()
      }
    } catch {
      case e: JSONException => null
    }
  }

  def newMoveUserOp(userId: String, from: (Int, Int), to: (Int, Int), dir: Dir): JSONObject = {
    val obj = new JSONObject()
    try {
      obj.put("type", "moveUserOp")
      obj.put("userId", userId)
      obj.put("fromX", from._1)
      obj.put("fromY", from._2)
      obj.put("toX", to._1)
      obj.put("toY", to._2)
      obj.put("dir", dir.toString)
    } catch {
      case e: JSONException => {}
    }
    obj
  }

  def newAddUserOp(name: String, userId: String, to: (Int, Int), color: String): JSONObject = {
    val obj = new JSONObject()
    try {
      obj.put("type", "addUserOp")
      obj.put("name", name)
      obj.put("userId", userId)
      obj.put("toX", to._1)
      obj.put("toY", to._2)
      obj.put("color", color)
    } catch {
      case e: JSONException => {}
    }
    obj
  }

  def newFireHitOp(fromId: String, toId: String): JSONObject = {
    val obj = new JSONObject()
    try {
      obj.put("type", "fireHitOp")
      obj.put("fromId", fromId)
      obj.put("toId", toId)
    } catch {
      case e: JSONException => {}
    }
    obj
  }

  def newFireMissOp(fromId: String): JSONObject = {
    val obj = new JSONObject()
    try {
      obj.put("type", "fireMissOp")
      obj.put("fromId", fromId)
    } catch {
      case e: JSONException => {}
    }
    obj
  }

  def newDieOp(userId: String): JSONObject = {
    val obj = new JSONObject()
    try {
      obj.put("type", "dieOp")
      obj.put("userId", userId)
    } catch {
      case e: JSONException => {}
    }
    obj
  }

}

class BoardState(u: Iterable[User], obs: Iterable[Obstacle]) extends IPropState {

  private val users = new ListBuffer[User]
  users ++= u

  private val obstacles = new ListBuffer[Obstacle]
  obstacles ++= obs

  def this() = this(List[User](), (1 until 15).map(i => Obstacle.newRandom))

  def dieUser(userId: String) {
    getUser(userId) match {
      case Some(u: User) => {
        users -= u
      }
      case None => {}
    }
  }

  def moveUser(userId: String, to: (Int, Int), dir: Dir) {
    getUser(userId) match {
      case Some(u: User) => {
        u.boardX = to._1
        u.boardY = to._2
        u.boardDir = dir
      }
      case None => {}
    }
  }

  def reduceUserHealth(userId: String, amount: Int) {
    getUser(userId) match {
      case Some(u: User) => {
        u.health -= amount;
      }
      case None => {}
    }
  }

  def getFireTarget(userId: String): Option[Any] = {
    getUser(userId) match {
      case Some(u: User) => {
        val s = Board.Dir.step(u.boardDir)
        val inRange = (for (i <- 1 until 20) yield {
            val (x, y) = (u.boardX + (i * s._1), u.boardY + (i * s._2))
            objectAt(x, y)
          }).flatten
        inRange.headOption
      }
      case None => None
    }
  }

  def addUser(name: String, userId: String, at: (Int, Int), color: String) {
    users += new User(name, userId, at._1, at._2, N, 100, color)
  }

  def hash(): String = users.map(_.hash()).mkString("|")

  def getUser(userId: String): Option[User] = users.find(_.id == userId)

  def getAllUsers(): List[User] = users.toList

  def userAt(x: Int, y: Int): Option[User] = users.find(u => u.boardX == x && u.boardY == y)

  def objectAt(x: Int, y: Int): Option[Any] = {
    users.find(u => u.boardX == x && u.boardY == y) match {
      case Some(u) => Some(u)
      case None => obstacles.find(o => o.boardX == x && o.boardY == y)
    }
  }

  def eachUser(iter: (User => Unit)) = users.foreach(iter)

  def eachObstacle(iter: (Obstacle => Unit)) = obstacles.foreach(iter)

  def copy(): IPropState = new BoardState(users.toList.map(_.copy()), obstacles.toList.map(_.copy()))

  def toJSON(): JSONObject = {
    val obj = new JSONObject()
    try {
      obj.put("type", "BoardState")

      val a = new JSONArray()
      for (u <- users) {
        a.put(u.toJSON())
      }
      obj.put("users", a)

      val o = new JSONArray()
      for (obs <- obstacles) {
        o.put(obs.toJSON())
      }
      obj.put("obstacles", o)
    } catch {
      case e: JSONException => {}
    }
    obj
  }

  def applyOperation(op: JSONObject): IPropState = {
    val tpe = op.optString("type")
    if (tpe.equals("moveUserOp")) {
      val userId = op.optString("userId")
      val fromX = op.optInt("fromX")
      val fromY = op.optInt("fromY")
      val toX = op.optInt("toX")
      val toY = op.optInt("toY")
      val dir = Board.Dir.withName(op.optString("dir"))
      moveUser(userId, (toX, toY), dir)
    } else if (tpe.equals("addUserOp")) {
      val name = op.optString("name")
      val userId = op.optString("userId")
      val toX = op.optInt("toX")
      val toY = op.optInt("toY")
      val color = op.optString("color")
      addUser(name, userId, (toX, toY), color)
    } else if (tpe.equals("fireHitOp")) {
      val fromId = op.optString("fromId")
      val toId = op.optString("toId")
      reduceUserHealth(toId, 20)
    } else if (tpe.equals("fireMissOp")) {
      val fromId = op.optString("fromId")
      // no-op
    } else if (tpe.equals("dieOp")) {
      val userId = op.optString("userId")
      dieUser(userId)
    } else {
      throw new RuntimeException("Unrecognized operation: " + op)
    }
    return this
  }

  override def toString() = "BoardState: " + users.toString()

}
