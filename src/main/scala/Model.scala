package edu.stanford.junction.sample.tower_defense
import org.json._
import java.util.Random
import java.util.UUID
import java.awt.geom.Rectangle2D
import scala.collection.mutable.ListBuffer
import scala.collection.immutable._
import scala.math._

object Obstacle{

  def fromJSON(obj:JSONObject):Obstacle = {
    try{
      new Obstacle(
	obj.optInt("x"),
	obj.optInt("y"))
    }
    catch{
      case e:JSONException => null
    }
  }

  def newRandom = {
    val rng = new Random()
    new Obstacle(rng.nextInt(15), rng.nextInt(30))
  }


}


abstract class BoardObject{
  def boardX:Int
  def boardY:Int
  def rotation:Double
}

class Obstacle(x:Int, y:Int) extends BoardObject{

  var boardX = x
  var boardY = y

  def rotation = 0

  def toJSON():JSONObject = {
    val obj = new JSONObject()
    try{
      obj.put("x", boardX)
      obj.put("y", boardY)
    }
    catch{
      case e:JSONException => {}
    }
    obj
  }

  def copy():Obstacle = {
    new Obstacle(boardX, boardY)
  }

}

object User{

  val tankColors:List[String] = List("green","blue","purple","yellow","black")

  def fromJSON(obj:JSONObject):User = {
    try{
      val t = obj.optString("type")
      if(t == "User"){
	new User(
	  obj.optString("name"), 
	  obj.optString("userId"),
	  obj.optInt("x"),
	  obj.optInt("y"),
	  Board.Dir.withName(obj.optString("dir")),
	  obj.optInt("health"),
	  obj.optString("color")
	)
      }
      else{
	null
      }
    }
    catch{
      case e:JSONException => null
    }
  }


}

class User(val name:String, val id:String, x:Int, y:Int, dir:Board.Dir.Dir, hlth:Int, cl:String) extends BoardObject{

  var boardX = x
  var boardY = y
  var boardDir = dir
  var health = hlth
  val color = cl

  val rng = new Random()

  def this(name:String, x:Int, y:Int) = this(
    name, name + "_" + UUID.randomUUID().toString(), x, y, Board.Dir.N, 100, User.tankColors((new Random()).nextInt(User.tankColors.length)))

  def this(name:String, userId:String, x:Int, y:Int) = this(
    name, userId, x, y, Board.Dir.N, 100, User.tankColors((new Random()).nextInt(User.tankColors.length)))

  def this(name:String) = this(
    name, 0, 0)

  override def equals(obj:Any):Boolean = {
    obj match{
      case u:User => (u.id == this.id)
      case _ => false
    }
  }

  def rotation = Board.dirToTheta(dir)

  def hash():String = {
    List(name,id,boardX,boardY,boardDir,health).mkString("_")
  }

  def copy():User = {
    new User(name, id, boardX, boardY, boardDir, health, color)
  }

  def toJSON():JSONObject = {
    val obj = new JSONObject()
    try{
      obj.put("type", "User")
      obj.put("name", name)
      obj.put("userId", id)
      obj.put("x", boardX)
      obj.put("y", boardY)
      obj.put("dir", boardDir.toString)
      obj.put("health", health)
      obj.put("color", color)
    }
    catch{
      case e:JSONException => {}
    }
    obj
  }

}

object Board{
  val CELL_WIDTH = 60
  val CELL_HEIGHT = 50
  val CELL_ARM_WIDTH = 16
  val CELL_BODY_WIDTH = CELL_WIDTH - (2 * CELL_ARM_WIDTH)

  object Dir extends Enumeration("NE", "NW", "N", "S", "SE", "SW") {
    type Dir = Value
    val NE, NW, N, S, SE, SW = Value

    def random:Dir = {
      val rng = new Random()
      Board.Dir(rng.nextInt(maxId))
    }

    def step(d:Dir):(Int, Int) = {
      d match {
	case N =>  (0, -1)
	case S =>  (0, 1)
	case NE => (1, 0)
	case NW => (-1, -1)
	case SW => (-1, 0)
	case SE => (1, 1)
      }
    }
  }

  import Dir._

	def pixelPointsToDir(from:(Int, Int), to:(Int, Int)):Dir = {
		val theta:Double = atan2(to._1 - from._1, from._2 - to._2)
		if (abs(theta) > 5.0/6.0*Pi)
			S
		else if (theta > 1.0/2.0*Pi)
			SE
		else if (theta > 1.0/6.0*Pi)
			NE
		else if (abs(theta) <= 1.0/6.0*Pi)
			N
		else if (theta < -1.0/2.0*Pi)
			SW
		else
			NW
	}

  def hexPointsToDir(from:(Int, Int), to:(Int, Int)):Dir = {
    if (to._2 < from._2) {
      if (to._1 < from._1) NW
      else if (to._1 == from._1) N
      else NE
    } else if (to._2 == from._2) {
      if (to._1 < from._1) SW
      else if (to._1 == from._1) N
      else NE
    } else {
      if (to._1 < from._1) SW
      else if (to._1 == from._1) S
      else SE
    }
  }

  def dirToTheta(d:Dir):Double = {
    d match {
      case NE => -0.7*Pi
      case NW => 0.7*Pi
      case N => Pi
      case S => 0.0
      case SE => -0.3*Pi
      case SW => 0.3*Pi
    }
  }

  def dirToDegrees(d:Dir):Double = {
    (dirToTheta(d) * 180.0) / Pi
  }

  def pixelToHex(x:Int, y:Int):(Int,Int) = {
    var mx = x - CELL_ARM_WIDTH
    var my = y
    var h = CELL_HEIGHT
    var cw = 16
    var tw = 28

    var tx = mx/(cw+tw)
    var rx = mx%(cw+tw)
    my += tx*h/2
    var ty = my/h 
    var ry = my%h
    rx = tw+cw-rx
    ry -= h/2
    if(2*cw*ry > rx*h) {
      tx += 1
      ty += 1
    }
    if(2*cw*ry < -rx*h) {
      tx += 1
    }
    (tx, ty)
  }

  def hexCenterInRect(hx:Int, hy:Int, x:Int, y:Int, w:Int, h:Int):Boolean = {
    val (p1,p2) = hexToPixel(hx,hy)
    p1 > x && p2 > y && p1 < (x + w) && p2 < (y + h)
  }

  /**
  * Return the pixel coordinates of the center of the given hex.
  */
  def hexToPixel(x:Int, y:Int):(Int, Int) = {
    val xp = (x * (CELL_BODY_WIDTH + CELL_ARM_WIDTH)) + (CELL_WIDTH / 2)
    val yp = (y * CELL_HEIGHT) - (x * CELL_HEIGHT/2) + (CELL_HEIGHT / 2)
    (xp,yp)
  }

  def hexDist(x1:Int, y1:Int, x2:Int, y2:Int):Int = {
    val dx = x1 - x2
    val dy = y1 - y2
    (abs(dx) + abs(dy) + abs(dx - dy)) / 2
  }

}
