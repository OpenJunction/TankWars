package edu.stanford.junction.sample.tower_defense
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.math._
import scala.collection.mutable.ArrayBuffer
import org.json._
import edu.umd.cs.piccolo.PCanvas
import edu.umd.cs.piccolo.nodes._
import edu.umd.cs.piccolo.util._
import edu.umd.cs.piccolo.PNode
import edu.umd.cs.piccolox.PFrame
import edu.umd.cs.piccolo.activities.PActivity
import edu.umd.cs.piccolo.activities.PActivity.PActivityDelegate
import edu.umd.cs.piccolo.event._
import javax.swing.SwingUtilities
import javax.imageio._
import java.util.Random
import java.awt.geom._
import java.awt.Graphics2D
import java.awt.Stroke
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.event.InputEvent
import java.awt.event.KeyEvent
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import edu.umd.cs.piccolo.PNode
import edu.umd.cs.piccolo.util.PDimension
import Board.Dir._
import edu.stanford.junction.props2._
import java.net.URI;
import edu.stanford.junction.JunctionMaker;
import edu.stanford.junction.provider.xmpp.XMPPSwitchboardConfig;
import edu.stanford.junction.api.activity._
import edu.stanford.junction.api.messaging._
import scala.collection.JavaConversions._
import javax.swing.Timer

object Img {

  def readResource(name: String): BufferedImage = {
    val url = this.getClass().getClassLoader().getResource(name)
    ImageIO.read(url)
  }

  val grassStyles: List[BufferedImage] = List(
    readResource("grass1.png"),
    readResource("grass2.png"),
    readResource("grass3.png"))

  val tank: BufferedImage = readResource("tank.png")
  val obstacle: BufferedImage = readResource("obstacle1.png")
  val explosion: BufferedImage = readResource("explosion.png")
  val bullet: BufferedImage = readResource("bullet.png")
}

class BoardView(prop: BoardProp, w: Double, h: Double) extends PNode {

  val userMap = new HashMap[String, UserView]
  setBounds(0, 0, w, h)
  val terrainSeed = 45

  override def setBounds(x: Double, y: Double, width: Double, height: Double): Boolean = {
    if (super.setBounds(x, y, width, height)) {
      return true
    }
    return false
  }

  override def intersects(aBounds: Rectangle2D): Boolean = {
    return getBounds().intersects(aBounds)
  }

  override def paint(context: PPaintContext) {
    val rng = new Random()
    rng.setSeed(terrainSeed)
    val g2: Graphics2D = context.getGraphics()
    for (x <- -1 until 12) {
      for (y <- -1 until 22) {
        val (xp, yp) = Board.hexToPixel(x, y)
        val grass = Img.grassStyles(rng.nextInt(Img.grassStyles.length))
        g2.drawImage(grass, xp - (Board.CELL_WIDTH / 2), yp - (Board.CELL_HEIGHT / 2), null)
      }
    }
    prop.eachObstacle { o =>
      val (xp, yp) = Board.hexToPixel(o.boardX, o.boardY)
      g2.drawImage(Img.obstacle, xp - (Board.CELL_WIDTH / 2), yp - (Board.CELL_HEIGHT / 2), null)
    }
  }

  def refresh() {
    repaint
    prop.eachUser { u =>
      userMap.get(u.id) match {
        case Some(uv) => {
          uv.animateToBoardPos(u.boardX, u.boardY, u.boardDir)
        }
        case _ => {
          val uv = new UserView(u.name)
          addChild(uv)
          uv.setBoardPos(u.boardX, u.boardY)
          userMap(u.id) = uv
        }
      }
    }
  }

  private def animateShot(x1: Double, y1: Double, x2: Double, y2: Double) {
    val line = new PPath(new Line2D.Double(x1, y1, x2, y2))
    val color = Color.ORANGE
    line.setStrokePaint(color)
    addChild(0, line)
    val act = line.animateToTransparency(1, 100)
    act.setDelegate(new PActivityDelegate {
	def activityFinished(act: PActivity) {
          removeChild(line)
	}
	def activityStepped(act: PActivity) {}
	def activityStarted(act: PActivity) {}
      })
  }

  def showFireHit(op: JSONObject, b: BoardProp) {
    for (
      u1 <- b.getUser(op.getString("fromId"));
      u2 <- b.getUser(op.getString("toId"));
      from <- userMap.get(u1.id);
      to <- userMap.get(u2.id)
    ) {
      val x1 = from.getXOffset()
      val y1 = from.getYOffset()
      val x2 = to.getXOffset()
      val y2 = to.getYOffset()
      animateShot(x1, y1, x2, y2)
    }
  }

  def showFireMiss(op: JSONObject, b: BoardProp) {
    for (
      u1 <- b.getUser(op.getString("fromId"));
      from <- userMap.get(u1.id)
    ) {
      val x1 = from.getXOffset()
      val y1 = from.getYOffset()
      val s = Board.Dir.step(u1.boardDir)
      val (toX, toY) = (u1.boardX + 10 * s._1, u1.boardY + 10 * s._2)
      val (x2, y2) = Board.hexToPixel(toX, toY)
      animateShot(x1, y1, x2, y2)
    }
  }

  def showDeath(op: JSONObject, b: BoardProp) {
    for (uv <- userMap.get(op.getString("userId"))) {
      val exp = new ExplosionView()
      addChild(exp)
      exp.setOffset(uv.getXOffset, uv.getYOffset)
      exp.start
      uv.removeFromParent()
      userMap.remove(op.getString("userId"))
    }
  }

}

class UserView(val name: String) extends PNode {

  val w = 35
  val h = 50
  setBounds(-w / 2, -h / 2, w, h)
  var toX = 0
  var toY = 0
  var curAnim: Option[PActivity] = None

  override def paint(context: PPaintContext) {
    val g2: Graphics2D = context.getGraphics()
    g2.drawImage(Img.tank, -w / 2, -h / 2, null)
  }

  def setBoardPos(x: Int, y: Int) {
    val (xp, yp) = Board.hexToPixel(x, y)
    setOffset(xp, yp)
    toX = xp
    toY = yp
  }

  def animateToBoardPos(boardX: Int, boardY: Int, dir: Board.Dir.Dir) {
    curAnim match {
      case Some(anim) => anim.terminate()
      case None => {}
    }
    val (x, y) = Board.hexToPixel(boardX, boardY)
    setOffset(toX, toY)
    toX = x
    toY = y
    val theta = Board.dirToTheta(dir)
    setRotation(theta)
    curAnim = Some(animateToPositionScaleRotation(
	toX.toDouble, toY.toDouble, 1.0, theta, 300))
  }

}

class ExplosionView extends PNode {

  val w = 128
  val h = 141
  setBounds(-w / 2, -h / 2, w, h)

  override def paint(context: PPaintContext) {
    val g2: Graphics2D = context.getGraphics()
    g2.drawImage(Img.explosion, -w / 2, -h / 2, null)
  }

  def start() {
    setScale(0.3)
    val act = animateToPositionScaleRotation(getXOffset, getYOffset, 1.0, 0.15, 1000)
    act.setDelegate(new PActivityDelegate {
	def activityFinished(act: PActivity) {
          removeFromParent()
	}
	def activityStepped(act: PActivity) {}
	def activityStarted(act: PActivity) {}
      })
  }

}

object KeyHandler extends PBasicInputEventHandler {

  private val keyStates = new HashMap[Int, Boolean] {
    override def default(i: Int) = false
  }
  private val moveListeners = new ListBuffer[(Dir => Unit)]
  private val fireListeners = new ListBuffer[(() => Unit)]

  def addDirectionHandler(l: (Dir => Unit)) {
    moveListeners += l
  }

  def addFireHandler(l: (() => Unit)) {
    fireListeners += l
  }

  override def keyPressed(e: PInputEvent) {
    keyStates(e.getKeyCode) = true
    val rng = new Random()
    if (keyStates(KeyEvent.VK_Q)) {
      moveListeners.foreach { _(NW) }
    } else if (keyStates(KeyEvent.VK_E)) {
      moveListeners.foreach { _(NE) }
    } else if (keyStates(KeyEvent.VK_Z)) {
      moveListeners.foreach { _(SW) }
    } else if (keyStates(KeyEvent.VK_C)) {
      moveListeners.foreach { _(SE) }
    } else if (keyStates(KeyEvent.VK_X)) {
      moveListeners.foreach { _(S) }
    } else if (keyStates(KeyEvent.VK_W)) {
      moveListeners.foreach { _(N) }
    } else if (keyStates(KeyEvent.VK_A)) {
      if (rng.nextInt(2) == 0) moveListeners.foreach { _(NW) }
      else moveListeners.foreach { _(SW) }
    } else if (keyStates(KeyEvent.VK_D)) {
      if (rng.nextInt(2) == 0) moveListeners.foreach { _(NE) }
      else moveListeners.foreach { _(SE) }
    } else if (keyStates(KeyEvent.VK_SPACE)) {
      fireListeners.foreach { _() }
    }
    e.setHandled(true)
  }

  override def keyReleased(e: PInputEvent) {
    keyStates(e.getKeyCode) = false
    e.setHandled(true)
  }
}

class Frame extends PFrame {
  override def initialize() {
    val rng = new Random()
    val w = 480
    val h = 854
    setSize(w, h)
    val layer = getCanvas().getLayer()

    val me = new User("aemon", rng.nextInt(5), rng.nextInt(5))
    val boardProp = new BoardProp("Board", 0, 0, w, h)

    val boardView = new BoardView(boardProp, w, h)
    layer.addChild(boardView)

    getCanvas().getRoot.getDefaultInputManager.setKeyboardFocus(KeyHandler)
    getCanvas().requestFocusInWindow()

    getCanvas().addInputEventListener(new PBasicInputEventHandler() {
	override def mouseClicked(e: PInputEvent) {
          getCanvas().getRoot.getDefaultInputManager.setKeyboardFocus(KeyHandler)
          getCanvas().requestFocusInWindow()
	}
      })

    KeyHandler.addDirectionHandler({ d: Dir =>
	boardProp.moveUser(me, d)
      })

    KeyHandler.addFireHandler({ () =>
	boardProp.fire(me)
      })

    def onSwing(todo: (() => Unit)) {
      if (SwingUtilities.isEventDispatchThread()) {
        todo()
      } else {
        SwingUtilities.invokeLater(new Runnable {
            override def run() {
              todo()
            }
          })
      }
    }

    boardProp.addChangeListener(new IPropChangeListener() {
	def getType = "change"
	def onChange(op: Object) = {

          /*
	  * Note:
	  * These are purely decorative event
	  * responses. We can't be guaranteed that
	  * any of these events will be received.
	  */
          onSwing { () =>
            op match {
              case obj: JSONObject => {
		obj.getString("type") match {
                  case "fireHitOp" =>
                  {
                    Sound.playShot
                    boardView.showFireHit(obj, boardProp)
                    for (target <- boardProp.getUser(obj.getString("toId"))) {
                      if (target == me && target.health <= 0) {
			boardProp.kill(me)
                      }
                    }
                  }
                  case "fireMissOp" =>
                  {
                    Sound.playShot
                    boardView.showFireMiss(obj, boardProp)
                  }
                  case "dieOp" =>
                  {
                    Sound.playExplosion
                    boardView.showDeath(obj, boardProp)
                  }
                  case _ =>
		}
              }
            }
            boardView.refresh()
          }
	}

      })

    boardProp.addChangeListener(new IPropChangeListener() {
	def getType = "sync"
	def onChange(data: Object) = {
          onSwing { () =>
            boardProp.getUser(me.id) match {
              case Some(u) => {}
              case None => boardProp.addUser(me)
            }
            boardView.refresh()
          }
	}
      })

    val aiTimer = new Timer(500, new ActionListener() {
	def actionPerformed(evt: ActionEvent) {
          onSwing { () =>
            boardProp.moveUser(me, Board.Dir.random)
          }
	}
      });

    // Junction initialization

    val jxURI = new URI("junction://sb.openjunction.org/tankwars#xmpp");
    val actor = new JunctionActor("participant") {
      override def getInitialExtras() = ListBuffer[JunctionExtra](boardProp)
      override def onMessageReceived(header: MessageHeader, msg: JSONObject) {
        System.err.println("OOPS: actor should not have seen message!")
      }
      override def onActivityJoin() {
        onSwing { () =>
          boardProp.addUser(me)
          //aiTimer.start(); 
        }
      }
      override def onActivityCreate() {}
    }
    val sb = JunctionMaker.getDefaultSwitchboardConfig(jxURI)
    val jxMaker = JunctionMaker.getInstance(sb)
    val jx = jxMaker.newJunction(jxURI, actor)
  }

}

//object Main {
//  def main(args: Array[String]) {
//    new Frame()
//  }
//}

