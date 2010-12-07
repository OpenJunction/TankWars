package edu.stanford.junction.sample.tower_defense

import android.content.Context
import android.content.res.Resources
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.RectF
import android.graphics.drawable.Drawable
import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.os.Vibrator
import android.util.AttributeSet
import android.view.KeyEvent
import android.view.SurfaceHolder
import android.view.SurfaceView
import android.view.View
import android.view.MotionEvent
import android.view.WindowManager
import android.widget.TextView
import android.util.Log
import java.util.Random
import scala.concurrent._
import edu.stanford.junction.android.AndroidJunctionMaker
import edu.stanford.junction.Junction
import edu.stanford.junction.api.activity.JunctionActor
import edu.stanford.junction.api.activity.JunctionExtra
import edu.stanford.junction.api.messaging.MessageHeader
import edu.stanford.junction.provider.xmpp.XMPPSwitchboardConfig
import edu.stanford.junction.props2._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import scala.concurrent.MailBox
import java.net.URI
import java.util.Date
import org.json._
import scala.math._
import java.util.Timer;
import java.util.TimerTask;
import java.io.FileOutputStream
import java.io.FileInputStream

object GameView {
  val TAG: String = "TowerActivity"
  val seed = 43
}

class GameView(context: Context, attrs: AttributeSet) extends SurfaceView(context, attrs) with SurfaceHolder.Callback {

  /** Pointer to the text view to display "Paused.." etc. */
  private var mStatusText: TextView = null
  getHolder().addCallback(this)

  /** Kinda nasty to get absolute screen dims here. But better than waiting for onLayout.. */
  val display = context.getSystemService(Context.WINDOW_SERVICE).asInstanceOf[WindowManager].getDefaultDisplay()
  private val w = display.getWidth()
  private val h = display.getHeight()

  private val boardProp = new BoardProp("Board", 0, 0, w, h)
  private val rng = new Random()
  private var me = new User("aemon", 2, 5)

  Sound.loadSounds(context)

  private val timerHandler = new Handler();
  class GameTimerTask(runnable: Runnable) extends TimerTask {
    def run() {
      timerHandler.post(runnable)
    }
  }
  def runAfter(delay: Long, todo: () => Unit) {
    new Timer().schedule(new GameTimerTask(new Runnable {
      def run = todo()
    }), delay)
  }

  boardProp.addChangeListener(new IPropChangeListener() {
    def getType = "change"
    def onChange(o: Object) = {
      o match {
        case op: JSONObject => {
          op.getString("type") match {
            case "fireHitOp" =>
              {
              Sound.playShot
              thread.showFireHit(op, boardProp)
              for (target <- boardProp.getUser(op.getString("toId"))) {
                if (target == me && target.health <= 0) {
                  boardProp.kill(me)
                }
              }
            }
            case "fireMissOp" =>
              {
              Sound.playShot
              thread.showFireMiss(op, boardProp)
            }
            case "fireDieOp" =>
              {
              Sound.playExplosion
              thread.showDeath(op, boardProp)
              if (op.getString("userId") == me.id) {
                runAfter(2000, { () =>
                  me = new User("aemon", 2 + rng.nextInt(2), 5 + rng.nextInt(2))
                  me.boardX = 2
                  me.boardY = 5
                  boardProp.addUser(me)
                })
              }
            }
            case _ =>
          }
        }
        case _ =>
      }
      thread.update(boardProp)
    }
  })

  boardProp.addChangeListener(new IPropChangeListener() {
    def getType = "sync"
    def onChange(data: Object) = {
      boardProp.getUser(me.id) match {
        case Some(u) => {}
        case None => {
          getSavedId() match {
            case Some(id) => {
              boardProp.getUser(id) match {
                case Some(u) => {
                  me = u
                  thread.update(boardProp)
                }
                case None => boardProp.addUser(me)
              }
            }
            case None => boardProp.addUser(me)
          }
        }
      }
      thread.init(boardProp)
    }
  })

  val actor = new JunctionActor("participant") {
    override def getInitialExtras() = ListBuffer[JunctionExtra](boardProp)
    override def onMessageReceived(header: MessageHeader, msg: JSONObject) {
      Log.i("TowerActivity", "OOPS: actor should not have seen message!")
    }
    override def onActivityJoin() {
      Log.i("TowerActivity", "Joined activity!")
      //boardProp.addUser(me)
    }
    override def onActivityCreate() {
      thread.init(boardProp)
      boardProp.addUser(me)
    }
  }

  def initJunctionConnection() {
    val jxURI = new URI("junction://sb.openjunction.org/tankwars#xmpp");
    val config = AndroidJunctionMaker.getDefaultSwitchboardConfig(jxURI)
    val jxMaker = AndroidJunctionMaker.getInstance(config)
    val jx = jxMaker.newJunction(jxURI, actor)
  }

  /** The thread that actually draws the animation */
  private val thread = new GameThread(w, h, getHolder(), context, new Handler() {
    override def handleMessage(m: Message) {
      mStatusText.setVisibility(m.getData().getInt("viz"))
      mStatusText.setText(m.getData().getString("text"))
    }
  })

  // make sure we get key events
  setFocusable(true)

  /**
   * Fetches the animation thread corresponding to this GameView.
   * 
   * @the animation thread
   */
  def getThread(): GameThread = thread

  def finish() {
    //    boardProp.kill(me)
    saveUserId()
    actor.leave()
    thread.setRunning(false)
  }

  private def saveUserId() = {
    val sessionId = actor.getJunction().getSessionID()
    val userId = me.id

    //for now we'll just save the userId
    val fos: FileOutputStream = context.openFileOutput("savedId", Context.MODE_PRIVATE)
    fos.write(userId.getBytes())
    fos.close()
  }

  private def getSavedId(): Option[String] = {
    try {
      val fis: FileInputStream = context.openFileInput("savedId")
      val len = fis.available()
      val bytes = new Array[Byte](len)
      fis.read(bytes)
      Some(new String(bytes))
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Standard override to get key-press events.
   */
  override def onKeyDown(keyCode: Int, msg: KeyEvent): Boolean = {
    thread.doKeyDown(keyCode, msg)
  }

  /**
   * Standard override to get key-press events.
   */
  override def onKeyUp(keyCode: Int, msg: KeyEvent): Boolean = {
    thread.doKeyUp(keyCode, msg)
  }

  override def onTouchEvent(msg: MotionEvent): Boolean = {
    if (msg.getAction == MotionEvent.ACTION_UP) {
      val (px, py) = (msg.getX(), msg.getY())
      val (hx, hy) = Board.pixelToHex(px.toInt, py.toInt)
      boardProp.getUser(me.id) match {
        case Some(u) => {
          val dir = Board.pixelPointsToDir(Board.hexToPixel(u.boardX, u.boardY), (px.toInt, py.toInt))
          boardProp.userAt(hx, hy) match {
            case Some(otherU) => {
              boardProp.rotateUser(u, dir)
              boardProp.fire(u)
            }
            case _ => {
              if (u.boardDir == dir)
                boardProp.moveUser(u, dir)
              else
                boardProp.rotateUser(u, dir)
            }
          }
        }
        case _ => {
          println("Sorry, no user found.")
        }
      }
      true
    } else {
      return true
    }
  }

  /**
   * Standard window-focus override. Notice focus lost so we can pause on
   * focus lost. e.g. user switches to take a call.
   */
  override def onWindowFocusChanged(hasWindowFocus: Boolean) {
    if (!hasWindowFocus) thread.pause()
  }

  /**
   * Installs a pointer to the text view used for messages.
   */
  def setTextView(textView: TextView) {
    mStatusText = textView
  }

  /* Callback invoked when the surface dimensions change. */
  override def surfaceChanged(holder: SurfaceHolder, format: Int, width: Int, height: Int) {
    thread.setSurfaceSize(width, height)
  }

  /*
* Callback invoked when the Surface has been created and is ready to be
* used.
*/
  def surfaceCreated(holder: SurfaceHolder) {
    // start the thread here so that we don't busy-wait in run()
    // waiting for the surface to be created
    println("Surface created. Starting thread...")
    thread.setRunning(true)
    thread.start()
    initJunctionConnection()
  }

  /*
* Callback invoked when the Surface has been destroyed and must no longer
* be touched. WARNING: after this method returns, the Surface/Canvas must
* never be touched again!
*/
  def surfaceDestroyed(holder: SurfaceHolder) {
    // we have to tell thread to shut down & wait for it to finish, or else
    // it might touch the Surface after we return and explode
    var retry = true
    thread.setRunning(false)
    while (retry) {
      try {
        thread.join()
        retry = false
      } catch {
        case e: InterruptedException => {}
      }
    }
  }
}

case class Update(prop: BoardProp)

class GameThread(w: Int, h: Int, surfaceHolder: SurfaceHolder, context: Context, handler: Handler) extends Thread {

  private var mRun = true

  private val rng = new Random()
  rng.setSeed(GameView.seed)

  private var mCanvasWidth = 1
  private var mCanvasHeight = 1

  private val mHandler: Handler = handler
  private val mContext: Context = context
  private val mSurfaceHolder: SurfaceHolder = surfaceHolder

  private val tank1Image = BitmapFactory.decodeResource(context.getResources(), R.drawable.tank1)
  private val tank2Image = BitmapFactory.decodeResource(context.getResources(), R.drawable.tank2)
  private val tank3Image = BitmapFactory.decodeResource(context.getResources(), R.drawable.tank3)
  private val tank4Image = BitmapFactory.decodeResource(context.getResources(), R.drawable.tank4)
  private val tank5Image = BitmapFactory.decodeResource(context.getResources(), R.drawable.tank5)

  private val bulletImage = BitmapFactory.decodeResource(context.getResources(), R.drawable.bullet)
  private val obstacleImage = BitmapFactory.decodeResource(context.getResources(), R.drawable.obstacle1)
  private val explosionImage = BitmapFactory.decodeResource(context.getResources(), R.drawable.explosion)
  private val grass1Image = BitmapFactory.decodeResource(context.getResources(), R.drawable.grass1)
  private val grass2Image = BitmapFactory.decodeResource(context.getResources(), R.drawable.grass2)
  private val grass3Image = BitmapFactory.decodeResource(context.getResources(), R.drawable.grass3)

  private val grasses: List[Bitmap] = List(grass1Image, grass2Image, grass3Image)
  private val tanksByColor: Map[String, Bitmap] = Map(
    ("green", tank1Image), ("blue", tank2Image), ("purple", tank3Image), ("yellow", tank4Image), ("black", tank5Image))

  private val sprites = new HashSet[Sprite]
  private val userMap = new HashMap[String, UserV]

  val res = context.getResources()
  // load background image as a Bitmap instead of a Drawable b/c
  // we don't need to transform it and it's faster to draw this way
  private var mBackgroundImage = Bitmap.createBitmap(w, h, Bitmap.Config.ARGB_8888)
  paintBackground()

  // Initialize paints for speedometer
  val firePaint = new Paint()
  firePaint.setAntiAlias(true)
  firePaint.setARGB(255, 0, 255, 0)

  /**
   * Paint the hex-grid background onto mBackgroundImage.
   * This is only done once for every time the surface is resized.
   */
  def paintBackground() {
    val c = new Canvas(mBackgroundImage)
    val rng = new Random()
    rng.setSeed(GameView.seed)
    for (x <- -1 until 12) {
      for (y <- -1 until 22) {
        val (xp, yp) = Board.hexToPixel(x, y)
        val grass = grasses(rng.nextInt(grasses.length))
        c.drawBitmap(grass, xp - (Board.CELL_WIDTH / 2), yp - (Board.CELL_HEIGHT / 2), null)
      }
    }
  }

  /**
   * Starts the game, setting parameters for the current difficulty.
   */
  def doStart() {
    mSurfaceHolder.synchronized {}
  }

  /**
   * Pauses the physics update & animation.
   */
  def pause() {
    mSurfaceHolder.synchronized {}
  }

  /**
   * Resumes from a pause.
   */
  def unpause() {
    mSurfaceHolder.synchronized {}
  }

  /**
   * Restores game state from the indicated Bundle. Typically called when
   * the Activity is being restored after having been previously
   * destroyed.
   * 
   * @param savedState Bundle containing the game state
   */
  def restoreState(savedState: Bundle) {
    mSurfaceHolder.synchronized {}
  }

  def init(prop: BoardProp) {
    mSurfaceHolder.synchronized {
      prop.eachObstacle { o =>
        val v = new CenteredBitmapSprite(obstacleImage)
        val (x, y) = Board.hexToPixel(o.boardX, o.boardY)
        v.x = x
        v.y = y
        sprites += v
      }
    }
  }

  def update(prop: BoardProp) {
    mSurfaceHolder.synchronized {
      prop.eachUser { u =>
        val uv = userMap.get(u.id) match {
          case Some(uv) => uv
          case _ => {
            val uv = new UserV(tanksByColor(u.color))
            sprites += uv
            userMap(u.id) = uv
            uv
          }
        }
        uv.animateToBoardPos(u.boardX, u.boardY, u.boardDir)
      }
    }
  }

  private def animateShot(x1: Double, y1: Double, x2: Double, y2: Double, hit: Boolean) {
    val bullet = new CenteredBitmapSprite(bulletImage)
    bullet.x = x1
    bullet.y = y1
    sprites += bullet
    val anim = bullet.animMoveTo(x2, y2, 200)
    anim.addFinishListener { () =>
      sprites -= bullet
      if (hit) {
        // Show a mini-explosion on hit
        val exp = new CenteredBitmapSprite(explosionImage)
        sprites += exp
        exp.scale = 0.2
        exp.x = x2
        exp.y = y2
        val anim = exp.animAlphaTo(0.2, 300)
        anim.addFinishListener { () => sprites -= exp }
      }
    }
  }

  def showFireHit(op: JSONObject, b: BoardProp) {
    mSurfaceHolder.synchronized {
      for (
        u1 <- b.getUser(op.getString("fromId"));
        u2 <- b.getUser(op.getString("toId"));
        from <- userMap.get(u1.id);
        to <- userMap.get(u2.id)
      ) {
        val x1 = from.x
        val y1 = from.y
        val x2 = to.x
        val y2 = to.y
        animateShot(x1, y1, x2, y2, true)
      }
    }
  }

  def showFireMiss(op: JSONObject, b: BoardProp) {
    mSurfaceHolder.synchronized {
      for (
        u1 <- b.getUser(op.getString("fromId"));
        from <- userMap.get(u1.id)
      ) {
        val x1 = from.x
        val y1 = from.y
        val s = Board.Dir.step(u1.boardDir)
        val (toX, toY) = (u1.boardX + 10 * s._1, u1.boardY + 10 * s._2)
        val (x2, y2) = Board.hexToPixel(toX, toY)
        animateShot(x1, y1, x2, y2, false)
      }
    }
  }

  def showDeath(op: JSONObject, b: BoardProp) {
    mSurfaceHolder.synchronized {
      for (uv <- userMap.get(op.getString("userId"))) {
        sprites -= uv
        userMap.remove(op.getString("userId"))
        val exp = new CenteredBitmapSprite(explosionImage)
        sprites += exp
        exp.scale = 0.2
        exp.x = uv.x
        exp.y = uv.y
        exp.animAlphaTo(0.2, 500)
        val anim = exp.animScaleTo(1.0, 500)
        anim.addFinishListener { () => sprites -= exp }
      }
    }
  }

  override def run() {
    while (mRun) {
      var c: Canvas = null
      try {
        c = mSurfaceHolder.lockCanvas(null)
        mSurfaceHolder.synchronized {
          val t = (new Date()).getTime
          sprites.foreach(_.update(t))
          c.drawBitmap(mBackgroundImage, 0, 0, null)
          sprites.foreach(_.paint(c))
        }
      } finally {
        // do this in a finally so that if an exception is thrown
        // during the above, we don't leave the Surface in an
        // inconsistent state
        if (c != null) {
          mSurfaceHolder.unlockCanvasAndPost(c)
        }
      }
      Thread.sleep(30)
    }
  }

  /**
   * Used to signal the thread whether it should be running or not.
   * Passing true allows the thread to run passing false will shut it
   * down if it's already running. Calling start() after this was most
   * recently called with false will result in an immediate shutdown.
   * 
   * @param b true to run, false to shut down
   */
  def setRunning(b: Boolean) {
    mRun = b
  }

  /* Callback invoked when the surface dimensions change. */
  def setSurfaceSize(width: Int, height: Int) {
    // synchronized to make sure these all change atomically
    mSurfaceHolder.synchronized {
      mCanvasWidth = width
      mCanvasHeight = height

      // don't forget to resize the background image
      mBackgroundImage = Bitmap.createScaledBitmap(
        mBackgroundImage, width, height, true)

      paintBackground()
    }
  }

  /**
   * Handles a key-down event.
   * 
   * @param keyCode the key that was pressed
   * @param msg the original event object
   * @true
   */
  def doKeyDown(keyCode: Int, msg: KeyEvent): Boolean = {
    mSurfaceHolder.synchronized {
      // if (keyCode == KeyEvent.KEYCODE_DPAD_UP) okStart = true
      // if (keyCode == KeyEvent.KEYCODE_DPAD_DOWN) okStart = true
      // if (keyCode == KeyEvent.KEYCODE_S) okStart = true
      //            || keyCode == KeyEvent.KEYCODE_SPACE) {
      //            || keyCode == KeyEvent.KEYCODE_W) {
      false
    }
  }

  /**
   * Handles a key-up event.
   * 
   * @param keyCode the key that was pressed
   * @param msg the original event object
   * @true if the key was handled and consumed, or else false
   */
  def doKeyUp(keyCode: Int, msg: KeyEvent): Boolean = {
    mSurfaceHolder.synchronized {}
    false
  }

}
