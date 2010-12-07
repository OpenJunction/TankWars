package edu.stanford.junction.sample.tower_defense

import android.content.res.Resources
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.RectF
import android.graphics.drawable.Drawable
import android.view.MotionEvent
import java.util.Random
import scala.collection.JavaConversions._
import scala.math._
import java.util.Date
import scala.collection.mutable.ListBuffer


class CenteredBitmapSprite(b:Bitmap) extends Sprite{
  private val (w, h) = (b.getWidth, b.getHeight)
  private val paint = new Paint

  def paintTransformed(c:Canvas){
    c.translate(-w/2, -h/2)
    paint.setAlpha((255.0 * alpha).toInt)
    c.drawBitmap(b, 0, 0, paint)
  }
}

trait Sprite {
  private var anims = new ListBuffer[Animation]
  var x:Double = 0
  var y:Double = 0
  var rotation:Double = 0
  var scale:Double = 1.0
  var alpha:Double = 1.0

  def addAnim(anim:Animation):Animation = {
    anims += anim
    anim
  }

  def fastForwardAndClearAnims(){
    anims.foreach(_.fastForward())
    anims.clear
  }

  def update(time:Long){
    anims.foreach{ ea:Animation => 
      ea.update(time)
    }
    anims = anims.filter{ea:Animation => !(ea.isFinished(time))}
  }

  def paint(c:Canvas){
    c.save()
    c.translate(x.toFloat, y.toFloat)
    c.rotate(((rotation / Pi) * 180.0).toFloat)
    c.scale(scale.toFloat, scale.toFloat)
    paintTransformed(c)
    c.restore()
  }

  def paintTransformed(c:Canvas)

  def animMoveTo(x:Double, y:Double, dur:Int):Animation = {
    val d = new Date()
    addAnim(new TranslationAnim(this, d.getTime, dur, x, y))
  }

  def animAlphaTo(a:Double, dur:Int):Animation = {
    val d = new Date()
    addAnim(new AlphaAnim(this, d.getTime, dur, a))
  }

  def animScaleTo(s:Double, dur:Int):Animation = {
    val d = new Date()
    addAnim(new ScaleAnim(this, d.getTime, dur, s))
  }

  def animRotationTo(r:Double, dur:Int):Animation = {
    val d = new Date()
    addAnim(new RotationAnim(this, d.getTime, dur, r))
  }

}

trait Animation{
  def update(time:Long)
  def target:Sprite
  def duration:Int
  def startTime:Long
  def fastForward() { update(startTime + duration) }
  def isFinished(time:Long):Boolean = (time - startTime) >= duration
  def addFinishListener(l:(() => Unit))
}

abstract class BaseAnim(s:Sprite, start:Long, dur:Int) extends Animation{
  protected val finishListeners = new ListBuffer[()=>Unit]
  def target:Sprite = s
  def duration:Int = dur
  def startTime:Long = start
  def addFinishListener(l:(() => Unit)){
    finishListeners += l
  }
  def checkFinished(time:Long){
    if(isFinished(time)) finishListeners.foreach(_())
  }
}


class AlphaAnim(s:Sprite, start:Long, dur:Int, toAlpha:Double) extends BaseAnim(s,start,dur){
  private val oAlpha = s.alpha

  def update(time:Long){
    val elapsed = time - startTime
    var r = min(1.0, elapsed.toDouble / dur.toDouble)
    s.alpha = oAlpha + (toAlpha - oAlpha) * r
    checkFinished(time)
  }

}

class TranslationAnim(s:Sprite, start:Long, dur:Int, toX:Double, toY:Double) extends BaseAnim(s,start,dur){
  private val oX = s.x
  private val oY = s.y
  def update(time:Long){
    val elapsed = time - startTime
    var r = min(1.0, elapsed.toDouble / dur.toDouble)
    s.x = oX + (toX - oX) * r
    s.y = oY + (toY - oY) * r
    checkFinished(time)
  }
}

class RotationAnim(s:Sprite, start:Long, dur:Int, toTheta:Double) extends BaseAnim(s,start,dur){
  private val oTheta = s.rotation
  def update(time:Long){
    val elapsed = time - startTime
    var r = min(1.0, elapsed.toDouble / dur.toDouble)
    s.rotation = oTheta + (toTheta - oTheta) * r
    checkFinished(time)
  }
}

class ScaleAnim(s:Sprite, start:Long, dur:Int, toScale:Double) extends BaseAnim(s,start,dur){
  private val oScale = s.rotation
  def update(time:Long){
    val elapsed = time - startTime
    var r = min(1.0, elapsed.toDouble / dur.toDouble)
    s.scale = oScale + (toScale - oScale) * r
    checkFinished(time)
  }
}
