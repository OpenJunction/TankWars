package edu.stanford.junction.sample.tower_defense

import android.content.Context
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


class UserV(b:Bitmap) extends CenteredBitmapSprite(b){

  def animateToBoardPos(boardX:Int, boardY:Int, dir:Board.Dir.Dir){
    fastForwardAndClearAnims()
    val (px, py) = Board.hexToPixel(boardX, boardY)
    rotation = Board.dirToTheta(dir)
    animMoveTo(px.toDouble, py.toDouble, 300)
  }

}
