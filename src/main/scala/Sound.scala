package edu.stanford.junction.sample.tower_defense

import java.io.File
import java.io.IOException
import java.net.URL
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioInputStream
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.DataLine
import javax.sound.sampled.LineUnavailableException
import javax.sound.sampled.SourceDataLine
import javax.sound.sampled.DataLine._
import android.media.SoundPool
import android.media.AudioManager
import android.content.Context


object Sound {

  //val BUFFER_SIZE:Int = 128000
  private var shot:Int = -1
  private var explosion:Int = -1
	val MAX_STREAMS:Int = 10
	private val pool:SoundPool = new SoundPool(MAX_STREAMS, AudioManager.STREAM_MUSIC, 0) 	

  def playShot() = pool.play(shot, 1.0f, 1.0f, 1, 0, 1.0f)
  def playExplosion() = pool.play(explosion, 1.0f, 1.0f, 1, 0, 1.0f)

	def loadSounds(context:Context) = {
		shot = pool.load(context, R.raw.shot, 1)
		explosion = pool.load(context, R.raw.explosion, 1)
	}
	
  /*def playURL(url:URL){
    (new Thread(){
      override def run(){
	val audioStream = AudioSystem.getAudioInputStream(url)      
	val audioFormat = audioStream.getFormat()
	val info = new DataLine.Info(classOf[SourceDataLine], audioFormat)
	val sourceLine = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
	sourceLine.open(audioFormat)
	sourceLine.start()
	var nBytesRead = 0
	val abData = new Array[Byte](BUFFER_SIZE)
	while (nBytesRead != -1) {
	  nBytesRead = audioStream.read(abData, 0, abData.length)
	  if (nBytesRead >= 0) {
	    val nBytesWritten = sourceLine.write(abData, 0, nBytesRead)
	  }
	}
	sourceLine.drain()
	sourceLine.close()
      }
    }).start
  }*/

}


