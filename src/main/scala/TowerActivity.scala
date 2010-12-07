package edu.stanford.junction.sample.tower_defense

import android.app.Activity
import android.app.NotificationManager
import android.os.Bundle
import android.util.Log
import android.view.Window
import android.view.View
import android.widget.TextView
import android.widget.Toast
import android.os.Handler
import android.os.Message
import java.util._


class TowerActivity extends Activity {

  /** A handle to the thread that's actually running the animation. */
  private var mGameThread:GameThread = null

  /** A handle to the View in which the game is running. */
  private var mGameView:GameView = null

  override def onCreate(savedInstanceState:Bundle) {
    super.onCreate(savedInstanceState)

    requestWindowFeature(Window.FEATURE_NO_TITLE)
    setContentView(R.layout.main)

    // get handles to the GameView from XML, and its GameThread
    mGameView = findViewById(R.id.game).asInstanceOf[GameView]
    mGameThread = mGameView.getThread().asInstanceOf[GameThread]

    // give the GameView a handle to the TextView used for messages
    mGameView.setTextView(findViewById(R.id.text).asInstanceOf[TextView])

  }

  /**
  * Invoked when the Activity loses user focus.
  */
  override def onPause() {
    super.onPause()
    mGameView.finish()
    System.exit(0)
  }

  override def onResume() {
    super.onResume()
    mGameView.getThread().unpause() // pause game when Activity pauses
  }

  override def onRestart() {
    super.onRestart()
    mGameView.getThread().unpause() // pause game when Activity pauses
  }


}
