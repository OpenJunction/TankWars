package edu.stanford.junction.sample.tower_defense

import edu.stanford.junction.android.AndroidJunctionMaker
import android.app.Activity
import android.app.NotificationManager
import android.os.Bundle
import android.util.Log
import android.net.Uri
import android.view.Window
import android.view.View
import android.widget.TextView
import android.widget.Toast
import android.os.Handler
import android.os.Message
import android.view.Menu;
import android.view.MenuItem;
import java.util._
import java.net.URI


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

    if (AndroidJunctionMaker.isJoinable(this)) {
      try{
	val sessionUri = new URI(getIntent().getStringExtra("invitationURI"));
	mGameView.setJunctionURI(sessionUri)
      }
      catch{
	case e:Exception => e.printStackTrace(System.err)
      }
    }

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

  override def onCreateOptionsMenu(menu:Menu):Boolean =  true

  override def onPreparePanel(featureId:Int, view:View, menu:Menu):Boolean = {
    menu.clear();
    menu.add(0, 0, 0, "Start Statistics Collection.");
    menu.add(0, 1, 0, "Stop and Save Statistics.");
    return true;
  }

  override def onOptionsItemSelected(item:MenuItem):Boolean = {
    item.getItemId() match{
      case 0 => {
	mGameView.startStats();
	true
      }
      case 1 => {
	mGameView.saveStats();
	true
      }
      case _ => false
    }
  }


}
