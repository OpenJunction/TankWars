package edu.stanford.junction.sample.tower_defense
import scala.util.Random._
import org.json.JSONObject
import scala.collection.JavaConversions._

object Data{

  def randomUserInProp(prop:BoardProp):Option[User] = {
    val users: List[User] = prop.getAllUsers
    shuffle(users).headOption
  }


}
