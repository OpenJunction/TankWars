import sbt._
import java.io.File

class TowerProject(info: ProjectInfo) extends AndroidProject(info) {

  override def androidPlatformName = "android-7"

  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val stanfordRepo = "Stanford Maven 2 Repo" at "http://prpl.stanford.edu:8081/nexus/content/groups/public"

  override def ivyXML = 
  <dependencies>

  <dependency org="org.piccolo2d" name="piccolo2d-core" rev="1.3">
  </dependency>

  <dependency org="org.piccolo2d" name="piccolo2d-extras" rev="1.3">
  </dependency>

  <dependency org="edu.stanford.prpl.junction" name="AndroidJunction" rev="0.6.7-SNAPSHOT">
  <exclude module="slf4j-api"/>
  <exclude module="slf4j-android"/>
  <exclude module="slf4j-simple"/>
  </dependency>

  </dependencies>


  override def proguardOption = List(
    " -keep public class org.jivesoftware.** {*;}",
    "  -keep public class org.slf4j.** {*;} "
  ).mkString(" ")

}
