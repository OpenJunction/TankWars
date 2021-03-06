import sbt._
import java.io.File

class TowerProject(info: ProjectInfo) extends AndroidProject(info) {

  override def androidPlatformName = "android-7"

  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val stanfordRepo = "Stanford Maven 2 Repo" at "http://prpl.stanford.edu:8081/nexus/content/groups/public"


  // Note: I manually re-include xpp3 & json here for the java Sim
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

  <dependency org="xpp3" name="xpp3" rev="1.1.4c">
  </dependency>
  <dependency org="json" name="json" rev="1.0">
  </dependency>

  </dependencies>


  override def proguardOption = List(
    " -keep public class org.jivesoftware.** {*;}",
    "  -keep public class org.slf4j.** {*;} "
  ).mkString(" ")

}
