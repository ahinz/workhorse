package org.hinz.septa;

import scala.xml._
import scala.io.Source


import swing._
import scala.swing.event._
import java.awt.geom._
import java.awt.Color

case class LatLon(lat:Double,lon:Double) {
  def toPoint2D = new Point2D.Double(lon,lat)
}

class GISPanel(pts: List[LatLon], dots:List[LatLon]) extends Panel {
 
  val lats = pts.map(_.lat)
  val lons = pts.map(_.lon)

  val boundsLat = (lats.max, lats.min)
  val boundsLon = (lons.max, lons.min)

  val xScale:Double = 950.0 //size.width
  val yScale:Double = 550.0 //size.height

  val scaleTransform = AffineTransform.getScaleInstance(
    xScale / (boundsLon._1 - boundsLon._2),
    yScale/ (boundsLat._2 - boundsLat._1))

  val translateTransform = AffineTransform.getTranslateInstance(
    - boundsLon._2,  - boundsLat._1)

  translateTransform.preConcatenate(scaleTransform)

  val transform = translateTransform

  listenTo(mouse.clicks)

  def d(x1: Double, y1:Double, x2:Double, y2: Double) =
    math.sqrt((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1-y2))

  def doClick(p: Point) = {
  }

  reactions += {
    case MouseClicked(_,p,_,_,_) => doClick(p)
  }

  override def paint(g: Graphics2D) = {

    val colors = List(Color.BLUE,Color.GREEN)

    var k = 0

    g.setColor(Color.WHITE)
    g.fillRect(0,0,xScale.toInt,yScale.toInt)
    g.setColor(Color.BLACK)

    pts.zip(pts.tail).map(p => {
      val p1:Point2D = transform.transform(p._1.toPoint2D, null)
      val p2:Point2D = transform.transform(p._2.toPoint2D, null)
      
      g.drawLine(p1.getX.toInt, p1.getY.toInt, p2.getX.toInt, p2.getY.toInt)
    })
    
    dots.map(p => {
      val p1:Point2D = transform.transform(p.toPoint2D,null)

      g.fillOval(p1.getX.toInt - 4,p1.getY.toInt - 4,9,9)
    })
  }
}

/**
object HelloWorld extends SimpleSwingApplication {

  val db = "/Users/ahinz/src/scala/septa/devdb.db"
    
  val r = new RouteLoader(db)

 val lat = 39.951881
 val lon =-75.158173

  val routepts = r.loadRoutePoints(Map("route_id" -> "1"))
  println("Loaded " + routepts.length + " points!")

  val pts = RouteProcessor.distanceOnRoute(routepts, LatLon(lat,lon)).get

  val sph = List(LatLon(lat,lon)) //LatLon(p1.lat,p1.lon),LatLon(p2.lat,p2.lon),LatLon(lat,lon))

  def top = new MainFrame {
    size = new java.awt.Dimension(1000,600)
    preferredSize = new Dimension(1000,600)
    title = "Hello, World!"
    contents = new GISPanel(routepts.map(x => LatLon(x.lat,x.lon)), sph)
  }
}*/

