package org.hinz.septa.server

import org.hinz.septa._

import org.scalatest._
import org.scalatest.matchers._

import java.util.{Date,Calendar}

class ServerSpec extends Spec with ShouldMatchers {
  describe("Server") {
    it("should handle new data stuff correctly") {
      val db = BusData(0,"",0,0,new Date(), "","")
      val rt = BusRecord("","","","","","","","2")

      new Server(null).isNewData(db,rt) should equal(false)

      val c = Calendar.getInstance()
      c.setTime(new Date())
      c.set(Calendar.MINUTE, c.get(Calendar.MINUTE) - 4)
      
      val db2 = BusData(0,"",0,0,c.getTime, "","")

      new Server(null).isNewData(db2,rt) should equal(true)
    }
  }
}
