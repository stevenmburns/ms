package ms

import org.scalatest.{ Matchers, FlatSpec, GivenWhenThen}

import chisel3._
import chisel3.util._
import chisel3.iotesters._
import org.scalatest.prop.Checkers

import scala.io.Source

class GenericTest extends ChiselFlatSpec with Checkers {
  val extra = Seq("-sverilog -notice")

  val optionsManager = new TesterOptionsManager {
    testerOptions = testerOptions.copy(
//      backendName="treadle",
      backendName="verilator",
//      backendName="vcs",

//    vcsCommandEdits = """s/\+vcs\+initreg\+random //""",
//      moreVcsFlags=extra,
      isVerbose=true
    )
  }
}


