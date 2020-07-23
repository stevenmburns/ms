package building_blocks

import org.scalatest.{ Matchers, FlatSpec, GivenWhenThen}

import chisel3._
import chisel3.util._
import chisel3.iotesters._

class req_packet extends Bundle {
  val addr = UInt(64.W)
  val meta = UInt(24.W)
}

class rsp_packet extends Bundle {
  val data = UInt(512.W)
  val meta = UInt(24.W)
}


class addr_gen extends Module {
  val max_cnt = 7.U
  val io = IO(new Bundle {
    val start = Input( Bool())
    val done = Output( Bool())
    val addr = DecoupledIO( UInt(64.W))
  })

  val cnt = RegInit( io.addr.bits.cloneType, init=0.U)

  io.done := false.B

  io.addr.noenq
  when ( io.start && io.addr.ready) {
    io.addr.enq( cnt)
    when ( cnt === max_cnt - 1.U) {
      io.done := true.B
    } .otherwise {
      cnt := cnt + 1.U
    }
  }
}

class MS extends Module {
  val io = IO(new Bundle {
    val start = Input( Bool())
    val done = Output( Bool())
    val ld_req = DecoupledIO( new req_packet)
    val ld_rsp = Input(ValidIO( new rsp_packet))
  })

  io.done := DontCare
  io.ld_req.noenq

}


class addr_gen_Tester( tag: String, factory: () => addr_gen) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {
         poke( c.io.start, 0)
         poke( c.io.addr.ready, 1)
         step( 1)
         poke( c.io.start, 1)
         expect( c.io.addr.valid, 1)
         expect( c.io.addr.bits, 0)
         step( 1)
         expect( c.io.addr.valid, 1)
         expect( c.io.addr.bits, 1)
         step( 1)
         poke( c.io.addr.ready, 0)
         expect( c.io.addr.valid, 0)
         step( 1)
       }
    }
  }
}

class MSTester( tag: String, factory: () => MS) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {
       }
    }
  }
}

class addr_gen_Test extends addr_gen_Tester( "addr_gen", () => new addr_gen)

class MSTest extends MSTester( "MS", () => new MS)

