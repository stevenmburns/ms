package ms

import org.scalatest.{ Matchers, FlatSpec, GivenWhenThen}
import org.scalatest.prop.Checkers

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
  val max_cnt = 3.U
  val io = IO(new Bundle {
    val start = Input( Bool())
    val done = Output( Bool())
    val ld_req = DecoupledIO( new req_packet)
  })

  val cnt = RegInit( io.ld_req.bits.addr.cloneType, init=0.U)

  io.done := false.B

  io.ld_req.noenq
  when ( io.start && io.ld_req.ready) {
    val pkt = Wire(new req_packet)
    pkt.addr := cnt
    pkt.meta := 0.U
    io.ld_req.enq( pkt)
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

  val ag = Module( new addr_gen)

  val inflight = RegInit( init=0.U(8.W))

  ag.io.start := io.start
  io.ld_req <> ag.io.ld_req

  io.done := ag.io.done


}


class addr_gen_Tester( tag: String, factory: () => addr_gen) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    check(chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {
         poke( c.io.start, 0)
         poke( c.io.ld_req.ready, 1)
         expect( c.io.done, 0)
         step( 1)
         poke( c.io.start, 1)
         expect( c.io.ld_req.valid, 1)
         expect( c.io.ld_req.bits.addr, 0)
         expect( c.io.done, 0)
         step( 1)
         expect( c.io.ld_req.valid, 1)
         expect( c.io.ld_req.bits.addr, 1)
         expect( c.io.done, 0)
         step( 1)
         poke( c.io.ld_req.ready, 0)
         expect( c.io.ld_req.valid, 0)
         expect( c.io.done, 0)
         step( 1)
         poke( c.io.ld_req.ready, 1)
         expect( c.io.ld_req.valid, 1)
         expect( c.io.ld_req.bits.addr, 2)
         expect( c.io.done, 1)
       }
    })
  }
}

class MSTester( tag: String, factory: () => MS) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    check(chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {
       }
    })
  }
}

class addr_gen_Test extends addr_gen_Tester( "addr_gen", () => new addr_gen)

class MSTest extends MSTester( "MS", () => new MS)

