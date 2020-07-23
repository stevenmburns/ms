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

class ScratchPad extends Module {
  val cacheline_bits = 512
  val log2banks = 2
  val banks = 1 << log2banks
  val sram_bits = 18 // addresses in units of BLOCKIN bytes
  val addr_bits = sram_bits - log2banks
  val word_bits = 128

  require( cacheline_bits == word_bits * banks)

  val io = IO(new Bundle {
    val sram_wr_addr = Input(UInt(sram_bits.W))
    val cacheline = Input(ValidIO(Vec( banks, UInt(word_bits.W))))
    val wmask = Input(Vec( banks, Bool()))

    val sram_rd_addr = Input(UInt(sram_bits.W))
    val sram_rd_en = Input(Bool())
    val sram_rd_data = Output(UInt(word_bits.W))
  })


  val mems = IndexedSeq.fill( banks){ Mem( (1<<addr_bits), UInt(word_bits.W))}

  io.sram_rd_data := DontCare
  when ( io.sram_rd_en) {
    for { b <- 0 until banks} {
      when ( (io.sram_rd_addr & (banks-1).U) === b.U) {
        io.sram_rd_data := mems(b).read( io.sram_rd_addr >> log2banks)
      }
    }
  }

  val wdata = Wire( Vec( banks, UInt(word_bits.W)))
  val waddr = Wire( Vec( banks, UInt(addr_bits.W)))
  val we = Wire( Vec( banks, Bool()))

  wdata := DontCare
  waddr := DontCare
  we := DontCare
  for { offset <- 0 until banks} {
    when ( (io.sram_wr_addr & (banks-1).U) === offset.U) {
      for { j <- 0 until banks} {
        val u = (offset + j) & (banks-1)
        wdata(u) := io.cacheline.bits(j);
        waddr(u) := (io.sram_wr_addr >> log2banks) + (if ( offset + j >= banks) 1 else 0).U
        we(u) := io.wmask(j)
      }
    }
  }

  when( io.cacheline.valid) {
    for { b <- 0 until banks} {
      when ( we(b)) {
        mems(b).write( waddr(b), wdata(b))
      }
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

class ScratchPad_Tester( tag: String, factory: () => ScratchPad) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    check(chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {
         poke( c.io.wmask(0), 1)
         poke( c.io.wmask(1), 1)
         poke( c.io.wmask(2), 1)
         poke( c.io.wmask(3), 1)

         poke( c.io.sram_rd_en, 0)


         for { offset <- 0 until c.banks} { 
           poke( c.io.sram_wr_addr, offset)

           poke( c.io.cacheline.valid, 1)
           for { i <- 0 until c.banks} {
             poke( c.io.cacheline.bits(i), i)
           }
           step(1)
           poke( c.io.cacheline.valid, 0)


           poke( c.io.sram_rd_en, 1)
           for { i <- 0 until c.banks} {
             poke( c.io.sram_rd_addr, i+offset)
             expect( c.io.sram_rd_data, i)
             step(1)
           }
           poke( c.io.sram_rd_en, 0)

         }

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
class ScratchPad_Test extends ScratchPad_Tester( "ScratchPad", () => new ScratchPad)

class MSTest extends MSTester( "MS", () => new MS)

