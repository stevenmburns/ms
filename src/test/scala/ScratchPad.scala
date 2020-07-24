package ms

import org.scalatest.{ Matchers, FlatSpec, GivenWhenThen}
import org.scalatest.prop.Checkers

import chisel3._
import chisel3.util._
import chisel3.iotesters._

class ScratchPad( val p: MS_params = MS_params()) extends Module {
  val io = IO(new Bundle {
    val resp = Input(ValidIO(new rsp_packet(p)))
    val sram_rd_addr = Input(UInt(p.sram_bits.W))
    val sram_rd_en = Input(Bool())
    val sram_rd_data = Output(UInt(p.word_bits.W))
  })

  val mems = IndexedSeq.fill( p.banks){ Mem( (1<<p.addr_bits), UInt(p.word_bits.W))}

  io.sram_rd_data := DontCare
  when ( io.sram_rd_en) {
    for { b <- 0 until p.banks} {
      when ( (io.sram_rd_addr & (p.banks-1).U) === b.U) {
        io.sram_rd_data := mems(b).read( io.sram_rd_addr >> p.log2banks)
      }
    }
  }

  val wdata = Wire( Vec( p.banks, UInt(p.word_bits.W)))
  val waddr = Wire( Vec( p.banks, UInt(p.addr_bits.W)))
  val we = Wire( Vec( p.banks, Bool()))

  wdata := DontCare
  waddr := DontCare
  we := DontCare
  for { offset <- 0 until p.banks} {
    when ( (io.resp.bits.meta.sram_wr_addr & (p.banks-1).U) === offset.U) {
      for { j <- 0 until p.banks} {
        val u = (offset + j) & (p.banks-1)
        wdata(u) := io.resp.bits.data(j);
        waddr(u) := (io.resp.bits.meta.sram_wr_addr >> p.log2banks) + (if ( offset + j >= p.banks) 1 else 0).U
        we(u) := io.resp.bits.meta.wmask(j)
      }
    }
  }

  when( io.resp.valid) {
    for { b <- 0 until p.banks} {
      when ( we(b)) {
        mems(b).write( waddr(b), wdata(b))
      }
    }
  }
}

class ScratchPad_Tester( tag: String, factory: () => ScratchPad) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    check(chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {

         for { wmask <- 0 until (1 << c.p.banks)} {
           val scratch_pad_model = scala.collection.mutable.Map[Int,BigInt]()

           for { b <- 0 until c.p.banks} {
             poke( c.io.resp.bits.meta.wmask(b), if ( 0 != (wmask & (1<<b))) 1 else 0)
           }
           poke( c.io.resp.valid, 0)
           poke( c.io.sram_rd_en, 0)

           /* Get the scratchpad state */
           poke( c.io.sram_rd_en, 1)
           for { idx <- 0 until 32} {
             poke( c.io.sram_rd_addr, idx)
             scratch_pad_model(idx) = peek( c.io.sram_rd_data)
             step(1)
           }
           poke( c.io.sram_rd_en, 0)

           def check_state() {
             poke( c.io.sram_rd_en, 1)
             for { idx <- 0 until 32} {
               poke( c.io.sram_rd_addr, idx)
               expect( c.io.sram_rd_data, scratch_pad_model(idx))
               step(1)
             }
             poke( c.io.sram_rd_en, 0)
           }

           check_state()

           for { offset <- 0 until c.p.banks} {
             poke( c.io.resp.bits.meta.sram_wr_addr, offset)

             poke( c.io.resp.valid, 1)
             for { b <- 0 until c.p.banks} {
               poke( c.io.resp.bits.data(b), b)
               if ( 0 != (wmask & (1<<b))) {
                 scratch_pad_model( offset + b) = b
               }
             }
             step(1)
             poke( c.io.resp.valid, 0)

             check_state()
           }
         }
       }
    })
  }
}

class ScratchPad_Test extends ScratchPad_Tester( "ScratchPad", () => new ScratchPad)
