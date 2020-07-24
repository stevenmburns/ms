package ms

import org.scalatest.{ Matchers, FlatSpec, GivenWhenThen}
import org.scalatest.prop.Checkers

import chisel3._
import chisel3.util._
import chisel3.iotesters._

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

class ScratchPad_Tester( tag: String, factory: () => ScratchPad) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    check(chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {

         for { wmask <- 0 until (1 << c.banks)} {
           val scratch_pad_model = scala.collection.mutable.Map[Int,BigInt]()

           for { b <- 0 until c.banks} {
             poke( c.io.wmask(b), if ( 0 != (wmask & (1<<b))) 1 else 0)
           }
           poke( c.io.cacheline.valid, 0)
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

           for { offset <- 0 until c.banks} {
             poke( c.io.sram_wr_addr, offset)

             poke( c.io.cacheline.valid, 1)
             for { b <- 0 until c.banks} {
               poke( c.io.cacheline.bits(b), b)
               if ( 0 != (wmask & (1<<b))) {
                 scratch_pad_model( offset + b) = b
               }
             }
             step(1)
             poke( c.io.cacheline.valid, 0)

             check_state()
           }
         }
       }
    })
  }
}

class ScratchPad_Test extends ScratchPad_Tester( "ScratchPad", () => new ScratchPad)
