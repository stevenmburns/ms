package ms

import org.scalatest.{ Matchers, FlatSpec, GivenWhenThen}
import org.scalatest.prop.Checkers

import chisel3._
import chisel3.util._
import chisel3.iotesters._

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
