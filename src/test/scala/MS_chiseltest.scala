package ms

import org.scalatest._

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VcsBackendAnnotation, VerilatorBackendAnnotation}

class MS_chiseltest_Tester( tag: String, factory: () => MS) extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    test( factory()).withAnnotations(Seq(VerilatorBackendAnnotation)){ c => 

         case class MOx( val timeout: Int, val data: IndexedSeq[BigInt], val meta : meta_data_literal) {}

         var t = 0
         val rnd = new scala.util.Random(47L)

         val sram_to_observe = 128
         val scratch_pad_model = scala.collection.mutable.Map[BigInt,BigInt]()
         val pq = scala.collection.mutable.PriorityQueue.empty[MOx](Ordering.by((_: MOx).timeout).reverse)

         def logical_step() {
           c.io.ld_rsp.valid.poke( false.B)
           for { h <- pq.headOption} {
             println( s"head of pq: ${t} ${h} ${pq.size}")
             if ( h.timeout < t) {
/*
               println( s"c.io.ld_rsp.bits.meta.sram_wr_addr: ${c.io.ld_rsp.bits.meta.sram_wr_addr.peek()}")
               println( s"c.io.ld_rsp.bits.meta.wmask(0): ${c.io.ld_rsp.bits.meta.wmask(0).peek()}")
               println( s"c.io.ld_rsp.bits.meta.wmask: ${c.io.ld_rsp.bits.meta.wmask.peek()}")
               println( s"c.io.ld_rsp.bits.meta: ${c.io.ld_rsp.bits.meta.peek()}")
               println( s"c.io.ld_rsp.bits: ${c.io.ld_rsp.bits.peek()}")
               println( s"c.io.ld_rsp: ${c.io.ld_rsp.peek()}")
 */

               c.io.ld_rsp.valid.poke( true.B)
               for { b <- 0 until c.p.banks} {
                 c.io.ld_rsp.bits.data(b).poke( h.data(b).U)
                 c.io.ld_rsp.bits.meta.wmask(b).poke( (h.meta.wmask(b) != 0).B)
               }
               c.io.ld_rsp.bits.meta.sram_wr_addr.poke( h.meta.sram_wr_addr.U)
               pq.dequeue
             }
           }

           if ( c.io.ld_req.valid.peek().litValue != 0) {
             val dram_addr = c.io.ld_req.bits.addr.peek().litValue
             val d = IndexedSeq.tabulate(4){ i => (4*dram_addr+i)}
             val m = meta_data_literal(
               c.io.ld_req.bits.meta.sram_wr_addr.peek().litValue,
               IndexedSeq.tabulate(c.p.banks){ i => c.io.ld_req.bits.meta.wmask(i).peek().litValue},
               c.io.ld_req.bits.meta.port_id.peek().litValue
             )
             val lb = 10
             val ub = 30
             val dt = rnd.nextInt( ub-lb + 1) + lb

             pq += new MOx( t+dt, d, m)
           }
           c.clock.step(1)
           t += 1
         }

         c.io.start.poke(false.B)
         c.io.sram_rd.en.poke(false.B)
         c.io.ld_req.ready.poke(true.B)

         /* Get the scratchpad state */
         c.io.sram_rd.en.poke(true.B)
         for { idx <- 0 until sram_to_observe} {
            c.io.sram_rd.addr.poke(idx.U)
            scratch_pad_model(idx) = c.io.sram_rd.data.peek().litValue
            logical_step()
         }
         c.io.sram_rd.en.poke(false.B)

         def check_state() {
           c.io.sram_rd.en.poke(true.B)
           for { idx <- 0 until sram_to_observe} {
             c.io.sram_rd.addr.poke(idx.U)
             val expected = scratch_pad_model(idx)
             val actual = c.io.sram_rd.data.peek().litValue
             println( s"check_state: ${idx} ${expected} ${actual} ${expected == actual}")
             c.io.sram_rd.data.expect( scratch_pad_model(idx).U)
             logical_step()
           }
           c.io.sram_rd.en.poke(false.B)
         }
         check_state




         logical_step()
         c.io.done.expect( false.B)
         c.io.start.poke( true.B)
         logical_step()
         c.io.done.expect( false.B)
         c.io.start.poke( false.B)
         while ( t < 10000 && c.io.done.peek().litValue == false.B.litValue) {
           logical_step()
         }

         assert( pq.size == 0)

         for { i <- 0 until (c.ag.max_cnt << c.p.log2banks)} {
           scratch_pad_model(i) = i
         }
         check_state

    }
  }
}

class MS_chiseltest_Test extends MS_chiseltest_Tester( "MS_chiseltest_Test", () => new MS)
