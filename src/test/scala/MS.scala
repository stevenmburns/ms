package ms

import org.scalatest.{ Matchers, FlatSpec, GivenWhenThen}
import org.scalatest.prop.Checkers

import chisel3._
import chisel3.util._
import chisel3.iotesters._

class addr_gen_Tester( tag: String, factory: () => addr_gen) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    check(chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {
         var addr = 0
         var rdy = false

         poke( c.io.start, 0)
         poke( c.io.ld_req.ready, false)
         step(1)

         poke( c.io.start, 1)


         while ( peek(c.io.done) == 0 && t < 1000) {
           rdy = rdy || (rnd.nextInt( 8) < 7)
           poke( c.io.ld_req.ready, rdy)

           if ( rdy && peek( c.io.ld_req.valid) == 1) {
             rdy = false
             expect( c.io.ld_req.bits.addr, addr)
             addr += 1
           }
           step(1)
           poke( c.io.start, 0)
         }
       }
    })
  }
}

class CountGenerator_Tester( tag: String, factory: () => CountGenerator) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    check(chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {
         var count = 0
         var rdy = false

         while ( count < c.tags && t < 1000) {
           rdy = rdy || (rnd.nextInt( 8) < 7)
           poke( c.io.count_out.ready, rdy)
           if ( rdy) {
             expect( c.io.count_out.valid, 1)
             rdy = false
             expect( c.io.count_out.bits, count)
             count += 1
           }
           step(1)
         }
         poke( c.io.count_out.ready, 1)
         expect( c.io.count_out.valid, 0)
         step(1)
       }
    })
  }
}

class TagGenerator_Tester( tag: String, factory: () => TagGeneratorIfc) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    check(chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {

         val available_tags = scala.collection.mutable.Set.empty[BigInt]
         for { t <- 0 until c.tags} {
           available_tags += t
         }

         case class Pair( val timeout : Long, val tag_value : BigInt) {}
         val pq = scala.collection.mutable.PriorityQueue.empty[Pair](Ordering.by((_: Pair).timeout).reverse)

         var rdy = false
         var used_tags = 0

         def logical_step() {
           println( s"${used_tags} ${available_tags} ${pq}")
           poke( c.io.tag_inp.valid, 0)
           for { h <- pq.headOption} {
             println( s"${h} ${t}")
             if ( h.timeout < t) {
               poke( c.io.tag_inp.valid, 1)
               poke( c.io.tag_inp.bits, h.tag_value)
               expect( !(available_tags contains h.tag_value), s"${h.tag_value} should not be in available_tags")
               available_tags += h.tag_value
               used_tags -= 1
               pq.dequeue
             }
           }

           if ( rdy && peek( c.io.tag_out.valid) == 1) {
             rdy = false
             val lb = 48
             val ub = 96
             val dt = rnd.nextInt( ub-lb+1) + lb
             val tag_value = peek( c.io.tag_out.bits)
             println( s"Sending ${tag_value}")
             pq += Pair(t+dt, tag_value)
             expect( available_tags contains tag_value, s"${tag_value} in available_tags")
             available_tags -= tag_value
             used_tags += 1
           }
           step(1)
         }

         while ( t < 1000) {
           rdy = rdy || (used_tags < c.tags && (rnd.nextInt( 8) < 7))
           poke( c.io.tag_out.ready, rdy)
           logical_step()
         }

       }
    })
  }
}

class MetadataCompress_Tester( tag: String, factory: () => MetadataCompress) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    check(chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {
         poke( c.io.ld_req_inp.valid, 0)
         poke( c.io.ld_rsp_inp.valid, 0)

         poke( c.io.ld_req_out.ready, 1)

         expect( c.io.ld_req_inp.ready, 1)
         step(1)
         poke( c.io.ld_req_inp.valid, 1)
         poke( c.io.ld_req_inp.bits.addr, 0)
         poke( c.io.ld_req_inp.bits.meta.sram_wr_addr, 0)
         poke( c.io.ld_req_inp.bits.meta.wmask(0), 1)
         poke( c.io.ld_req_inp.bits.meta.wmask(1), 1)
         poke( c.io.ld_req_inp.bits.meta.wmask(2), 1)
         poke( c.io.ld_req_inp.bits.meta.wmask(3), 1)

         expect( c.io.ld_req_out.valid, 1)
         expect( c.io.ld_req_out.bits.addr, 0)
         expect( c.io.ld_req_out.bits.meta.tag, 0)
         step(1)
         poke( c.io.ld_req_inp.valid, 1)
         poke( c.io.ld_req_inp.bits.addr, 0)
         poke( c.io.ld_req_inp.bits.meta.sram_wr_addr, 0)
         poke( c.io.ld_req_inp.bits.meta.wmask, IndexedSeq.fill(4){ BigInt(1)})

         expect( c.io.ld_req_out.valid, 1)
         expect( c.io.ld_req_out.bits.addr, 0)
         expect( c.io.ld_req_out.bits.meta.tag, 1)
         step(1)
         poke( c.io.ld_req_inp.valid, 0)

         poke( c.io.ld_rsp_inp.valid, 1)
         poke( c.io.ld_rsp_inp.bits.data, IndexedSeq.fill(4){BigInt(0)})
         poke( c.io.ld_rsp_inp.bits.meta.tag, 1)

         expect( c.io.ld_rsp_out.valid, 0)
         step(1)
         expect( c.io.ld_rsp_out.valid, 1)
         expect( c.io.ld_rsp_out.bits.data, IndexedSeq.fill(4){BigInt(0)})
         expect( c.io.ld_rsp_out.bits.meta.sram_wr_addr, 0)
         expect( c.io.ld_rsp_out.bits.meta.wmask, IndexedSeq.fill(4){ BigInt(1)})
       }
    })
  }
}

case class MO( val timeout: Int, val data: IndexedSeq[BigInt], val sram_wr_addr: BigInt, val wmask: Seq[BigInt])

class MS_Tester( tag: String, factory: () => MS) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    check(chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {
         val sram_to_observe = 128

         val scratch_pad_model = scala.collection.mutable.Map[BigInt,BigInt]()

         val pq = scala.collection.mutable.PriorityQueue.empty[MO](Ordering.by((_: MO).timeout).reverse)

         def logical_step() {
           poke( c.io.ld_rsp.valid, 0)
           for { h <- pq.headOption} {
             println( s"${h} ${pq} ${t}")
             if ( h.timeout < t) {
               poke( c.io.ld_rsp.valid, 1)
               for { b <- 0 until c.p.banks} {
                 poke( c.io.ld_rsp.bits.data(b), h.data(b))
                 poke( c.io.ld_rsp.bits.meta.wmask(b), h.wmask(b))

                 if ( h.wmask(b) != 0) {
                   scratch_pad_model(h.sram_wr_addr + b) = h.data(b)
                 }
               }
               poke( c.io.ld_rsp.bits.meta.sram_wr_addr, h.sram_wr_addr)

               pq.dequeue
             }
           }

           if ( peek( c.io.ld_req.valid) == 1) {
             val dram_addr = peek( c.io.ld_req.bits.addr)
             val d = IndexedSeq.tabulate(4){ i => 4*dram_addr+i}
             val a = peek( c.io.ld_req.bits.meta.sram_wr_addr)

             val m = IndexedSeq.tabulate(4){ i => peek( c.io.ld_req.bits.meta.wmask(i)) }
             val lb = 10
             val ub = 30
             val dt = rnd.nextInt( ub-lb + 1) + lb

             pq += new MO( t+dt, d, a, m)
           }
           step(1)
         }

         poke( c.io.start, 0)
         poke( c.io.sram_rd.en, 0)
         poke( c.io.ld_req.ready, 1)

         /* Get the scratchpad state */
         poke( c.io.sram_rd.en, 1)
         for { idx <- 0 until sram_to_observe} {
            poke( c.io.sram_rd.addr, idx)
            scratch_pad_model(idx) = peek( c.io.sram_rd.data)
            logical_step()
         }
         poke( c.io.sram_rd.en, 0)

         def check_state() {
           poke( c.io.sram_rd.en, 1)
           for { idx <- 0 until sram_to_observe} {
             poke( c.io.sram_rd.addr, idx)
             expect( c.io.sram_rd.data, scratch_pad_model(idx))
             logical_step()
           }
           poke( c.io.sram_rd.en, 0)
         }

         logical_step()
         expect( c.io.done, 0)
         poke( c.io.start, 1)
         logical_step()
         expect( c.io.done, 0)
         poke( c.io.start, 0)
         while ( t < 1000 && peek( c.io.done) == 0) {
           logical_step()
         }
         check_state
       }
    })
  }
}

class MS_Tester2( tag: String, factory: () => MS) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    check(chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {
         val sram_to_observe = 128

         val scratch_pad_model = scala.collection.mutable.Map[BigInt,BigInt]()

         val pq = scala.collection.mutable.PriorityQueue.empty[MO](Ordering.by((_: MO).timeout).reverse)

         def logical_step() {
           poke( c.io.ld_rsp.valid, 0)
           for { h <- pq.headOption} {
             println( s"${h} ${pq} ${t}")
             if ( h.timeout < t) {
               poke( c.io.ld_rsp.valid, 1)
               poke( c.io.ld_rsp.bits.meta.sram_wr_addr, h.sram_wr_addr)
               for { b <- 0 until c.p.banks} {
                 poke( c.io.ld_rsp.bits.meta.wmask(b), h.wmask(b))
                 poke( c.io.ld_rsp.bits.data(b), h.data(b))

               }
               pq.dequeue
             }
           }

           if ( peek( c.io.ld_req.valid) == 1) {
             val dram_addr = peek( c.io.ld_req.bits.addr)
             val d = IndexedSeq.tabulate(4){ i => 4*dram_addr+i}
             val sram_wr_addr = peek( c.io.ld_req.bits.meta.sram_wr_addr)
             val wmask = Seq.tabulate(4){ i => peek( c.io.ld_req.bits.meta.wmask(i)) }
             val lb = 10
             val ub = 30
             val dt = rnd.nextInt( ub-lb + 1) + lb

             pq += new MO( t+dt, d, sram_wr_addr, wmask)
           }
           step(1)
         }

         poke( c.io.start, 0)
         poke( c.io.sram_rd.en, 0)
         poke( c.io.ld_req.ready, 1)

         /* Get the scratchpad state */
         poke( c.io.sram_rd.en, 1)
         for { idx <- 0 until sram_to_observe} {
            poke( c.io.sram_rd.addr, idx)
            scratch_pad_model(idx) = peek( c.io.sram_rd.data)
            logical_step()
         }
         poke( c.io.sram_rd.en, 0)

         def check_state() {
           poke( c.io.sram_rd.en, 1)
           for { idx <- 0 until sram_to_observe} {
             poke( c.io.sram_rd.addr, idx)
             expect( c.io.sram_rd.data, scratch_pad_model(idx))
             logical_step()
           }
           poke( c.io.sram_rd.en, 0)
         }
         check_state

         logical_step()
         expect( c.io.done, 0)
         poke( c.io.start, 1)
         logical_step()
         expect( c.io.done, 0)
         poke( c.io.start, 0)
         while ( t < 1000 && peek( c.io.done) == 0) {
           logical_step()
         }

         for { i <- 0 until (c.ag.max_cnt << c.p.log2banks)} {
           scratch_pad_model(i) = i
         }
         check_state
       }
    })
  }
}


case class MO_small( val timeout: Int, val data: IndexedSeq[BigInt], val meta: BigInt)

class MS_small_Tester( tag: String, factory: () => MS_small) extends GenericTest {
  behavior of s"$tag"
  it should "compile and execute without expect violations" in {
    check(chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
       new PeekPokeTester(c) {
         val sram_to_observe = 128

         val scratch_pad_model = scala.collection.mutable.Map[BigInt,BigInt]()

         val pq = scala.collection.mutable.PriorityQueue.empty[MO_small](Ordering.by((_: MO_small).timeout).reverse)

         def logical_step() {
           poke( c.io.ld_rsp.valid, 0)
           for { h <- pq.headOption} {
             println( s"${h} ${pq} ${t}")
             if ( h.timeout < t) {
               poke( c.io.ld_rsp.valid, 1)
               poke( c.io.ld_rsp.bits.meta.asUInt, h.meta)
               for { b <- 0 until c.p.banks} {
                 poke( c.io.ld_rsp.bits.data(b), h.data(b))

               }
               pq.dequeue
             }
           }

           if ( peek( c.io.ld_req.valid) == 1) {
             val dram_addr = peek( c.io.ld_req.bits.addr)
             val d = IndexedSeq.tabulate(4){ i => 4*dram_addr+i}
             val meta = peek( c.io.ld_req.bits.meta.asUInt)
             val lb = 10
             val ub = 30
             val dt = rnd.nextInt( ub-lb + 1) + lb

             pq += new MO_small( t+dt, d, meta)
           }
           step(1)
         }

         poke( c.io.start, 0)
         poke( c.io.sram_rd.en, 0)
         poke( c.io.ld_req.ready, 1)

         /* Get the scratchpad state */
         poke( c.io.sram_rd.en, 1)
         for { idx <- 0 until sram_to_observe} {
            poke( c.io.sram_rd.addr, idx)
            scratch_pad_model(idx) = peek( c.io.sram_rd.data)
            logical_step()
         }
         poke( c.io.sram_rd.en, 0)

         def check_state() {
           poke( c.io.sram_rd.en, 1)
           for { idx <- 0 until sram_to_observe} {
             poke( c.io.sram_rd.addr, idx)
             expect( c.io.sram_rd.data, scratch_pad_model(idx))
             logical_step()
           }
           poke( c.io.sram_rd.en, 0)
         }
         check_state

         logical_step()
         expect( c.io.done, 0)
         poke( c.io.start, 1)
         logical_step()
         expect( c.io.done, 0)
         poke( c.io.start, 0)
         while ( t < 1000 && peek( c.io.done) == 0) {
           logical_step()
         }

         for { i <- 0 until (c.ag.max_cnt << c.p.log2banks)} {
           scratch_pad_model(i) = i
         }
         check_state
       }
    })
  }
}

class addr_gen_Test extends addr_gen_Tester( "addr_gen", () => new addr_gen)

class CountGenerator_Test extends CountGenerator_Tester( "CountGenerator", () => new CountGenerator)

class TagGenerator_Test extends TagGenerator_Tester( "TagGenerator", () => new TagGenerator)
class TagGenerator2_Test extends TagGenerator_Tester( "TagGenerator2", () => new TagGenerator2)
class MetadataCompress_Test extends MetadataCompress_Tester( "MetadataCompress", () => new MetadataCompress)

class MS_Test extends MS_Tester( "MS", () => new MS)
class MS_Test2 extends MS_Tester2( "MS_Test2", () => new MS)
class MS_small_Test extends MS_small_Tester( "MS_small", () => new MS_small)
