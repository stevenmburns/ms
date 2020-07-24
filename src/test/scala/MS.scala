package ms

import org.scalatest.{ Matchers, FlatSpec, GivenWhenThen}
import org.scalatest.prop.Checkers

import chisel3._
import chisel3.util._
import chisel3.iotesters._

class MS_params {
  val cacheline_addr_bits = 32
  val log2banks = 2
  val banks = 1 << log2banks
  val BLOCKIN = 16
  val sram_bits = 18 // addresses in units of BLOCKIN bytes
  val addr_bits = sram_bits - log2banks
  val word_bits = BLOCKIN*8
}

object MS_params {
  def apply(): MS_params = new MS_params
}

class meta_data( val p: MS_params) extends Bundle {
  val sram_wr_addr = UInt(p.sram_bits.W)
  val wmask = Vec(p.banks,Bool())
}

class req_packet( val p: MS_params) extends Bundle {
  val addr = UInt(p.cacheline_addr_bits.W)
  val meta = new meta_data(p)
}

class rsp_packet( val p: MS_params) extends Bundle {
  val data = Vec(p.banks, UInt(p.word_bits.W))
  val meta = new meta_data(p)
}


class addr_gen( val p: MS_params = MS_params()) extends Module {
  val max_cnt = 32.U
  val io = IO(new Bundle {
    val start = Input( Bool())
    val done = Output( Bool())
    val ld_req = DecoupledIO( new req_packet(p))
  })
 
  val running = RegInit( init=false.B)
  when ( io.start) {
    running := true.B
  }

  val cnt = RegInit( io.ld_req.bits.addr.cloneType, init=0.U)

  io.done := false.B

  io.ld_req.noenq
  when ( (running || io.start) && io.ld_req.ready) {
    val pkt = Wire(new req_packet(p))
    pkt.addr := cnt
    pkt.meta.sram_wr_addr := cnt << p.log2banks
    pkt.meta.wmask := VecInit( IndexedSeq.fill( p.banks){ true.B})
    io.ld_req.enq( pkt)
    when ( cnt === max_cnt - 1.U) {
      running := false.B
      io.done := true.B
      cnt := 0.U
    } .otherwise {
      cnt := cnt + 1.U
    }
  }
}

class MS( val p: MS_params = MS_params()) extends Module {
  val io = IO(new Bundle {
    val start = Input( Bool())
    val done = Output( Bool())
    val ld_req = DecoupledIO( new req_packet( p))
    val ld_rsp = Input(ValidIO( new rsp_packet( p)))

    val sram_rd_addr = Input(UInt(p.sram_bits.W))
    val sram_rd_en = Input(Bool())
    val sram_rd_data = Output(UInt(p.word_bits.W))
  })

  val ag = Module( new addr_gen(p))
  val sp = Module( new ScratchPad(p))

  sp.io.sram_rd_addr := io.sram_rd_addr
  sp.io.sram_rd_en := io.sram_rd_en
  io.sram_rd_data := sp.io.sram_rd_data

  val sIdle #:: sRunning #:: sWaiting #:: _ = Stream.from(0).map {_.U(2.W)}

  val s = RegInit( init=sIdle)

  val inflight = RegInit( init=0.U(8.W))
  when ( !io.ld_req.fire() && io.ld_rsp.valid) {
    inflight := inflight - 1.U
  } .elsewhen( io.ld_req.fire() && !io.ld_rsp.valid) {
    inflight := inflight + 1.U
  }

  ag.io.start := io.start
  io.ld_req <> ag.io.ld_req
  io.ld_rsp <> sp.io.resp

  io.done := false.B
  when        ( s === sIdle    && io.start) {
    s := sRunning
  } .elsewhen ( s === sRunning && ag.io.done) {
    s := sWaiting
  } .elsewhen ( s === sWaiting && inflight === 0.U) {
    io.done := true.B
    s := sIdle
  }

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

case class MO( val timeout: Int, val data: IndexedSeq[BigInt], val sram_wr_addr: BigInt, val wmask: IndexedSeq[Boolean])

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
                 poke( c.io.ld_rsp.bits.meta.wmask(b), if ( h.wmask(b)) 1 else 0)
                 if ( h.wmask(b)) {
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
             val m = IndexedSeq.tabulate(4){ i => peek( c.io.ld_req.bits.meta.wmask(i)) != 0}
             val lb = 10
             val ub = 30
             val dt = rnd.nextInt( ub-lb + 1) + lb

             pq += new MO( t+dt, d, a, m)
           }
           step(1)
         }

         poke( c.io.start, 0)
         poke( c.io.sram_rd_en, 0)
         poke( c.io.ld_req.ready, 1)

         /* Get the scratchpad state */
         poke( c.io.sram_rd_en, 1)
         for { idx <- 0 until sram_to_observe} {
            poke( c.io.sram_rd_addr, idx)
            scratch_pad_model(idx) = peek( c.io.sram_rd_data)
            logical_step()
         }
         poke( c.io.sram_rd_en, 0)

         def check_state() {
           poke( c.io.sram_rd_en, 1)
           for { idx <- 0 until sram_to_observe} {
             poke( c.io.sram_rd_addr, idx)
             expect( c.io.sram_rd_data, scratch_pad_model(idx))
             logical_step()
           }
           poke( c.io.sram_rd_en, 0)
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

class addr_gen_Test extends addr_gen_Tester( "addr_gen", () => new addr_gen)

class MS_Test extends MS_Tester( "MS", () => new MS)

