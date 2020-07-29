package ms

import chisel3._
import chisel3.util._

class MS_params {
  val cacheline_addr_bits = 32
  val log2banks = 2
  val banks = 1 << log2banks
  val BLOCKIN = 16
  val sram_bits = 18 // addresses in units of BLOCKIN bytes
  val addr_bits = sram_bits - log2banks
  val word_bits = BLOCKIN*8
  val tags = 32
  val tag_bits = log2Ceil(tags)
  require( tag_bits == 5)
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

class req_packet_small( val p: MS_params) extends Bundle {
  val addr = UInt(p.cacheline_addr_bits.W)
  val tag = UInt(p.tag_bits.W)
}

class rsp_packet( val p: MS_params) extends Bundle {
  val data = Vec(p.banks, UInt(p.word_bits.W))
  val meta = new meta_data(p)
}

class rsp_packet_small( val p: MS_params) extends Bundle {
  val data = Vec(p.banks, UInt(p.word_bits.W))
  val tag = UInt(p.tag_bits.W)
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
  when ( running || io.start) {
    val pkt = Wire(new req_packet(p))
    pkt.addr := cnt
    pkt.meta.sram_wr_addr := cnt << p.log2banks
    pkt.meta.wmask := VecInit( IndexedSeq.fill( p.banks){ true.B})
    io.ld_req.enq( pkt) // helpful interface
    when ( io.ld_req.ready) {
      when ( cnt === max_cnt - 1.U) {
        running := false.B
        io.done := true.B
        cnt := 0.U
      } .otherwise {
        cnt := cnt + 1.U
      }
    }
  }
}

class CountGenerator( val p: MS_params = MS_params()) extends Module {
  val tags = 32
  val tag_bits = 5
  val io = IO(new Bundle {
    val count_out = DecoupledIO( UInt(tag_bits.W))
  })

  val done = RegInit( init=false.B)

  val count = RegInit( init=0.U(tag_bits.W))

  io.count_out.noenq
  when( !done) {
    io.count_out.enq( count)
    when( io.count_out.ready) {
      when ( count === (tags-1).U) {
        count := 0.U
        done := true.B
      } .otherwise {
        count := count + 1.U
      }
    }
  }
}

object CountGeneratorVerilog {
  def main(args: Array[String]): Unit = {
    chisel3.Driver.execute(args, () => new CountGenerator)
  }
}

class TagGeneratorIfc( val p: MS_params = MS_params()) extends Module {
  val tags = p.tags
  val tag_bits = p.tag_bits
  val io = IO(new Bundle {
    val tag_inp = Input(ValidIO( UInt(tag_bits.W)))
    val tag_out = DecoupledIO( UInt(tag_bits.W))
  })
}

object toDecoupledIO {
  def apply[T <: Data]( vio: ValidIO[T]): DecoupledIO[T] = {
    val decoupled = Wire( DecoupledIO(vio.bits.cloneType))
    decoupled.valid := vio.valid
    decoupled.bits := vio.bits
    assert( ~vio.valid || decoupled.ready)
    decoupled
  }
}

class TagGenerator( p: MS_params = MS_params()) extends TagGeneratorIfc(p) {
  val cg = Module(new CountGenerator(p))
  val arb = Module( new RRArbiter( UInt(), 2))
  arb.io.in(0) <> Queue( toDecoupledIO(io.tag_inp), tags, pipe=false, flow=false)
  arb.io.in(1) <> cg.io.count_out
  io.tag_out <> arb.io.out
}

class TagGenerator2( p: MS_params = MS_params()) extends TagGeneratorIfc(p) {
  val bv = RegInit( init=VecInit( IndexedSeq.fill(tags){ true.B}))
  io.tag_out.noenq
  when ( bv.asUInt.orR) {
    io.tag_out.enq( PriorityEncoder( bv))
    when ( io.tag_out.ready) {
      bv(io.tag_out.bits) := false.B
    }
  }
  when ( io.tag_inp.valid) {
    bv(io.tag_inp.bits) := true.B
  }
}

class MetadataCompress( val p : MS_params = MS_params()) extends Module {
  val io = IO(new Bundle {
    val ld_req_inp = Flipped(DecoupledIO( new req_packet(p)))
    val ld_req_out = DecoupledIO( new req_packet_small(p))

    val ld_rsp_inp = Input(ValidIO( new rsp_packet_small(p)))
    val ld_rsp_out = Output(ValidIO( new rsp_packet(p)))
  })

  val tg = Module( new TagGenerator2(p))

  val m = SyncReadMem( p.tags, new meta_data(p))

  io.ld_req_inp.nodeq
  io.ld_req_out.noenq

  tg.io.tag_out.nodeq
  when ( tg.io.tag_out.valid && io.ld_req_inp.valid && io.ld_req_out.ready) {
    val tag = tg.io.tag_out.deq()
    val req = io.ld_req_inp.deq()
    io.ld_req_out.valid := true.B
    io.ld_req_out.bits.addr := req.addr
    io.ld_req_out.bits.tag := tag
    m.write( tag, req.meta)
  }

  tg.io.tag_inp.valid := io.ld_rsp_inp.valid
  tg.io.tag_inp.bits := io.ld_rsp_inp.bits.tag
  io.ld_rsp_out.valid := RegNext( io.ld_rsp_inp.valid, init=false.B)
  io.ld_rsp_out.bits.data := RegNext( io.ld_rsp_inp.bits.data)
  io.ld_rsp_out.bits.meta := m.read(io.ld_rsp_inp.bits.tag, io.ld_rsp_inp.valid)
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


