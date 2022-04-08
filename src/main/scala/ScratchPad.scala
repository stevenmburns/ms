package ms

import chisel3._
import chisel3.util._

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
        wdata(u) := io.resp.bits.data(j)
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
