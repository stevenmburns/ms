package feature_vec_literal

import org.scalatest._

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.BundleLiterals._

class Test extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of s"bundle_literal"
  it should "compile and execute without expect violations" in {
    test( new Module {
      class meta_data extends Bundle {
        val data = UInt(8.W)
        val bundle = new Bundle {
          val a = UInt(8.W)
          val b = UInt(8.W)
        }
      }
      val io = IO(new Bundle {
        val inp0 = Input( new meta_data)
        val out0 = Output( new meta_data)
        val inp1 = Input( new meta_data)
        val out1 = Output( new meta_data)
      })
      io.inp0 <> io.out0
      io.out1 <> io.inp1
    }) { c => 
      def logical_step() {
        val x = c.io.out0.peek()
        println( s"${x}")
        c.io.inp1.poke( x)
        c.clock.step()
      }
      c.io.inp0.data.poke( 47.U)
      c.io.inp0.bundle.poke( chiselTypeOf(c.io.inp0.bundle).Lit(_.a -> 0.U, _.b -> 1.U))
      logical_step()
      c.io.out1.data.expect( 47.U)
      c.io.out1.bundle.expect( chiselTypeOf(c.io.inp0.bundle).Lit(_.a -> 0.U, _.b -> 1.U))
      c.io.inp0.data.poke( 48.U)
      c.io.inp0.bundle.poke( chiselTypeOf(c.io.inp0.bundle).Lit(_.a -> 1.U, _.b -> 2.U))
      logical_step()
      c.io.out1.data.expect( 48.U)
      c.io.out1.bundle.expect( chiselTypeOf(c.io.inp0.bundle).Lit(_.a -> 1.U, _.b -> 2.U))
    }
  }
}

/*
class TestFails extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of s"vec_literal"
  it should "compile and execute without expect violations" in {
    test( new Module {
      class meta_data extends Bundle {
        val data = UInt(8.W)
        val vector = Vec( 4, Bool())
      }
      val io = IO(new Bundle {
        val inp0 = Input( new meta_data)
        val out0 = Output( new meta_data)
        val inp1 = Input( new meta_data)
        val out1 = Output( new meta_data)
      })
      io.inp0 <> io.out0
      io.out1 <> io.inp1
    }) { c => 
      def logical_step() {
        val x = c.io.out0.peek() // Fails because peek on array fails
        c.io.inp1.poke( x)
        c.clock.step()
      }
      c.io.inp0.data.poke( 47.U)
      logical_step()
      c.io.out1.data.expect( 47.U)
      c.io.inp0.data.poke( 48.U)
      logical_step()
      c.io.out1.data.expect( 48.U)
    }
  }
}

class TestCopyFails extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of s"vec_literal_copy"
  it should "compile and execute without expect violations" in {
    test( new Module {
      class meta_data extends Bundle {
        val data = UInt(8.W)
        val vector = Vec( 4, Bool())
        def copy( that: meta_data) {
          that.data.poke( this.data.peek())
          for { i <- this.vector.indices} {
            that.vector(i).poke( this.vector(i).peek())
          }
        }
      }
      val io = IO(new Bundle {
        val inp0 = Input( new meta_data)
        val out0 = Output( new meta_data)
        val inp1 = Input( new meta_data)
        val out1 = Output( new meta_data)
      })
      io.inp0 <> io.out0
      io.out1 <> io.inp1
    }) { c => 
      def logical_step() {
        c.io.inp0.copy( c.io.inp1)
        c.clock.step()
      }
      c.io.inp0.data.poke( 47.U)
      logical_step()
      c.io.out1.data.expect( 47.U)
      c.io.inp0.data.poke( 48.U)
      logical_step()
      c.io.out1.data.expect( 48.U)
    }
  }
}
*/
