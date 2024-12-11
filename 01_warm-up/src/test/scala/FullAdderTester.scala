// ADS I Class Project
// Chisel Introduction
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 18/10/2022 by Tobias Jauch (@tojauch)

package adder

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


/** 
  * Full adder tester
  * Use the truth table from the exercise sheet to test all possible input combinations and the corresponding results exhaustively
  */
class FullAdderTester extends AnyFlatSpec with ChiselScalatestTester {

  "FullAdder" should "work" in {
    test(new FullAdder).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

          /*dut.io.a.poke(...)
           *dut.io.b.poke(...)
           *dut.io.ci.poke(...)
           *dut.io.s.expect(...)
           *dut.io.co.expect(...)
           *...
           *TODO: Insert your test cases
           */

          dut.io.A.poke(0.U)
          dut.io.B.poke(0.U)
          dut.io.Cin.poke(0.U)
          dut.io.S.expect(0.U)
          dut.io.Cout.expect(0.U)

          dut.io.A.poke(0.U)
          dut.io.B.poke(0.U)
          dut.io.Cin.poke(1.U)
          dut.io.S.expect(1.U)
          dut.io.Cout.expect(0.U)
          
          dut.io.A.poke(0.U)
          dut.io.B.poke(1.U)
          dut.io.Cin.poke(0.U)
          dut.io.S.expect(1.U)
          dut.io.Cout.expect(0.U)
          
          dut.io.A.poke(0.U)
          dut.io.B.poke(1.U)
          dut.io.Cin.poke(1.U)
          dut.io.S.expect(0.U)
          dut.io.Cout.expect(1.U)

          dut.io.A.poke(1.U)
          dut.io.B.poke(0.U)
          dut.io.Cin.poke(0.U)
          dut.io.S.expect(1.U)
          dut.io.Cout.expect(0.U)

          dut.io.A.poke(1.U)
          dut.io.B.poke(0.U)
          dut.io.Cin.poke(1.U)
          dut.io.S.expect(0.U)
          dut.io.Cout.expect(1.U)

          dut.io.A.poke(1.U)
          dut.io.B.poke(1.U)
          dut.io.Cin.poke(0.U)
          dut.io.S.expect(0.U)
          dut.io.Cout.expect(1.U)

          dut.io.A.poke(1.U)
          dut.io.B.poke(1.U)
          dut.io.Cin.poke(1.U)
          dut.io.S.expect(1.U)
          dut.io.Cout.expect(1.U)

        }
    } 
}

