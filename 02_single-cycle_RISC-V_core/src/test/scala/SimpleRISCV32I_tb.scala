// ADS I Class Project
// Single-Cycle RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 05/10/2023 by Tobias Jauch (@tojauch)

package SimpleRV32I_Tester

import chisel3._
import chiseltest._
import SimpleRV32I._
import org.scalatest.flatspec.AnyFlatSpec

class SimpleRISCV32ITest extends AnyFlatSpec with ChiselScalatestTester {

"SimpleRV32I_Tester" should "work" in {
    test(new SimpleRV32I("src/test/programs/BinaryFile")).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(0)
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      
      dut.clock.step(1)
      dut.io.result.expect(4.U)     // ADDI x1, x0, 4

      dut.clock.step(1)
      dut.io.result.expect(5.U)     // ADDI x2, x0, 5

      dut.clock.step(1)
      dut.io.result.expect(9.U)     // ADD x3, x1, x2
      
        /* 
         * TODO: Add testcases for all R-type instructions in 'BinaryFile' and check the expected results here
         */

      dut.clock.step(1)
      dut.io.result.expect(1.U)            // SLT x3, x1, x2  -> signed comparison, writing 1 to rd if rs1 < rs2, 0 otherwise
       
      dut.clock.step(1)       
      dut.io.result.expect(1.U)            // SLTU x3, x1, x2 -> unsigned comparison, writing 1 to rd if rs1 < rs2, 0 otherwise (note:rd, x0, rs2 sets rd to 1 if rs2 != 0, otherwise sets rd to 0)
       
      dut.clock.step(1)       
      dut.io.result.expect(4.U)            // AND  x3, x1, x2 -> bitwise AND operation
       
      dut.clock.step(1)       
      dut.io.result.expect(5.U)            // OR   x3, x1, x2 -> bitwise OR operation
       
      dut.clock.step(1)       
      dut.io.result.expect(1.U)            // XOR  x3, x1, x2 -> bitwise XOR operation
       
      dut.clock.step(1)       
      dut.io.result.expect(128.U)          // SLL  x3, x1, x2 -> Logical Left on the value in register rs1 by the shift amount held in the lower 5 bits of register rs2
       
      dut.clock.step(1)       
      dut.io.result.expect(0.U)            // SRL  x3, x1, x2 -> Logical Right on the value in register rs1 by the shift amount held in the lower 5 bits of register rs2
 
      dut.clock.step(1) 
      dut.io.result.expect("hFFFFFFFF".U)  // SUB  x3, x1, x2 -> expected result: -1

      dut.clock.step(1)       
      dut.io.result.expect(0.U)            // SRA  x3, x1, x2 -> Arithmetic Right Shifts on the value in register rs1 by the shift amount held in the lower 5 bits of register rs2

      dut.clock.step(1)
    }
  }
}


