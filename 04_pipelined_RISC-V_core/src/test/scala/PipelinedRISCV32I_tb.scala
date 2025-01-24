// ADS I Class Project
// Pipelined RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 12/19/2023 by Tobias Jauch (@tojauch)

package PipelinedRV32I_Tester

import chisel3._
import chiseltest._
import PipelinedRV32I._
import org.scalatest.flatspec.AnyFlatSpec

class PipelinedRISCV32ITest extends AnyFlatSpec with ChiselScalatestTester {

"PipelinedRV32I_Tester" should "work" in {
    test(new PipelinedRV32I("src/test/programs/BinaryFile")).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

        /* 
         * TODO: Insert your testcases from the previous assignments and adapt them for the pipelined core
         */

         /* SETUP Environment */

      dut.clock.setTimeout(0)

      dut.clock.step(5) 
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      
      dut.clock.step(1)             // Wait for 5 cycles (end of NOP)
      dut.io.result.expect(4.U)     // ADDI x1, x0, 4
      
      dut.clock.step(1)             // Wait for 5 cycles: in this case we have a 15 multicycle logic with 5 stage, hence we need to wait that all the operations are done
      dut.io.result.expect(5.U)     // ADDI x2, x0, 5
      
      /* Arithmetic R-Type operations */

      dut.clock.step(4)                   // we need to stall for 3 clock cycles, necessary to get x2 and x3 values
      dut.io.result.expect(9.U)           // ADD x3, x1, x2
      
      dut.clock.step(1)      
      dut.io.result.expect(1.U)           // SLT x4, x1, x2  -> signed comparison, writing 1 to rd if rs1 < rs2, 0 otherwise
            
      dut.clock.step(1)      
      dut.io.result.expect(1.U)           // SLTU x5, x1, x2 -> unsigned comparison, writing 1 to rd if rs1 < rs2, 0 otherwise (note:rd, x0, rs2 sets rd to 1 if rs2 != 0, otherwise sets rd to 0)
            
      dut.clock.step(1)      
      dut.io.result.expect(4.U)           // AND  x6, x1, x2 -> bitwise AND operation
            
      dut.clock.step(1)      
      dut.io.result.expect(5.U)           // OR   x7, x1, x2 -> bitwise OR operation
            
      dut.clock.step(1)      
      dut.io.result.expect(1.U)           // XOR  x8, x1, x2 -> bitwise XOR operation
            
      dut.clock.step(1)      
      dut.io.result.expect(128.U)         // SLL  x9, x1, x2 -> Logical Left on the value in register rs1 by the shift amount held in the lower 5 bits of register rs2
            
      dut.clock.step(1)      
      dut.io.result.expect(0.U)           // SRL  x10, x1, x2 -> Logical Right on the value in register rs1 by the shift amount held in the lower 5 bits of register rs2
            
      dut.clock.step(1)      
      dut.io.result.expect("hFFFFFFFF".U) // SUB  x11, x1, x2 -> expected result: -1
      
      dut.clock.step(1)
      dut.io.result.expect(0.U)           // SRA  x12, x1, x2 -> Arithmetic Right Shifts on the value in register rs1 by the shift amount held in the lower 5 bits of register rs2

      dut.clock.step(1)
           
    }
  }
}