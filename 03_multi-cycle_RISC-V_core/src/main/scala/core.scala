// ADS I Class Project
// Multi-Cycle RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 05/10/2023 by Tobias Jauch (@tojauch)

/*
The goal of this task is to implement a 5-stage multi-cycle 32-bit RISC-V processor (without pipelining) supporting parts of the RV32I instruction set architecture. The RV32I core is relatively basic 
and does not include features like memory operations, exception handling, or branch instructions. It is designed for a simplified subset of the RISC-V ISA. It mainly 
focuses on ALU operations and basic instruction execution. 

    Instruction Memory:
        The CPU has an instruction memory (IMem) with 4096 words, each of 32 bits.
        The content of IMem is loaded from a binary file specified during the instantiation of the MultiCycleRV32Icore module.

    CPU Registers:
        The CPU has a program counter (PC) and a register file (regFile) with 32 registers, each holding a 32-bit value.
        Register x0 is hard-wired to zero.

    Microarchitectural Registers / Wires:
        Various signals are defined as either registers or wires depending on whether they need to be used in the same cycle or in a later cycle.

    Processor Stages:
        The FSM of the processor has five stages: fetch, decode, execute, memory, and writeback.
        The current stage is stored in a register named stage.

        Fetch Stage:
            The instruction is fetched from the instruction memory based on the current value of the program counter (PC).

        Decode Stage:
            Instruction fields such as opcode, rd, funct3, and rs1 are extracted.
            For R-type instructions, additional fields like funct7 and rs2 are extracted.
            Control signals (isADD, isSUB, etc.) are set based on the opcode and funct3 values.
            Operands (operandA and operandB) are determined based on the instruction type.

        Execute Stage:
            Arithmetic and logic operations are performed based on the control signals and operands.
            The result is stored in the aluResult register.

        Memory Stage:
            No memory operations are implemented in this basic CPU.

        Writeback Stage:
            The result of the operation (writeBackData) is written back to the destination register (rd) in the register file.
            The program counter (PC) is updated for the next instruction.

        Other:
            If the processor state is not in any of the defined stages, an assertion is triggered to indicate an error.

    Check Result:
        The final result (writeBackData) is output to the io.check_res signal.
        In the fetch stage, a default value of 0 is assigned to io.check_res.
*/

package core_tile

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

class MultiCycleRV32Icore (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val check_res = Output(UInt(32.W))
  })

  val fetch :: decode :: execute :: memory :: writeback :: Nil = Enum(5) // Enum datatype to define the stages of the processor FSM
  val stage = RegInit(fetch) 

  // -----------------------------------------
  // Instruction Memory
  // -----------------------------------------


  /*
   * TODO: Implement the memory as described above
   */

  val IMem = Mem(4096, UInt(32.W))
  loadMemoryFromFile(IMem, BinaryFile)

  // -----------------------------------------
  // CPU Registers
  // -----------------------------------------

  /*
   * TODO: Implement the program counter as a register, initialize with zero
   */

  val PC = RegInit(0.U(32.W))

  /*
   * TODO: Implement the Register File as described above
   */

  val regFile = Mem(32, UInt(32.W))
  regFile.write(0.U,0.U)

  // -----------------------------------------
  // Microarchitectural Registers / Wires
  // -----------------------------------------

  // if signal is processed in the same cycle --> wire
  // is signal is used in a later cycle       --> register

  /*
   * TODO: Implement the registers and wires you need in the individual stages of the processor 
   */
   //fetch
  val instr = RegInit(0.U(32.W))
   
  //decode
  val rd = RegInit(0.U(32.W))
 

  val isADD  = RegInit(false.B)
  val isSLT  = RegInit(false.B)
  val isSLTU = RegInit(false.B)
  val isAND  = RegInit(false.B)
  val isOR   = RegInit(false.B)
  val isXOR  = RegInit(false.B)
  val isSLL  = RegInit(false.B)
  val isSRL  = RegInit(false.B)
  val isSUB  = RegInit(false.B)
  val isSRA  = RegInit(false.B)
  val isADDI = RegInit(false.B)

   
val operandA = RegInit(0.U(32.W))
val operandB = RegInit(0.U(32.W))


//execute
val aluResult = RegInit(0.U(32.W))
//aluResult := 0.U

//writeback
val writeBackData = Wire(UInt(32.W))
writeBackData := 0.U
  

  // IOs need default case
  io.check_res := "h_0000_0000".U


  // -----------------------------------------
  // Processor Stages
  // -----------------------------------------

  when (stage === fetch)
  {
  /*
   * TODO: Implement fetch stage
   */
   instr := IMem(PC>>2.U)

   stage := decode 

  } 
    .elsewhen (stage === decode)
  {
  /*
   * TODO: Implement decode stage
   */

   val opcode = instr(6, 0)
   rd := instr(11,7)
   val funct3 = instr(14,12)
   val rs1 = instr(19,15)
   val rs2 = instr(24,20)
   val funct7 = instr(31,25)


  isADD  := (opcode === "b0110011".U && funct3 === "b000".U && funct7 === "b0000000".U)
  isSLTU := (opcode === "b0110011".U && funct3 === "b011".U && funct7 === "b0000000".U)
  isAND  := (opcode === "b0110011".U && funct3 === "b111".U && funct7 === "b0000000".U)
  isOR   := (opcode === "b0110011".U && funct3 === "b110".U && funct7 === "b0000000".U)
  isXOR  := (opcode === "b0110011".U && funct3 === "b100".U && funct7 === "b0000000".U)
  isSLL  := (opcode === "b0110011".U && funct3 === "b001".U && funct7 === "b0000000".U)
  isSRL  := (opcode === "b0110011".U && funct3 === "b101".U && funct7 === "b0000000".U)
  isSUB  := (opcode === "b0110011".U && funct3 === "b000".U && funct7 === "b0100000".U)
  isSRA  := (opcode === "b0110011".U && funct3 === "b101".U && funct7 === "b0100000".U)
  isADDI := (opcode === "b0010011".U && funct3 === "b000".U)
  isSLT := (opcode === "b0110011".U && funct3 === "b010".U && funct7 === "b0000000".U)

  operandA := regFile(rs1)

  when(opcode === "b0010011".U){
    operandB := instr(31,20)
  }.otherwise{
    operandB := regFile(rs2)
  }

stage := execute
  } 
    .elsewhen (stage === execute)
  {
  /*
   * TODO: Implement execute stage
   */
  when(isADDI) {
    aluResult := operandA + operandB
  }.elsewhen(isADD) {  
    aluResult := operandA + operandB 
  }.elsewhen(isSLT) {
    when((operandA).asSInt < (operandB).asSInt){
        aluResult := 1.U
    }.otherwise{
        aluResult := 0.U
    }
  }.elsewhen(isSLTU) {
    when((operandA).asUInt < (operandB).asUInt){
        aluResult := 1.U
    }.otherwise{
        aluResult := 0.U
    }
  }.elsewhen(isAND) {
    aluResult := operandA & operandB
  }.elsewhen(isOR) {
    aluResult := operandA | operandB 
  }.elsewhen(isXOR) {
    aluResult := operandA ^ operandB 
  }.elsewhen(isSLL) {
    aluResult := operandA << operandB(4,0)
  }.elsewhen(isSRL) {
    aluResult := operandA >> operandB 
  }.elsewhen(isSUB) {
    aluResult := operandA - operandB 
  }.elsewhen(isSRA) {
    aluResult := (operandA.asSInt >> operandB.asUInt).asUInt 
  }.otherwise{
    aluResult := 0.U
  }
  stage := memory
  }
    .elsewhen (stage === memory)
  {

    // No memory operations implemented in this basic CPU

    // TODO: There might still something be missing here
    stage := writeback

  } 
    .elsewhen (stage === writeback)
  {
  /*
   * TODO: Implement Writeback stag
   */
   writeBackData := aluResult
   regFile(rd) := writeBackData

  /*
   * TODO: Write result to output
   */
  io.check_res := 0.U
  io.check_res := writeBackData
  PC := PC + 4.U

  stage := fetch
  }
    .otherwise 
  {

     // default case (needed for RTL-generation but should never be reached   

     assert(true.B, "Pipeline FSM must never be left")

  }

}

