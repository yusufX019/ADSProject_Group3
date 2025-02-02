// ADS I Class Project
// Pipelined RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/15/2023 by Tobias Jauch (@tojauch)

/*
The goal of this task is to extend the 5-stage multi-cycle 32-bit RISC-V core from the previous task to a pipelined processor. 
All steps and stages have the same functionality as in the multi-cycle version from task 03, but are supposed to handle different instructions in each stage simultaneously.
This design implements a pipelined RISC-V 32-bit core with five stages: IF (Fetch), ID (Decode), EX (Execute), MEM (Memory), and WB (Writeback).

    Data Types:
        The uopc enumeration data type (enum) defines micro-operation codes representing ALU operations according to the RV32I subset used in the previous tasks.

    Register File (regFile):
        The regFile module represents the register file, which has read and write ports.
        It consists of a 32-entry register file (x0 is hard-wired to zero).
        Reading from and writing to the register file is controlled by the read request (regFileReadReq), read response (regFileReadResp), and write request (regFileWriteReq) interfaces.

    Fetch Stage (IF Module):
        The IF module represents the instruction fetch stage.
        It includes an instruction memory (IMem) of size 4096 words (32-bit each).
        Instructions are loaded from a binary file (provided to the testbench as a parameter) during initialization.
        The program counter (PC) is used as an address to access the instruction memory, and one instruction is fetched in each cycle.

    Decode Stage (ID Module):
        The ID module performs instruction decoding and generates control signals.
        It extracts opcode, operands, and immediate values from the instruction.
        It uses the uopc (micro-operation code) Enum to determine the micro-operation (uop) and sets control signals accordingly.
        The register file requests are generated based on the operands in the instruction.

    Execute Stage (EX Module):
        The EX module performs the arithmetic or logic operation based on the micro-operation code.
        It takes two operands and produces the result (aluResult).

    Memory Stage (MEM Module):
        The MEM module does not perform any memory operations in this basic CPU design.

    Writeback Stage (WB Module):
        The WB module writes the result back to the register file.

    IF, ID, EX, MEM, WB Barriers:
        IFBarrier, IDBarrier, EXBarrier, MEMBarrier, and WBBarrier modules serve as pipeline registers to separate the pipeline stages.
        They hold the intermediate results of each stage until the next clock cycle.

    PipelinedRV32Icore (PipelinedRV32Icore Module):
        The top-level module that connects all the pipeline stages, barriers and the register file.
        It interfaces with the external world through check_res, which is the result produced by the core.

Overall Execution Flow:

    1) Instructions are fetched from the instruction memory in the IF stage.
    2) The fetched instruction is decoded in the ID stage, and the corresponding micro-operation code is determined.
    3) The EX stage executes the operation using the operands.
    4) The MEM stage does not perform any memory operations in this design.
    5) The result is written back to the register file in the WB stage.

Note that this design only represents a simplified RISC-V pipeline. The structure could be equipped with further instructions and extension to support a real RISC-V ISA.
*/

package core_tile

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile


// -----------------------------------------
// Global Definitions and Data Types
// -----------------------------------------

object uopc extends ChiselEnum {

  val isADD   = Value(0x01.U)
  val isSUB   = Value(0x02.U)
  val isXOR   = Value(0x03.U)
  val isOR    = Value(0x04.U)
  val isAND   = Value(0x05.U)
  val isSLL   = Value(0x06.U)
  val isSRL   = Value(0x07.U)
  val isSRA   = Value(0x08.U)
  val isSLT   = Value(0x09.U)
  val isSLTU  = Value(0x0A.U)

  val isADDI  = Value(0x10.U)

  val invalid = Value(0xFF.U)
}

import uopc._

//val PC = RegInit(0.U(32.W))
//val IMem = Mem(4096, UInt(32.W))

// -----------------------------------------
// Register File
// -----------------------------------------

class regFileReadReq extends Bundle {
    // what signals does a read request need?
    val rr_rs1 = Input(UInt(5.W))    // Wire(UInt(12.W))
    val rr_rs2 = Input(UInt(5.W))    // Wire(UInt(12.W))
}

class regFileReadResp extends Bundle {
    // what signals does a read response need?
    val rp_d1 = Output(UInt(32.W))    // Wire(UInt(32.W))
    val rp_d2 = Output(UInt(32.W))    // Wire(UInt(32.W))
}

class regFileWriteReq extends Bundle {
    // what signals does a write request need?
    val wr_rd = Input(UInt(5.W))  // Wire(UInt(12.W))
    val wr_d  = Input(UInt(32.W))  // Wire(UInt(32.W))
    val wr_writeEnable = Input(Bool())
}

class regFile extends Module {
  val io = IO(new Bundle {
    val req  = new regFileReadReq
    val resp = new regFileReadResp
    val write = new regFileWriteReq
    // how many read and write ports do you need to handle all requests
    // from the pipeline to the register file simultaneously?
        // 2 ports for reading and 1 for writing
})

  /* 
    TODO: Initialize the register file as described in the task 
          and handle the read and write requests
   */

  val regFile = Mem(32, UInt(32.W))
  regFile.write(0.U,0.U)

  when(io.req.rr_rs1 === 0.U){
    io.resp.rp_d1 := 0.U
  }.otherwise{
    io.resp.rp_d1 := regFile(io.req.rr_rs1)
  }
  
   when(io.req.rr_rs2 === 0.U){
    io.resp.rp_d2 := 0.U
  }.otherwise{
    io.resp.rp_d2 := regFile(io.req.rr_rs2)
  }

  when (io.write.wr_writeEnable && io.write.wr_rd =/= 0.U){
    regFile(io.write.wr_rd) := io.write.wr_d
  }

}

// -----------------------------------------
// Fetch Stage
// -----------------------------------------

class IF (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this pipeline stage need?
    val instrOut = Output(UInt(32.W))
    val PCOut = Output(UInt(32.W))
  })

  /* 
    TODO: Initialize the IMEM as described in the task 
          and handle the instruction fetch.*/
  val IMem = Mem(4096, UInt(32.W))
  loadMemoryFromFile(IMem, BinaryFile) //maybe mising declaration

  val PC = RegInit(0.U(32.W))

   /* TODO: Update the program counter (no jumps or branches, 
          next PC always reads next address from IMEM)
   */
   PC := PC + 4.U
   val instr = IMem(PC>>2)

  io.PCOut := PC
   io.instrOut := instr

  
}


// -----------------------------------------
// Decode Stage
// -----------------------------------------

class ID extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this pipeline stage need?
    val instrIn = Input(UInt(32.W))
    val microOP = Output(uopc())
    val rdOUt = Output(UInt(5.W))
    val imm = Output(UInt(12.W))
    val operandA_out = Output(UInt(32.W))
    val operandB_out = Output(UInt(32.W))
  })

  /* 
   * TODO: Any internal signals needed?
   */
  val opcode = io.instrIn(6, 0)
  val rd = io.instrIn(11,7)
  val funct3 = io.instrIn(14,12)
  val rs1 = io.instrIn(19,15)
  val rs2 = io.instrIn(24,20)
  val funct7 = io.instrIn(31,25)
  val imm_value = io.instrIn(31,20)

  /* 
    Determine the uop based on the disassembled instruction*/

   when(opcode === "b0110011".U && funct3 === "b000".U && funct7 === "b0000000".U){
    io.microOP := uopc.isADD
   }.elsewhen(opcode === "b0110011".U && funct3 === "b011".U && funct7 === "b0000000".U){
    io.microOP := uopc.isSLTU
   }.elsewhen(opcode === "b0110011".U && funct3 === "b111".U && funct7 === "b0000000".U){
    io.microOP := uopc.isAND
   }.elsewhen(opcode === "b0110011".U && funct3 === "b110".U && funct7 === "b0000000".U){
    io.microOP := uopc.isOR
   }.elsewhen(opcode === "b0110011".U && funct3 === "b100".U && funct7 === "b0000000".U){
    io.microOP := uopc.isXOR
   }.elsewhen(opcode === "b0110011".U && funct3 === "b001".U && funct7 === "b0000000".U){
    io.microOP := uopc.isSLL
   }.elsewhen(opcode === "b0110011".U && funct3 === "b101".U && funct7 === "b0000000".U){
    io.microOP := uopc.isSRL
   }.elsewhen(opcode === "b0110011".U && funct3 === "b000".U && funct7 === "b0100000".U){
    io.microOP := uopc.isSUB
   }.elsewhen(opcode === "b0110011".U && funct3 === "b101".U && funct7 === "b0100000".U){
    io.microOP := uopc.isSRA
   }.elsewhen(opcode === "b0010011".U && funct3 === "b000".U){
    io.microOP := uopc.isADDI
   }.elsewhen(opcode === "b0110011".U && funct3 === "b010".U && funct7 === "b0000000".U){
    io.microOP := uopc.isSLT
   }.otherwise{
    io.microOP := uopc.invalid
   }

  /* 
   * TODO: Read the operands from teh register file
   */
  io.operandA_out := rs1
  io.operandB_out := rs2
  io.imm := imm_value
  io.rdOUt := rd
}

// -----------------------------------------
// Execute Stage
// -----------------------------------------

class EX extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this pipeline stage need?
    val operandA  = Input(UInt(32.W))
    val operandB  = Input(UInt(32.W))
    val microOP   = Input(uopc())
    val imm       = Input(UInt(12.W))
    val rdIn      = Input(UInt(5.W))
    val rdOut     = Output(UInt(5.W))
    val aluResult = Output(UInt(32.W))
  })

  /* 
    TODO: Perform the ALU operation based on the uopc
  */

    when(io.microOP === isADDI) {
    io.aluResult := (io.imm.asSInt + io.operandA.asSInt).asUInt
    }.elsewhen(io.microOP === isADD) {  
      io.aluResult := (io.operandA.asSInt + io.operandB.asSInt).asUInt 
    }.elsewhen(io.microOP === isSLT) {
      when(io.operandA.asSInt < io.operandB.asSInt){
          io.aluResult := 1.U
      }.otherwise{
          io.aluResult := 0.U
      }
    }.elsewhen(io.microOP === isSLTU) {
      when(io.operandA < io.operandB){
          io.aluResult := 1.U
      }.otherwise{
          io.aluResult := 0.U
      }
    }.elsewhen(io.microOP === isAND) {
      io.aluResult := io.operandA & io.operandB
    }.elsewhen(io.microOP === isOR) {
      io.aluResult := io.operandA | io.operandB 
    }.elsewhen(io.microOP === isXOR) {
      io.aluResult := io.operandA ^ io.operandB 
    }.elsewhen(io.microOP === isSLL) {
      io.aluResult := io.operandA << io.operandB(4,0)
    }.elsewhen(io.microOP === isSRL) {
      io.aluResult := io.operandA >> io.operandB 
    }.elsewhen(io.microOP === isSUB) {
      io.aluResult := io.operandA - io.operandB 
    }.elsewhen(io.microOP === isSRA) {
      io.aluResult := (io.operandA.asSInt >> io.operandB.asUInt).asUInt 
    }.otherwise{
    io.aluResult := 5.U
  }

  io.rdOut := io.rdIn
}

// -----------------------------------------
// Memory Stage
// -----------------------------------------

class MEM extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this pipeline stage need?
  })

  // No memory operations implemented in this basic CPU

}


// -----------------------------------------
// Writeback Stage
// -----------------------------------------

class WB extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this pipeline stage need?
    val res = Input(UInt(32.W))
    val rd  = Input(UInt(5.W))
    val addr = Output(UInt(5.W))
    val data = Output(UInt(32.W))

  })

  /* 
   * TODO: Perform the write back to the register file and set 
   *       the check_res signal for the testbench.
   */
  io.addr := io.rd
  io.data := io.res
}


// -----------------------------------------
// IF-Barrier
// -----------------------------------------

class IFBarrier extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this barrier need?
  })

  /* 
   * TODO: Define registers
   *
   * TODO: Fill registers from the inputs and write regioster values to the outputs
   */

}


// -----------------------------------------
// ID-Barrier: IF-ID
// -----------------------------------------

class IDBarrier extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this barrier need?
    val instrOut = Output(UInt(32.W))
    val PCOut    = Output(UInt(32.W))
    val instrIn  = Input(UInt(32.W))
    val PCIn     = Input(UInt(32.W))
  })

  /* TODO: Define registers */
    val instrReg = RegInit(0.U(32.W))
    val pcReg    = RegInit(0.U(32.W))


   /* TODO: Fill registers from the inputs and write regioster values to the outputs*/
   instrReg    := io.instrIn
   pcReg       := io.PCIn

   io.instrOut := instrReg
   io.PCOut    := pcReg

}


// -----------------------------------------
// EX-Barrier: ID-EX
// -----------------------------------------

class EXBarrier extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this barrier need?
    val operandA_in = Input(UInt(32.W))
    val operandB_in = Input(UInt(32.W))
    val microOP_in  = Input(uopc())
    val imm_in      = Input(UInt(12.W))
    val rd_in       = Input(UInt(5.W))

    val operandA_out = Output(UInt(32.W))
    val operandB_out = Output(UInt(32.W))
    val microOP_out  = Output(uopc())
    val imm_out      = Output(UInt(12.W))
    val rd_out       = Output(UInt(5.W))

  })

  /* TODO: Define registers */

  val operandA_reg = Reg(UInt(32.W))
  val operandB_reg = Reg(UInt(32.W))
  val microOP_reg  = Reg(uopc())
  val imm_reg      = Reg(UInt(12.W))
  val rd_reg       = Reg(UInt(5.W))

  /* TODO: Fill registers from the inputs and write regioster values to the outputs */

  operandA_reg := io.operandA_in
  operandB_reg := io.operandB_in
  microOP_reg  := io.microOP_in
  imm_reg      := io.imm_in
  rd_reg       := io.rd_in
 
  io.operandA_out := operandA_reg
  io.operandB_out := operandB_reg
  io.microOP_out  := microOP_reg 
  io.imm_out      := imm_reg
  io.rd_out       := rd_reg
}


// -----------------------------------------
// MEM-Barrier: EX-MEM
// -----------------------------------------

class MEMBarrier extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this barrier need?
    val data_in  = Input(UInt(32.W))
    val rd_in    = Input(UInt(5.W))
    val data_out = Output(UInt(32.W))
    val rd_out   = Output(UInt(5.W))
  })

  /* TODO: Define registers */

  val data_reg = Reg(UInt(32.W))
  val rd_reg   = Reg(UInt(5.W))

  /* TODO: Fill registers from the inputs and write regioster values to the outputs */
  
  data_reg    := io.data_in
  rd_reg      := io.rd_in 
  io.data_out := data_reg
  io.rd_out   := rd_reg
}


// -----------------------------------------
// WB-Barrier: MEM-WB
// -----------------------------------------

class WBBarrier extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this barrier need?
    val data_in  = Input(UInt(32.W))
    val addr_in  = Input(UInt(5.W))
    val data_out = Output(UInt(32.W))
    val addr_out = Output(UInt(5.W))
  })

  /* 
   * TODO: Define registers */

  val reg_data = Reg(UInt(32.W))
  val reg_addr = Reg(UInt(5.W))
  
  /* TODO: Fill registers from the inputs and write regioster values to the outputs */

  reg_data    := io.data_in
  reg_addr    := io.addr_in
  io.data_out := reg_data
  io.addr_out    := reg_addr

}



class PipelinedRV32Icore (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val check_res = Output(UInt(32.W))
  })

  val regFile = Module(new regFile)
  regFile.io.write.wr_writeEnable := 0.U //disabling write enable

  /* 
   * TODO: Instantiate Barriers
   */

  val if_id_bar  = Module(new IDBarrier)
  val id_ex_bar  = Module(new EXBarrier)
  val ex_mem_bar = Module(new MEMBarrier)
  val mem_wb_bar = Module(new WBBarrier)


  /* 
   * TODO: Instantiate Pipeline Stages
   */

  val if_stage  = Module(new IF(BinaryFile))
  val id_stage  = Module(new ID)
  val ex_stage  = Module(new EX)
  val mem_stage = Module(new MEM)
  val wb_stage  = Module(new WB)


  /* 
   * TODO: Instantiate Register File
   */
  io.check_res := 0.U // necessary to make the empty design buildable TODO: change this

  /* 
   * TODO: Connect all IOs between the stages, barriers and register file.
   * Do not forget the global output of the core module
   */

  // getting if/id barrier inputs from if stage outputs
  if_id_bar.io.instrIn := if_stage.io.instrOut
  if_id_bar.io.PCIn    := if_stage.io.PCOut

  // getting id stage inputs from if/id barrier
  id_stage.io.instrIn := if_id_bar.io.instrOut

  regFile.io.req.rr_rs1 := id_stage.io.operandA_out
  regFile.io.req.rr_rs2 := id_stage.io.operandB_out

  // getting id/ex barrier inputs from id stage outputs
  id_ex_bar.io.operandA_in := regFile.io.resp.rp_d1
  id_ex_bar.io.operandB_in := regFile.io.resp.rp_d2
  id_ex_bar.io.microOP_in  := id_stage.io.microOP
  id_ex_bar.io.imm_in      := id_stage.io.imm
  id_ex_bar.io.rd_in       := id_stage.io.rdOUt

  // getting ex stage inputs from id/ex barrier outputs
  ex_stage.io.operandA   := id_ex_bar.io.operandA_out
  ex_stage.io.operandB   := id_ex_bar.io.operandB_out
  ex_stage.io.microOP    := id_ex_bar.io.microOP_out
  ex_stage.io.imm        := id_ex_bar.io.imm_out
  ex_stage.io.rdIn       := id_ex_bar.io.rd_out

  // getting ex/mem barrier inputs from ex stage outputs
  ex_mem_bar.io.data_in := ex_stage.io.aluResult
  ex_mem_bar.io.rd_in := id_ex_bar.io.rd_out

  // getting mem/wb inputs from ex/mem barrier outputs, thus
  // we skipped mem stage
  mem_wb_bar.io.data_in := ex_mem_bar.io.data_out
  mem_wb_bar.io.addr_in := ex_mem_bar.io.rd_out

  // getting wb stage inputs from mem/wb barrier output
  wb_stage.io.res := mem_wb_bar.io.data_out
  wb_stage.io.rd  := mem_wb_bar.io.addr_out

  regFile.io.write.wr_writeEnable := 1.U        // enabling writing
  regFile.io.write.wr_rd := wb_stage.io.addr
  regFile.io.write.wr_d  := wb_stage.io.data

  io.check_res := wb_stage.io.data
}

