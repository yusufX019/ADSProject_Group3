// ADS I Class Project
// Pipelined RISC-V Core with Hazard Detetcion and Resolution
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 05/21/2024 by Andro Mazmishvili (@Andrew8846)

/*
The goal of this task is to equip the pipelined 5-stage 32-bit RISC-V core from the previous task with a forwarding unit that takes care of hazard detetction and hazard resolution.
The functionality is the same as in task 4, but the core should now also be able to also process instructions with operands depending on the outcome of a previous instruction without stalling.

In addition to the pipelined design from task 4, you need to implement the following modules and functionality:

    Hazard Detection and Forwarding:
        Forwarding Unit: Determines if and from where data should be forwarded to resolve hazards. 
                         Resolves data hazards by forwarding the correct values from later pipeline stages to earlier ones.
                         - Inputs: Register identifiers from the ID, EX, MEM, and WB stages.
                         - Outputs: Forwarding select signals (forwardA and forwardB) indicating where to forward the values from.

        The forwarding logic utilizes multiplexers to select the correct operand values based on forwarding decisions.

Make sure that data hazards (dependencies between instructions in the pipeline) are detected and resolved without stalling the pipeline. For additional information, you can revise the ADS I lecture slides (6-25ff).

Note this design only represents a simplified RISC-V pipeline. The structure could be equipped with further instructions and extension to support a real RISC-V ISA.
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


// -----------------------------------------
// Register File
// -----------------------------------------

class regFileReadReq extends Bundle {
    val addr  = Input(UInt(5.W))
}

class regFileReadResp extends Bundle {
    val data  = Output(UInt(32.W))
}

class regFileWriteReq extends Bundle {
    val addr  = Input(UInt(5.W))
    val data  = Input(UInt(32.W))
    val wr_en = Input(Bool())
}

class regFile extends Module {
  val io = IO(new Bundle {
    val read_req_1  = new regFileReadReq
    val read_resp_1 = new regFileReadResp
    val read_req_2  = new regFileReadReq
    val read_resp_2 = new regFileReadResp
    val write_req   = new regFileWriteReq
})

  val regFile = Mem(32, UInt(32.W))
  regFile.write(0.U, 0.U)                           // hard-wired zero for x0

  when(io.write_req.wr_en){
    when(io.write_req.addr =/= 0.U){
      regFile(io.write_req.addr) := io.write_req.data
    }
  }

  io.read_resp_1.data := Mux(io.read_req_1.addr === 0.U, 0.U, regFile(io.read_req_1.addr))
  io.read_resp_2.data := Mux(io.read_req_2.addr === 0.U, 0.U, regFile(io.read_req_2.addr))

}

class ForwardingUnit extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does the forwarding unit need?
    //input to check for hazards
    val idex_bar_rs1 = Input(UInt(5.W))
    val idex_bar_rs2 = Input(UInt(5.W))
<<<<<<< HEAD
<<<<<<< HEAD
    val exme_bar_rd  = Input(UInt(5.W))
    val mewb_bar_rd  = Input(UInt(5.W))
    //val wb_rd        = Input(UInt(5.W))

    //none for memory because there are no memory operations
    val forward_a    = Output(UInt(2.W))
    val forward_b    = Output(UInt(2.W))

    val ex_reg_w     = Input(Bool())
    val mem_reg_w    = Input(Bool())
    val wb_reg_w     = Input(Bool())
  })

    io.forward_a := 0.U
    io.forward_b := 0.U

  
=======
=======
>>>>>>> anya
    val exme_bar_rd  = Input(UInt(5.W)) //from EX bar
    val mewb_bar_rd  = Input(UInt(5.W)) // from MEM bar
    val wb_bar_rd    = Input (UInt(5.W)) // from WB bar

    //inputs to forward
    val exme_bar_result = Input(UInt(32.W)) //from EX bar
    val mewb_bar_result = Input(UInt(32.W)) //from MEM bar
    val wb_bar_result   = Input(UInt(32.W)) // from WB bar

    val operand_a = Output(UInt(32.W))
    val operand_b = Output(UInt(32.W))

    val forward_a = Output(UInt(2.W))
    val forward_b = Output(UInt(2.W))
  })
  io.forward_a := 0.U
  io.forward_b := 0.U
<<<<<<< HEAD
>>>>>>> 5ef621d98c3c20d90d687c60649b8c067ba61c43
=======
>>>>>>> anya


  /* TODO:
     Hazard detetction logic:
     Which pipeline stages are affected and how can a potential hazard be detected there?
  */
  //RAW hazards
<<<<<<< HEAD
<<<<<<< HEAD

  when(io.ex_reg_w && (io.exme_bar_rd =/= 0.U) && (io.exme_bar_rd === io.idex_bar_rs1)) {
    io.forward_a := 1.U
  }.elsewhen(io.mem_reg_w && (io.mewb_bar_rd =/= 0.U) && (io.mewb_bar_rd === io.idex_bar_rs1)) {
    io.forward_a := 2.U
  }/*.elsewhen(io.wb_reg_w && (io.wb_rd =/= 0.U) && (io.wb_rd === io.idex_bar_rs1)) {
    io.forward_a := 3.U
  }*/

  when(io.ex_reg_w && (io.exme_bar_rd =/= 0.U) && (io.exme_bar_rd === io.idex_bar_rs2)) {
    io.forward_b := 1.U
  }.elsewhen(io.mem_reg_w && (io.mewb_bar_rd =/= 0.U) && (io.mewb_bar_rd === io.idex_bar_rs2)) {
    io.forward_b := 2.U
  }/*.elsewhen(io.wb_reg_w && (io.wb_rd =/= 0.U) && (io.wb_rd === io.idex_bar_rs2)) {
    io.forward_b := 3.U
  }*/
  

=======
=======
>>>>>>> anya
  when(io.idex_bar_rs1 === io.mewb_bar_rd && io.mewb_bar_rd =/= 0.U ){
      io.forward_a := 2.U
  }.elsewhen(io.idex_bar_rs1 === io.exme_bar_rd && io.exme_bar_rd =/= 0.U) {
    io.forward_a := 1.U
  }.elsewhen(io.idex_bar_rs1 === io.wb_bar_rd && io.wb_bar_rd =/= 0.U){
    io.forward_a := 3.U
  }.otherwise{
    io.forward_a := 0.U
  }

  when(io.idex_bar_rs2 === io.mewb_bar_rd && io.mewb_bar_rd =/= 0.U ){
    io.forward_b := 2.U
  }.elsewhen(io.idex_bar_rs2 === io.exme_bar_rd && io.exme_bar_rd =/= 0.U){
    io.forward_b := 1.U
  }.elsewhen(io.idex_bar_rs2 === io.wb_bar_rd && io.wb_bar_rd =/= 0.U){
    io.forward_b := 3.U
  }.otherwise{
    io.forward_b := 0.U
  }
<<<<<<< HEAD
>>>>>>> 5ef621d98c3c20d90d687c60649b8c067ba61c43
=======
>>>>>>> anya
  //WAW hazards cannot occur here
  //WAR hazards cannot occur here

  /* TODO:
     Forwarding Selection:
     Select the appropriate value to forward from one stage to another based on the hazard checks.
  */
 /* //hazards on rs1
  when( io.forward_a === 1.U){
    io.operand_a := io.exme_bar_result
  }.elsewhen(io.forward_a === 2.U){
    io.operand_a := io.mewb_bar_result
  }.elsewhen(io.forward_a === 3.U){
    io.operand_a := io.wb_bar_result
  }.otherwise{
    io.operand_a := 0.U
  }
  
  //hazards on rs2
  when( io.forward_b === 1.U){
    io.operand_b := io.exme_bar_result
  }.elsewhen(io.forward_b === 2.U){
    io.operand_b := io.mewb_bar_result
  }.elsewhen(io.forward_b === 3.U){
    io.operand_b := io.wb_bar_result
  }.otherwise{
    io.operand_b := 0.U
<<<<<<< HEAD
<<<<<<< HEAD
  }*/
  printf(p"idex_bar_rs1: ${io.idex_bar_rs1}  idex_bar_rs2: ${io.idex_bar_rs2}\n")
  printf(p"exme_bar_rd: ${io.exme_bar_rd}  mewb_bar_rd: ${io.mewb_bar_rd}\n")
  printf(p"forward a: ${io.forward_a}  forward b:${io.forward_b}\n")

=======
  }
  
>>>>>>> 5ef621d98c3c20d90d687c60649b8c067ba61c43
=======
  }
  
>>>>>>> anya
}


// -----------------------------------------
// Fetch Stage
// -----------------------------------------

class IF (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val instr = Output(UInt(32.W))
  })

  val IMem = Mem(4096, UInt(32.W))
  loadMemoryFromFile(IMem, BinaryFile)

  val PC = RegInit(0.U(32.W))
  
  io.instr := IMem(PC>>2.U)

  // Update PC
  // no jumps or branches, next PC always reads next address from IMEM
  PC := PC + 4.U
  
}


// -----------------------------------------
// Decode Stage
// -----------------------------------------

class ID extends Module {
  val io = IO(new Bundle {
    val regFileReq_A  = Flipped(new regFileReadReq) 
    val regFileResp_A = Flipped(new regFileReadResp) 
    val regFileReq_B  = Flipped(new regFileReadReq) 
    val regFileResp_B = Flipped(new regFileReadResp) 
    val instr         = Input(UInt(32.W))
    val uop           = Output(uopc())
    val rd            = Output(UInt(5.W))
    val rs1           = Output(UInt(5.W))
    val rs2           = Output(UInt(5.W))
    val operandA      = Output(UInt(32.W))
    val operandB      = Output(UInt(32.W))
  })

  val opcode  = io.instr(6, 0)
  io.rd      := io.instr(11, 7)
  val funct3  = io.instr(14, 12)
  val rs1     = io.instr(19, 15)

  // R-Type
  val funct7  = io.instr(31, 25)
  val rs2     = io.instr(24, 20)

  // I-Type
  val imm     = io.instr(31, 20) 

  when(opcode === "b0110011".U){
    when(funct3 === "b000".U){
      when(funct7 === "b0000000".U){
        io.uop := isADD
      }.elsewhen(funct7 === "b0100000".U){
        io.uop := isSUB
      }.otherwise{
        io.uop := invalid
      }
    }.elsewhen(funct3 === "b100".U){
      when(funct7 === "b0000000".U){
        io.uop := isXOR
      }.otherwise{
        io.uop := invalid
      }
    }.elsewhen(funct3 === "b110".U){
      when(funct7 === "b0000000".U){
        io.uop := isOR
      }.otherwise{
        io.uop := invalid
      }
    }.elsewhen(funct3 === "b111".U){
      when(funct7 === "b0000000".U){
        io.uop := isAND
      }.otherwise{
        io.uop := invalid
      }
    }.elsewhen(funct3 === "b001".U){
      when(funct7 === "b0000000".U){
        io.uop := isSLL
      }.otherwise{
        io.uop := invalid
      }
    }.elsewhen(funct3 === "b101".U){
      when(funct7 === "b0000000".U){
        io.uop := isSRL
      }.elsewhen(funct7 === "b0100000".U){
        io.uop := isSRA
      }.otherwise{
        io.uop := invalid
      }
    }.elsewhen(funct3 === "b010".U){
      when(funct7 === "b0000000".U){
        io.uop := isSLT
      }.otherwise{
        io.uop := invalid
      }
    }.elsewhen(funct3 === "b011".U){
      when(funct7 === "b0000000".U){
        io.uop := isSLTU
      }.otherwise{
        io.uop := invalid
      }
    }.otherwise{
      io.uop := invalid
    }
  }.elsewhen(opcode === "b0010011".U){
    when(funct3 === "b000".U){
      io.uop := isADDI
    }.otherwise{
      io.uop := invalid
    }
  }.otherwise{
    io.uop := invalid
  }

  // Operands
  io.regFileReq_A.addr := rs1
  io.regFileReq_B.addr := rs2

  io.operandA := io.regFileResp_A.data
  io.operandB := Mux(opcode === "b0110011".U, io.regFileResp_B.data, Mux(opcode === "b0010011".U, imm, 0.U))

  io.rs1     := rs1
  io.rs2     := rs2  
}

// -----------------------------------------
// Execute Stage
// -----------------------------------------

class EX extends Module {
  val io = IO(new Bundle {
    val uop       = Input(uopc())
    val operandA  = Input(UInt(32.W))
    val operandB  = Input(UInt(32.W))
    val aluResult = Output(UInt(32.W))
  })

  val operandA = io.operandA
  val operandB = io.operandB
  val uop      = io.uop

  when(uop === isADDI) { 
      io.aluResult := operandA + operandB 
    }.elsewhen(uop === isADD) {                           
      io.aluResult := operandA + operandB 
    }.elsewhen(uop === isSUB) {  
      io.aluResult := operandA - operandB 
    }.elsewhen(uop === isXOR) {  
      io.aluResult := operandA ^ operandB 
    }.elsewhen(uop === isOR) {  
      io.aluResult := operandA | operandB 
    }.elsewhen(uop === isAND) {  
      io.aluResult := operandA & operandB 
    }.elsewhen(uop === isSLL) {  
      io.aluResult := operandA << operandB(4, 0) 
    }.elsewhen(uop === isSRL) {  
      io.aluResult := operandA >> operandB(4, 0) 
    }.elsewhen(uop === isSRA) {  
      io.aluResult := operandA >> operandB(4, 0)          // automatic sign extension, if SInt datatype is used
    }.elsewhen(uop === isSLT) {  
      io.aluResult := Mux(operandA < operandB, 1.U, 0.U)  // automatic sign extension, if SInt datatype is used
    }.elsewhen(uop === isSLTU) {  
      io.aluResult := Mux(operandA < operandB, 1.U, 0.U) 
    }.otherwise{
      io.aluResult := "h_FFFF_FFFF".U // = 2^32 - 1; self-defined encoding for invalid operation, value is unlikely to be reached in a regular arithmetic operation
    } 

}

// -----------------------------------------
// Memory Stage
// -----------------------------------------

class MEM extends Module {
  val io = IO(new Bundle {

  })

  // No memory operations implemented in this basic CPU

}

// -----------------------------------------
// Writeback Stage
// -----------------------------------------

class WB extends Module {
  val io = IO(new Bundle {
    val regFileReq = Flipped(new regFileWriteReq) 
    val rd         = Input(UInt(5.W))
    val aluResult  = Input(UInt(32.W))
    val check_res  = Output(UInt(32.W))
    val outRD      = Output(UInt(5.W))// output added
  })

 io.regFileReq.addr  := io.rd // output added
 io.regFileReq.data  := io.aluResult
 io.regFileReq.wr_en := io.aluResult =/= "h_FFFF_FFFF".U  // could depend on the current uopc, if ISA is extendet beyond R-type and I-type instructions

 io.check_res := io.aluResult
 io.outRD     := io.rd //declaration added

}


// -----------------------------------------
// IF-Barrier
// -----------------------------------------

class IFBarrier extends Module {
  val io = IO(new Bundle {
    val inInstr  = Input(UInt(32.W))
    val outInstr = Output(UInt(32.W))
  })

  val instrReg = RegInit(0.U(32.W))

  io.outInstr := instrReg
  instrReg    := io.inInstr

}


// -----------------------------------------
// ID-Barrier
// -----------------------------------------

class IDBarrier extends Module {
  val io = IO(new Bundle {
    val inUOP       = Input(uopc())
    val inRD        = Input(UInt(5.W))
    val inRS1       = Input(UInt(5.W))
    val inRS2       = Input(UInt(5.W))
    val inOperandA  = Input(UInt(32.W))
    val inOperandB  = Input(UInt(32.W))
    val outUOP      = Output(uopc())
    val outRD       = Output(UInt(5.W))
    val outRS1      = Output(UInt(5.W))
    val outRS2      = Output(UInt(5.W))
    val outOperandA = Output(UInt(32.W))
    val outOperandB = Output(UInt(32.W))
  })

  val uop      = Reg(uopc())
  val rd       = RegInit(0.U(5.W))
  val rs1      = RegInit(0.U(5.W))
  val rs2      = RegInit(0.U(5.W))
  val operandA = RegInit(0.U(32.W))
  val operandB = RegInit(0.U(32.W))

  io.outUOP := uop
  uop := io.inUOP
  io.outRD := rd
  rd := io.inRD
  io.outRS1 := rs1
  rs1 := io.inRS1
  io.outRS2 := rs2
  rs2 := io.inRS2
  io.outOperandA := operandA
  operandA := io.inOperandA
  io.outOperandB := operandB
  operandB := io.inOperandB

}


// -----------------------------------------
// EX-Barrier
// -----------------------------------------

class EXBarrier extends Module {
  val io = IO(new Bundle {
    val inAluResult  = Input(UInt(32.W))
    val outAluResult = Output(UInt(32.W))
    val inRD         = Input(UInt(5.W))
    val outRD        = Output(UInt(5.W))
  })

  val aluResult = RegInit(0.U(32.W))
  val rd       = RegInit(0.U(5.W))

  io.outAluResult := aluResult
  aluResult       := io.inAluResult

  io.outRD := rd
  rd := io.inRD

}


// -----------------------------------------
// MEM-Barrier
// -----------------------------------------

class MEMBarrier extends Module {
  val io = IO(new Bundle {
    val inAluResult  = Input(UInt(32.W))
    val outAluResult = Output(UInt(32.W))
    val inRD         = Input(UInt(5.W))
    val outRD        = Output(UInt(5.W))
  })

  val aluResult = RegInit(0.U(32.W))
  val rd        = RegInit(0.U(5.W))

  io.outAluResult := aluResult
  aluResult       := io.inAluResult

  io.outRD := rd
  rd := io.inRD

}


// -----------------------------------------
// WB-Barrier
// -----------------------------------------

class WBBarrier extends Module {
  val io = IO(new Bundle {
    val inCheckRes   = Input(UInt(32.W))
    val outCheckRes  = Output(UInt(32.W))
    val inRD         = Input(UInt(5.W)) //input added
    val outRD        = Output(UInt(5.W)) // output added 
  })

  val check_res   = RegInit(0.U(32.W))
  val rd          = RegInit(0.U(5.W)) //register added

  io.outCheckRes := check_res
  check_res      := io.inCheckRes
  //declaration added
  io.outRD       := rd
  rd             := io.inRD
}


// -----------------------------------------
// Main Class
// -----------------------------------------

class HazardDetectionRV32Icore (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val check_res = Output(UInt(32.W))
  })


  // Pipeline Registers
  val IFBarrier  = Module(new IFBarrier)
  val IDBarrier  = Module(new IDBarrier)
  val EXBarrier  = Module(new EXBarrier)
  val MEMBarrier = Module(new MEMBarrier)
  val WBBarrier  = Module(new WBBarrier)

  // Pipeline Stages
  val IF  = Module(new IF(BinaryFile))
  val ID  = Module(new ID)
  val EX  = Module(new EX)
  val MEM = Module(new MEM)
  val WB  = Module(new WB)

  /* 
    TODO: Instantiate the forwarding unit.
  */
  val ForwardingUnit = Module(new ForwardingUnit)


  //Register File
  val regFile = Module(new regFile)

  // Connections for IOs
  IFBarrier.io.inInstr      := IF.io.instr
  
  ID.io.instr               := IFBarrier.io.outInstr
  ID.io.regFileReq_A        <> regFile.io.read_req_1
  ID.io.regFileReq_B        <> regFile.io.read_req_2
  ID.io.regFileResp_A       <> regFile.io.read_resp_1
  ID.io.regFileResp_B       <> regFile.io.read_resp_2

  IDBarrier.io.inUOP        := ID.io.uop
  IDBarrier.io.inRD         := ID.io.rd
  IDBarrier.io.inRS1        := ID.io.rs1
  IDBarrier.io.inRS2        := ID.io.rs2
  IDBarrier.io.inOperandA   := ID.io.operandA
  IDBarrier.io.inOperandB   := ID.io.operandB

  /* 
    TODO: Connect the I/Os of the forwarding unit 
  */
  ForwardingUnit.io.idex_bar_rs1 := IDBarrier.io.outRS1
  ForwardingUnit.io.idex_bar_rs2 := IDBarrier.io.outRS2
<<<<<<< HEAD
<<<<<<< HEAD
  ForwardingUnit.io.exme_bar_rd  := EXBarrier.io.outRD
  ForwardingUnit.io.mewb_bar_rd  := MEMBarrier.io.outRD
  //ForwardingUnit.io.wb_rd        := WB.io.rd
  ForwardingUnit.io.ex_reg_w := (EXBarrier.io.outRD =/= 0.U)
  ForwardingUnit.io.mem_reg_w := (MEMBarrier.io.outRD =/= 0.U)
  ForwardingUnit.io.wb_reg_w := (WB.io.rd =/= 0.U)

=======
=======
>>>>>>> anya
  ForwardingUnit.io.exme_bar_rd := EXBarrier.io.outRD
  ForwardingUnit.io.mewb_bar_rd := MEMBarrier.io.outRD
  ForwardingUnit.io.wb_bar_rd := WBBarrier.io.outRD
  ForwardingUnit.io.exme_bar_result := EXBarrier.io.outAluResult
  ForwardingUnit.io.mewb_bar_result := MEMBarrier.io.outAluResult
  ForwardingUnit.io.wb_bar_result := WBBarrier.io.outCheckRes
<<<<<<< HEAD
>>>>>>> 5ef621d98c3c20d90d687c60649b8c067ba61c43
=======
>>>>>>> anya

  /* 
    TODO: Implement MUXes to select which values are sent to the EX stage as operands
  */
<<<<<<< HEAD
<<<<<<< HEAD
  printf(p"in main class before mux, forward a= ${ForwardingUnit.io.forward_a}  forward b= ${ForwardingUnit.io.forward_b}\n")
  /*EX.io.operandA := Mux(ForwardingUnit.io.forward_a === 0.U,IDBarrier.io.outOperandA, ForwardingUnit.io.operand_a)
  EX.io.operandB := Mux(ForwardingUnit.io.forward_b === 0.U,IDBarrier.io.outOperandB, ForwardingUnit.io.operand_b)*/
  
=======
>>>>>>> 5ef621d98c3c20d90d687c60649b8c067ba61c43
=======
>>>>>>> anya
  EX.io.uop := IDBarrier.io.outUOP

  EX.io.operandA := Mux(ForwardingUnit.io.forward_a === 0.U,IDBarrier.io.outOperandA, ForwardingUnit.io.operand_a)
  EX.io.operandB := Mux(ForwardingUnit.io.forward_b === 0.U,IDBarrier.io.outOperandB, ForwardingUnit.io.operand_b)

  //EX.io.operandA := 0.U // just there to make empty project buildable
  //EX.io.operandB := 0.U // just there to make empty project buildable

  EX.io.operandA := MuxLookup(ForwardingUnit.io.forward_a, IDBarrier.io.outOperandA, Array(
    1.U -> EXBarrier.io.outAluResult,
    2.U -> MEMBarrier.io.outAluResult,
    3.U -> WB.io.aluResult
  ))

  EX.io.operandB := MuxLookup(ForwardingUnit.io.forward_b, IDBarrier.io.outOperandB, Array(
    1.U -> EXBarrier.io.outAluResult,
    2.U -> MEMBarrier.io.outAluResult,
    3.U -> WB.io.aluResult
  ))

  EXBarrier.io.inRD         := IDBarrier.io.outRD
  EXBarrier.io.inAluResult  := EX.io.aluResult

  MEMBarrier.io.inRD        := EXBarrier.io.outRD
  MEMBarrier.io.inAluResult := EXBarrier.io.outAluResult

  WB.io.rd                  := MEMBarrier.io.outRD
  WB.io.aluResult           := MEMBarrier.io.outAluResult
  WB.io.regFileReq          <> regFile.io.write_req


  WBBarrier.io.inCheckRes   := WB.io.check_res
  WBBarrier.io.inRD         := WB.io.outRD //connection added

  io.check_res              := WBBarrier.io.outCheckRes

}

