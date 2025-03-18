// ADS I Class Project
// Pipelined RISC-V Core with BTB

package core_tile

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

// -----------------------------------------
// Definition of branch target buffer
// -----------------------------------------

/* Bundles within the BTB */
class BtbEntry extends Bundle {
  val valid = UInt(1.W)
  val tag = UInt(29.W)
  val target_address = UInt(32.W)
  val prediction = StateBranchTargetBuffer.State() // 2-bit FSM (see line 30)
}

class BtbSet extends Bundle {
  val ways = Vec(2, new BtbEntry())
  val LRU_counter = UInt(1.W) // if 0 then way 1 was most recently used, if 1 then way 1 most recenlty used
}

val index = io.PC //Here??

// States for the BTB State Machine
object StateBranchTargetBuffer {
  object State extends ChiselEnum {
    val StrongTaken, WeakTaken, WeakNotTaken, StrongNotTaken = Value
  }
}

// Defintion of the BTB
class BranchTargetBuffer extends Module{
  import StateBranchTargetBuffer.State
  import StateBranchTargetBuffer.State._
  // Inputs/Outputs
  val io = IO(new Bundle {
    val PC  = Input(UInt(32.W))
    val update  = Input(UInt(1.W))
    val updatePC  = Input(UInt(32.W))
    val updateTarget  = Input(UInt(32.W))
    val mispredicted  = Input(UInt(1.W)) //1= miss 0= good predic
    val branch = Input(Bool())

    val valid = Output(UInt(1.W))
    val target = Output(UInt(32.W))
    val predictTaken = Output(UInt(1.W))
  })
  // Structure
  val btb = RegInit(VecInit(Seq.fill(8)(0.U.asTypeOf(new BtbSet()))))

  // Index and Tag extraction
  val index = io.PC(4,2)
  val tag = io.PC(31,5)

  // Check if any way has a matching tag
  val way0Match = btb(index).ways(0).valid && (btb(index).ways(0).tag === tag)
  val way1Match = btb(index).ways(1).valid && (btb(index).ways(1).tag === tag)
  val hit = way0Match || way1Match

  // Select the correct way if there is a match
  val waySel = Mux(way0Match, 0.U, 1.U)

  // Get the predictor state
  val predState = btb(index).ways(waySel).prediction
  val predictedTaken = predState >= StateBranchTargetBuffer.State.WeakTaken


  /* State Machine Transition Logic (following the scheme sl.6-47) */
  
  val stateBtb = State()
  //starting state
  stateBtb := State.WeakNotTaken 

  when(io.branch){ //add inside also the logic for prediction
    switch(stateBtb){
      is(State.StrongNotTaken){ //00
        when(io.mispredicted){
          stateBtb := State.WeakNotTaken
        }.otherwise{
          stateBtb := State.StrongNotTaken
        }
      }
      is(State.WeakNotTaken){ //01
        when(io.mispredicted){
          stateBtb := State.WeakTaken
        }.otherwise{
          stateBtb := State.StrongNotTaken
        }
      }
      is(State.WeakTaken){ //10
        when(io.mispredicted){
          stateBtb := State.StrongTaken
        }.otherwise{
          stateBtb := State.WeakTaken
        }
      }
      is(State.StrongTaken){ //11
        when(io.mispredicted){
          stateBtb := State.StrongNotTaken
        }.otherwise{
          stateBtb := State.WeakTaken
        }
      }
    }
  }
  //different "when statement" to implement cache, will be merged with previous

  //checking if target address is in btb
  when(branch){
    val currentSet = btb(io.PC(4,2)) 
    when(currentSet(1).tag === io.PC(31,5)){ //get the 1st entry
      when(currentSet(1).valid){
        //predict taken
        io.target = currentSet(1).target_address
        //change prediction
        currentSet.LRU_counter = 1.U
      }
    }.elsewhen(currentSet(2).tag === io.PC(31,5)){//get 2nd entry
      when(currentSet(2).valid){
        //predict taken
         io.target = currentSet(2).target_address
        //change prediction 
        currentSet.LRU_counter = 0.U
      }
    }.otherwise{
      //predict not taken
      io.target = io.PC + 4.U
    }
  }

  //writing new entry + eviction
  when(io.update && io.mispredicted){ 
    val currentSet := btb(io.PC(4,2))
    when(currentSet(1).valid ==== 0.U || (currentSet.LRU_counter === 1.U && currentSet(1).valid ==== 1.U && currentSet(2).valid ==== 1.U)){
      currentSet(1).valid = 1.U
      currentSet(1).tag = io.updatePC(31,5)
      currentSet(1).target_address = io.updateTarget
      currentSet.LRU_counter = 1.U
      //update counter
      when((io.mispredicted && io.predictTaken) || (~io.predictTaken && io.mispredicted)){
        currentSet(1).prediction = currentSet(1).prediction - 1.U
      }elswhen(~io.mispredicted && ~io.predictTaken || (io.predictTaken && ~io.mispredicted)){
        currentSet(1).prediction = currentSet(1).prediction + 1.U
      } //link it to FSM
    }.elsewhen(currentSet(2).valid ==== 0.U || (currentSet.LRU_counter === 0.U && currentSet(1).valid ==== 1.U && currentSet(2).valid ==== 1.U)){
      currentSet(2).valid = 1.U
      currentSet(2).tag = io.updatePC(31,5)
      currentSet(2).target_address = io.updateTarget
      currentSet.LRU_counter = 0.U
      //update counter
      when((io.mispredicted && io.predictTaken) || (~io.predictTaken && io.mispredicted)){
        currentSet(2).prediction = currentSet(2).prediction - 1.U
      }elswhen(~io.mispredicted && ~io.predictTaken || (io.predictTaken && ~io.mispredicted)){
        currentSet(2).prediction = currentSet(2).prediction + 1.U
      } //link it to FSM
    }
  }

  // Output Connections
  io.valid := hit
  io.target := Mux(hit, btb(index).ways(waySel).target_address, 0.U)
  io.predictTaken := Mux(hit, predictedTaken, false.B)
}


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

  val isJAL  = Value(0x11.U)
  val isJALR = Value(0x12.U)
  val isBEQ  = Value(0x13.U)
  val isBNE  = Value(0x14.U)
  val isBLT  = Value(0x15.U)
  val isBGE  = Value(0x16.U)

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
    val rr_rs1 = Input(UInt(5.W))    
    val rr_rs2 = Input(UInt(5.W))  
}

class regFileReadResp extends Bundle {
    // what signals does a read response need?
    val rp_d1 = Output(UInt(32.W))    
    val rp_d2 = Output(UInt(32.W))    
}

class regFileWriteReq extends Bundle {
    // what signals does a write request need?
    val wr_rd = Input(UInt(5.W))  
    val wr_d  = Input(UInt(32.W))  
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

class IF (BinaryFile: String, BTB: BTB) extends Module {
  val io = IO(new Bundle {
    // What inputs and / or outputs does this pipeline stage need?
    val instrOut = Output(UInt(32.W))
    val PCOut = Output(UInt(32.W))
    val valid = Input(UInt(1.W))
    val target = Input(UInt(32.W))
    val predictTaken = Input(UInt(1.W))
    val branch = Output(Bool())
  })

  val IMem = Mem(4096, UInt(32.W))
  loadMemoryFromFile(IMem, BinaryFile)

  // In this Project, we consider branches and jumps, differently from previous tasks
  val PC = RegInit(0.U(32.W))

  val instr = IMem(PC >> 2) //suspicious
  //testing if it's a branch/jump or not
  val opcode := instr(6,0)
  when(opcode === "b1100011"){
    branch = true.B
  }

  // PC update (using the BTB)
  when (io.valid && io.predictTaken) {
    PC := io.target // we use the predicted target if the branch is taken
  } .otherwise {
    PC := PC + 4.U // as seen in the previous tasks: default
  }

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
    val branch_offset = Output(UInt(32.W))
  })

  val opcode = io.instrIn(6, 0)
  val rd = io.instrIn(11,7)
  val funct3 = io.instrIn(14,12)
  val rs1 = io.instrIn(19,15)
  val rs2 = io.instrIn(24,20)
  val funct7 = io.instrIn(31,25)
  val imm_value = io.instrIn(31,20)
  //for the offset 
  val imm_11= io.instrIn(7) 
  val imm_4_1 = io.instrIn(8,11)
  val imm_10_5 = io.instrIn(25,30)
  val imm_12= io.instrIn(31)

  /* Determine the uop based on the disassembled instruction*/

   when(opcode === "b0110011".U){//R-type instr
    when(funct7 === "b0000000".U){ //most r-type instr
     when(funct3 === "b000"){
      io.microOP := uopc.isADD
     }.elsewhen(funct3 === "b011"){
      io.microOP := uopc.isSLTU
     }.elsewhen(funct3 === "b111"){
      io.microOP := uopc.isAND
     }.elsewhen(funct3 === "b110"){
      io.microOP := uopc.isOR
     }.elsewhen(funct3 === "b100"){
      io.microOP := uopc.isXOR
     }.elsewhen(funct3 === "b001"){
      io.microOP := uopc.isSLL
     }.elsewhen(funct3 === "b101"){
      io.microOP := uopc.isSRL
     }
    }.elsewhen(funct7 === "b0100000".U){ //for sub and sra
    when(funct3 === "b000"){
      io.microOP := uopc.isSUB
     }.elsewhen(funct3 === "b101"){
      io.microOP := uopc.isSRA
     }
    }
   }.elsewhen(opcode === "b0010011".U){ //I-type
      when(funct3 === "b000".U){ //add imm
        io.microOP := uopc.isADDI
      }
   }.elsewhen(opcode === "b1101111".U){ //jump 
      io.microOP := uopc.isJAL
   }.elsewhen(opcode === "b1100111".U){ //jr
      when(funct3 === "b000".U){
        io.microOP := uopc.isJALR
      }
   }.elsewhen(opcode === "b1100011".U){ //branch
      when(funct3 === "b000"){
      io.microOP := uopc.isBEQ
     }.elsewhen(funct3 === "b001"){
      io.microOP := uopc.isBNE
     }.elsewhen(funct3 === "b100"){
      io.microOP := uopc.isBLT
     }.elsewhen(funct3 === "b101"){
      io.microOP := uopc.isBGE
     }
   }otherwise{
    io.microOP := uopc.invalid
   }


  io.operandA_out := rs1
  io.operandB_out := rs2
  io.imm := imm_value
  io.rdOUt := rd
  //concatenation + sign extension
  io.branch_offset = Cat(Fill(20, io.imm12), Cat(imm_12, imm_11,imm_10_5,imm_4_1))
  
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
    val branch_offset = Output(UInt(32.W))

    // Outputs to update BTB
    val update = Output(UInt(1.W))
    val updatePC = Output(UInt(32.W))
    val updateTarget = Output(UInt(32.W))
    val mispredicted = Output(UInt(1.W))
  })

  val branchTaken = Wire(UInt(1.W))
  branchTaken := 0.UInt

  // operations and branch evaluation
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
      io.aluResult := (io.operandA.asSInt >> io.operandB.asUInt).asUInt // whatever comes next might not be correct
    }.elsewhen(io.microOP === isBEQ) {
      branchTaken := io.operandA === io.operandB
    }.elsewhen(io.microOP === isBNE) {
      branchTaken := io.operandA =/= io.operandB
    }.elsewhen(io.microOP === isBLT) {
      branchTaken := io.operandA.asSInt < io.operandB.asSInt
    }.elsewhen(io.microOP === isBGE) {
      branchTaken := io.operandA.asSInt >= io.operandB.asSInt
    }.otherwise{
    io.aluResult := 0.U // not sure
    }

    // Branch target computation
  val branchTarget = (io.PC.asSInt + io.branch_offset.asSInt).asUInt

  // Check of the BTB prediction
  mispredicted = (branchTaken =/= io.predictTaken) || (branchTaken && (branchTarget =/= io.predictTarget))

  //update BTB on misprediction
  io.update := mispredicted
  io.updatePC := io.PC //might be + offset?
  io.updateTarget := branchTarget
  io.mispredicted := mispredicted

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
    val res = Input(UInt(32.W))
    val rd  = Input(UInt(5.W))
    val addr = Output(UInt(5.W))
    val data = Output(UInt(32.W))

  })

  io.addr := io.rd
  io.data := io.res
}


// -----------------------------------------
// IF-Barrier
// -----------------------------------------

class IFBarrier extends Module {
  val io = IO(new Bundle {
  })

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
    val branchOffset_in = Input(32.W)

    val operandA_out = Output(UInt(32.W))
    val operandB_out = Output(UInt(32.W))
    val microOP_out  = Output(uopc())
    val imm_out      = Output(UInt(12.W))
    val rd_out       = Output(UInt(5.W))
    val branchOffset_out = Output(32.W)

  })

  val operandA_reg = Reg(UInt(32.W))
  val operandB_reg = Reg(UInt(32.W))
  val microOP_reg  = Reg(uopc())
  val imm_reg      = Reg(UInt(12.W))
  val rd_reg       = Reg(UInt(5.W))
  val branchOffset_reg = Reg(UInt(32.W))

  /* TODO: Fill registers from the inputs and write regioster values to the outputs */

  operandA_reg := io.operandA_in
  operandB_reg := io.operandB_in
  microOP_reg  := io.microOP_in
  imm_reg      := io.imm_in
  rd_reg       := io.rd_in
  branchOffset_reg := io.branchOffset_in
 
  io.operandA_out := operandA_reg
  io.operandB_out := operandB_reg
  io.microOP_out  := microOP_reg 
  io.imm_out      := imm_reg
  io.rd_out       := rd_reg
  io.branchOffset_out : branchOffset_reg
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

  val data_reg = Reg(UInt(32.W))
  val rd_reg   = Reg(UInt(5.W))
  
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

  val reg_data = Reg(UInt(32.W))
  val reg_addr = Reg(UInt(5.W))
  
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
 
  val btb = Module(new BranchTargetBuffer())


  val if_id_bar  = Module(new IDBarrier)
  val id_ex_bar  = Module(new EXBarrier)
  val ex_mem_bar = Module(new MEMBarrier)
  val mem_wb_bar = Module(new WBBarrier)


  val if_stage  = Module(new IF(BinaryFile))
  val id_stage  = Module(new ID)
  val ex_stage  = Module(new EX)
  val mem_stage = Module(new MEM)
  val wb_stage  = Module(new WB)

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
  id_ex_bar.io.branchOffset_in := id_stage.io.branch_offset

  // getting ex stage inputs from id/ex barrier outputs
  ex_stage.io.operandA   := id_ex_bar.io.operandA_out
  ex_stage.io.operandB   := id_ex_bar.io.operandB_out
  ex_stage.io.microOP    := id_ex_bar.io.microOP_out
  ex_stage.io.imm        := id_ex_bar.io.imm_out
  ex_stage.io.rdIn       := id_ex_bar.io.rd_out
  ex_stage.io.branch_offset := id_ex_bar.io.branchOffset_out

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

  //connecting i/o of btb
  btb.io.PC := if_stage.io.PCOut
  btb.io.branch := id_stage.io.branch
  btb.io.update := ex_stage.io.update
  btb.io.updatePC := ex_stage.io.updatePC
  btb.io.updateTarget := ex_stage.io.updateTarget
  btb.io.mispredicted := ex_stage.io.mispredicted

  io.check_res := wb_stage.io.data
}

