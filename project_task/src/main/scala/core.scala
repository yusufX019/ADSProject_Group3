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
  val LRU_counter = UInt(1.W) // if 0 then way 0 was most recently used, if 1 then way 1 most recenlty used
}

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
    val mispredicted  = Input(UInt(1.W)) //1= miss 0= good pred
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

  when(io.branch){ 
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
    val currentSet = btb(index) 
    when(currentSet(0).tag === tag){ //get the 1st entry
      when(currentSet(0).valid){
        //predict taken
        io.target = currentSet(0).target_address
        //change prediction
        currentSet.LRU_counter = 1.U
      }
    }.elsewhen(currentSet(1).tag === tag){//get 2nd entry
      when(currentSet(1).valid){
        //predict taken
         io.target = currentSet(1).target_address
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
    val currentSet := btb(index)
    when(currentSet(0).valid ==== 0.U || (currentSet.LRU_counter === 1.U && currentSet(0).valid ==== 1.U && currentSet(1).valid ==== 1.U)){
      currentSet(0).valid = 1.U
      currentSet(0).tag = io.updatePC(31,5)
      currentSet(0).target_address = io.updateTarget
      currentSet.LRU_counter = 1.U
      //update counter
      when((io.mispredicted && io.predictTaken) || (~io.predictTaken && io.mispredicted)){
        currentSet(0).prediction = currentSet(0).prediction - 1.U
      }elswhen(~io.mispredicted && ~io.predictTaken || (io.predictTaken && ~io.mispredicted)){
        currentSet(0).prediction = currentSet(0).prediction + 1.U
      } //link it to FSM
    }.elsewhen(currentSet(1).valid ==== 0.U || (currentSet.LRU_counter === 0.U && currentSet(0).valid ==== 1.U && currentSet(1).valid ==== 1.U)){
      currentSet(1).valid = 1.U
      currentSet(1).tag = io.updatePC(31,5)
      currentSet(1).target_address = io.updateTarget
      currentSet.LRU_counter = 0.U
      //update counter
      when((io.mispredicted && io.predictTaken) || (~io.predictTaken && io.mispredicted)){
        currentSet(1).prediction = currentSet(1).prediction - 1.U
      }elswhen(~io.mispredicted && ~io.predictTaken || (io.predictTaken && ~io.mispredicted)){
        currentSet(1).prediction = currentSet(1).prediction + 1.U
      } //link it to FSM
    }
  }

  // Output Connections
  io.valid := hit
  io.target := Mux(hit, btb(index).ways(waySel).target_address, 0.U)
  io.predictTaken := Mux(hit, predictedTaken, false.B)
}


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

class PipelinedRV32Icore (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val check_res = Output(UInt(32.W))
  })

  val regFile = Module(new regFile)
  regFile.io.write.wr_writeEnable := 0.U //disabling write enable
 
  val btb = Module(new BranchTargetBuffer())


  /* 
   * TODO: Connect all IOs between the stages, barriers and register file.
   * Do not forget the global output of the core module
   */


  regFile.io.req.rr_rs1 := id_stage.io.operandA_out
  regFile.io.req.rr_rs2 := id_stage.io.operandB_out

  // getting ex stage inputs from id/ex barrier outputs
  ex_stage.io.operandA   := id_ex_bar.io.operandA_out
  ex_stage.io.operandB   := id_ex_bar.io.operandB_out
  ex_stage.io.microOP    := id_ex_bar.io.microOP_out
  ex_stage.io.imm        := id_ex_bar.io.imm_out
  ex_stage.io.rdIn       := id_ex_bar.io.rd_out
  ex_stage.io.branch_offset := id_ex_bar.io.branchOffset_out

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

