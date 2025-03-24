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
  val prediction = UInt(2.W)
  //val prediction = StateBranchTargetBuffer.State() // 2-bit FSM (see line 30) why ? why not numbers?
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
  val way0Match = btb(index).ways(0).valid & (btb(index).ways(0).tag === tag)
  val way1Match = btb(index).ways(1).valid & (btb(index).ways(1).tag === tag)
  val hit = way0Match | way1Match

  // Select the correct way if there is a match
  val waySel = Mux((way0Match).asBool, 0.U, 1.U)

  // Get the predictor state
  val predState = btb(index).ways(waySel).prediction
  val predictedTaken = predState >= 1.U // StateBranchTargetBuffer.State.WeakNotTaken

  //all of this happens after IF

    //checking if target address is in btb
    val currentSet = btb(index) 

    when(currentSet.ways(0).tag === tag){ //get the 1st entry
      when(currentSet.ways(0).valid === 1.U){
        //predict taken
        io.target := currentSet.ways(0).target_address
        //change prediction
        currentSet.LRU_counter := 1.U
      }
    }.elsewhen(currentSet.ways(1).tag === tag){//get 2nd entry
      when(currentSet.ways(1).valid === 1.U){
        //predict taken
         io.target := currentSet.ways(1).target_address
        //change prediction 
        currentSet.LRU_counter := 0.U
      }
    }.otherwise{
      //predict not taken
      io.target := io.PC + 4.U
    }


  // all this happens after EX stage
 
  /* State Machine Transition Logic (following the scheme sl.6-47) */
  
  val stateBtb = State()
  //starting state
  stateBtb := State.WeakNotTaken 

    switch(stateBtb){
      is(State.StrongNotTaken){ //00
        when(io.mispredicted === 1.U){
          stateBtb := State.WeakNotTaken
        }.otherwise{
          stateBtb := State.StrongNotTaken
        }
      }
      is(State.WeakNotTaken){ //01
        when(io.mispredicted=== 1.U){
          stateBtb := State.WeakTaken
        }.otherwise{
          stateBtb := State.StrongNotTaken
        }
      }
      is(State.StrongTaken){ //10
        when(io.mispredicted=== 1.U){
          stateBtb := State.WeakTaken
        }.otherwise{
          stateBtb := State.StrongTaken
        }
      }
      is(State.WeakTaken){ //11
        when(io.mispredicted=== 1.U){
          stateBtb := State.StrongNotTaken
        }.otherwise{
          stateBtb := State.StrongTaken
        }
      }
    }


  //writing new entry + eviction


  val SetFullAndLRU1 = (currentSet.LRU_counter === 1.U) & (currentSet.ways(0).valid === 1.U) & (currentSet.ways(1).valid === 1.U)

  when((io.update & io.mispredicted) === 1.U){ 
    when(currentSet.ways(0).valid === 0.U | SetFullAndLRU1){
      currentSet.ways(0).valid := 1.U
      currentSet.ways(0).tag := io.updatePC(31,5)
      currentSet.ways(0).target_address := io.updateTarget
      currentSet.LRU_counter := 1.U
      //update counter
      when(((io.mispredicted & io.predictTaken) | (~io.predictTaken & io.mispredicted)) === 1.U){
        currentSet.ways(0).prediction := currentSet.ways(0).prediction - 1.U
      }.elsewhen((~io.mispredicted & ~io.predictTaken | (io.predictTaken & ~io.mispredicted)) === 1.U){
        currentSet.ways(0).prediction := currentSet.ways(0).prediction + 1.U
      }
    }.elsewhen(currentSet.ways(1).valid === 0.U | currentSet.LRU_counter === 0.U & currentSet.ways(0).valid === 1.U & currentSet.ways(1).valid === 1.U){
      currentSet.ways(1).valid := 1.U
      currentSet.ways(1).tag := io.updatePC(31,5)
      currentSet.ways(1).target_address := io.updateTarget
      currentSet.LRU_counter := 0.U
      //update counter
      when(((io.mispredicted & io.predictTaken) | (~io.predictTaken & io.mispredicted)) === 1.U){
        currentSet.ways(1).prediction := currentSet.ways(1).prediction - 1.U
      }.elsewhen((~io.mispredicted & ~io.predictTaken | (io.predictTaken & ~io.mispredicted)) === 1.U){
        currentSet.ways(1).prediction := currentSet.ways(1).prediction + 1.U
      } 
    }
  }

  // Output Connections
  io.valid := hit
  io.target := Mux((hit).asBool, btb(index).ways(waySel).target_address, 0.U)
  io.predictTaken := Mux((hit).asBool, predictedTaken, false.B)
}


class PipelinedRV32Icore (BinaryFile: String) extends Module {

  val btb = Module(new BranchTargetBuffer())
}

