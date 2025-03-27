// ADS I Class Project
// Single-Cycle RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 05/10/2023 by Tobias Jauch (@tojauch)

package PipelinedRV32I

import chisel3._
import chisel3.util._

import core_tile._

class PipelinedRV32I extends Module {

    val io = IO(new Bundle {
        val updateTarget  = Input(UInt(32.W))
        val mispredicted  = Input(UInt(1.W)) 
    })

  val core = Module(new PipelinedRV32Icore())

  io.updateTarget := core.btb.io.updateTarget
  io.mispredicted := core.btb.io.mispredicted
}

