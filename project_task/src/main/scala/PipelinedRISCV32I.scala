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
        val PC  = Input(UInt(32.W))
        val update  = Input(UInt(1.W))
        val updatePC  = Input(UInt(32.W))
        val updateTarget  = Input(UInt(32.W))
        val mispredicted  = Input(UInt(1.W))

        val valid = Output(UInt(1.W))
        val target = Output(UInt(32.W))
        val predictTaken = Output(UInt(1.W))
    })

  val core = Module(new PipelinedRV32Icore())

  io.updateTarget := core.io.updateTarget
  io.mispredicted := core.io.mispredicted
  io.PC           := core.io.PC
  io.update       := core.io.update
  io.updatePC     := core.io.updatePC

  io.valid          := core.io.valid
  io.target         := core.io.target
  io.predictTaken   := core.io.predictTaken

}

