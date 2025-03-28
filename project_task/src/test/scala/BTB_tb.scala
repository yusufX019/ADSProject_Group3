// ADS I Class Project
// Pipelined RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 12/19/2023 by Tobias Jauch (@tojauch)

package BTB_Tester

import chisel3._
import chiseltest._
import core_tile._
import org.scalatest.flatspec.AnyFlatSpec

class BTBTest extends AnyFlatSpec with ChiselScalatestTester {


"BTB_Tester" should "work" in {
    test(new BranchTargetBuffer).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      /*dut.io.a.poke(...)
           *dut.io.b.poke(...)
           *dut.io.ci.poke(...)
           *dut.io.s.expect(...)
           *dut.io.co.expect(...)
           *...
           *TODO: Insert your test cases
           */

      // Initial reset
    dut.io.PC.poke(0.U)
    dut.io.update.poke(0.U)
    dut.io.mispredicted.poke(0.U)
    dut.io.updatePC.poke(0.U)
    dut.io.updateTarget.poke(0.U)
    dut.clock.step()

    // --- Insert first entry ---
    val testPC = 0x00000010.U
    val testTag = testPC(31,5)
    val testIndex = testPC(4,2)
    val testTarget = 0x00000080.U

    dut.io.update.poke(1.U)
    dut.io.updatePC.poke(testPC)
    dut.io.updateTarget.poke(testTarget)
    dut.io.mispredicted.poke(1.U) 
    dut.clock.step()

    // Verify first entry
    dut.io.PC.poke(testPC)
    dut.clock.step()
    dut.io.valid.expect(1.U)
    dut.io.target.expect(testTarget)

    // --- Insert second entry at the same index (to test collisions) ---
    val conflictPC = 0x00000018.U  // Different tag, same index
    val conflictTag = conflictPC(31,5)
    val conflictTarget = 0x00000090.U

    dut.io.update.poke(1.U)
    dut.io.updatePC.poke(conflictPC)
    dut.io.updateTarget.poke(conflictTarget)
    dut.clock.step()

    // Verify the second entry
    dut.io.PC.poke(conflictPC)
    dut.clock.step()
    dut.io.valid.expect(1.U)
    dut.io.target.expect(conflictTarget)

    // Check if first entry is still present (depends on replacement policy)
    dut.io.PC.poke(testPC)
    dut.clock.step()
    dut.io.valid.expect(1.U) // May fail if LRU evicted it
    dut.io.target.expect(testTarget)

    // --- Insert a new entry to force eviction (if table is full) ---
    val evictPC = 0x00002010.U
    val evictTag = evictPC(31,5)
    val evictTarget = 0x00003080.U

    dut.io.update.poke(1.U)
    dut.io.updatePC.poke(evictPC)
    dut.io.updateTarget.poke(evictTarget)
    dut.clock.step()

    // Verify eviction logic
    dut.io.PC.poke(evictPC)
    dut.clock.step()
    dut.io.valid.expect(1.U)
    dut.io.target.expect(evictTarget)


    // Test whether oldest entry was evicted
    dut.io.PC.poke(testPC)
    dut.clock.step()
    dut.io.valid.expect(1.U) // Should fail if evicted due to LRU

    // --- Simulate misprediction and state transition ---
    val mispredictPC = 0x00000010.U
    dut.io.PC.poke(mispredictPC)
    dut.io.mispredicted.poke(1.U)
    dut.clock.step()

    // Check if state transitioned (Strong Taken → Weak Taken)
    dut.io.predictTaken.expect(true.B) // If FSM transitioned correctly

/*
    // --- Insert and check a boundary case (last index) ---
    val boundaryPC = 0xFFFFF010.U  // Edge case: near max address
    val boundaryTarget = 0xFFFFF080.U

    dut.io.update.poke(1.U)
    dut.io.updatePC.poke(boundaryPC)
    dut.io.updateTarget.poke(boundaryTarget)
    dut.clock.step()

    // Verify last entry
    dut.io.PC.poke(boundaryPC)
    dut.clock.step()
    dut.io.valid.expect(1.U)
    dut.io.target.expect(boundaryTarget)
*/

    }
  }
}


