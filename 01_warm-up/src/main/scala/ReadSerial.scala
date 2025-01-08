// ADS I Class Project
// Chisel Introduction
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 18/10/2022 by Tobias Jauch (@tojauch)

package readserial

import chisel3._
import chisel3.util._
import chisel3.stage._
import chisel3.experimental.ChiselEnum
import adder.FourBitAdder


object StateController {
  object State extends ChiselEnum {
    val Idle, Reading, Finish = Value
  }
}

/** controller class */
class Controller extends Module{
  import StateController.State
  import StateController.State._

  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a the component as stated in the documentation
     */

    val rxd     = Input(UInt(1.W))
    //val reset_n = Input(0.U(1.W))
    val cnt_s   = Input(UInt(1.W))
    val cnt_en  = Output(UInt(1.W))
    val valid   = Output(UInt(1.W))
    val state   = Output(State())
    })

  // internal variables
  /* 
   * TODO: Define internal variables (registers and/or wires), if needed
   */
   val signalValid = Wire(UInt(1.W))
   val signalCntEn = Wire(UInt(1.W))

   signalValid := 0.U
   signalCntEn := 0.U 

   val state = RegInit(Idle)
   io.state := state
  

  // state machine
  /* 
   * TODO: Describe functionality if the controller as a state machine
   */

   switch(state) {
    is(Idle) {
      when(io.rxd === 0.U) {
        state := Reading
        signalCntEn := 1.U
      } .otherwise {
        state := Idle
      }
    }
    is(Reading) {
      signalCntEn := 1.U
      when(io.cnt_s === 1.U){
        signalCntEn := 0.U
        state := Finish
      }
    }
    is(Finish) {
      signalValid := 1.U
      state := Idle
    }
  }

  io.cnt_en := signalCntEn
  io.valid  := signalValid

}

object StateCounter {
  object State extends ChiselEnum {
    val Idle, Counting = Value
  }
}


/** counter class */
class Counter extends Module{
  import StateCounter.State
  import StateCounter.State._ 

  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a the component as stated in the documentation
     */
     //val reset_n = Input(UInt(1.W))
     val cnt_en  = Input(UInt(1.W))
     val cnt_s   = Output(UInt(1.W))

    })

  // internal variables
  /* 
   * TODO: Define internal variables (registers and/or wires), if needed
   */
    val signalCntS = Wire(UInt(1.W))
    val regCntValue = RegInit(0.U(3.W)) // counting 8 times
    signalCntS := 0.U
    io.cnt_s := signalCntS 

  // state machine
  /* 
   * TODO: Describe functionality if the counter as a state machine
   */
   val state = RegInit(StateCounter.State.Idle)

   switch(state){
    is(Idle){
      regCntValue := 0.U
      signalCntS  := 0.U
      when(io.cnt_en === 1.U){
        state := State.Counting
      } .otherwise{
        state := State.Idle
      }
    }
    is(Counting){
      when(regCntValue < 8.U){ 
        regCntValue + 1.U
        state := State.Counting
      }
      .otherwise{
        signalCntS := 1.U
        state := State.Idle
      }
    }
   }
}

/** shift register class */
class ShiftRegister extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a the component as stated in the documentation
     */
     val rxd  = Input(UInt(1.W))
     val data = Output(UInt(8.W))
    })

  // internal variables
  /* 
   * TODO: Define internal variables (registers and/or wires), if needed
   */
    val regData = RegInit(0.U(8.W))

    io.data := regData

  // functionality
  /* 
   * TODO: Describe functionality if the shift register
   */

   regData := Cat(regData(6, 0), io.rxd)
}

/** 
  * The last warm-up task deals with a more complex component. Your goal is to design a serial receiver.
  * It scans an input line (“serial bus”) named rxd for serial transmissions of data bytes. A transmission 
  * begins with a start bit ‘0’ followed by 8 data bits. The most significant bit (MSB) is transmitted first. 
  * There is no parity bit and no stop bit. After the last data bit has been transferred a new transmission 
  * (beginning with a start bit, ‘0’) may immediately follow. If there is no new transmission the bus line 
  * goes high (‘1’, this is considered the “idle” bus signal). In this case the receiver waits until the next 
  * transmission begins. The outputs of the design are an 8-bit parallel data signal and a valid signal. 
  * The valid signal goes high (‘1’) for one clock cycle after the last serial bit has been transmitted, 
  * indicating that a new data byte is ready.
  */
class ReadSerial extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a the component as stated in the documentation
     */
     //val reset_n = Input(UInt(1.W))
     val rxd     = Input(UInt(1.W))
     val data    = Output(UInt(8.W))
     val valid   = Output(UInt(1.W))  
    })


  // instanciation of modules
  /* 
   * TODO: Instanciate the modules that you need
   */

   val controller    = Module(new Controller())
   val counter       = Module(new Counter())
   val shiftRegister = Module(new ShiftRegister())

  // connections between modules
  /* 
   * TODO: connect the signals between the modules
   */

   controller.io.rxd     := io.rxd
   //controller.io.reset_n := io.reset_n

   counter.io.cnt_en    := controller.io.cnt_en
   controller.io.cnt_s  := counter.io.cnt_s

   shiftRegister.io.rxd := io.rxd


  // global I/O 
  /* 
   * TODO: Describe output behaviour based on the input values and the internal signals
   */

   io.data  := shiftRegister.io.data
   io.valid := controller.io.valid

}
