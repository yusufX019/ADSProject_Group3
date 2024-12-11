// ADS I Class Project
// Chisel Introduction
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 18/10/2022 by Tobias Jauch (@tojauch)

package adder

import chisel3._
import chisel3.util._


/** 
  * Half Adder Class 
  * 
  * Your task is to implement a basic half adder as presented in the lecture.
  * Each signal should only be one bit wide (inputs and outputs).
  * There should be no delay between input and output signals, we want to have
  * a combinational behaviour of the component.
  */
class HalfAdder extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a half adder as presented in the lecture
     */

    val A = Input(UInt(1.W))
    val B = Input(UInt(1.W))
    val S = Output(UInt(1.W))
    val C = Output(UInt(1.W))

    })

  /* 
   * TODO: Describe output behaviour based on the input values
   */

    val signalXor = Wire(UInt(1.W))
    val signalAnd = Wire(UInt(1.W))

    signalXor := io.A ^ io.B
    signalAnd := io.A & io.B

    io.S := signalXor
    io.C := signalAnd

}

/** 
  * Full Adder Class 
  * 
  * Your task is to implement a basic full adder. The component's behaviour should 
  * match the characteristics presented in the lecture. In addition, you are only allowed 
  * to use two half adders (use the class that you already implemented) and basic logic 
  * operators (AND, OR, ...).
  * Each signal should only be one bit wide (inputs and outputs).
  * There should be no delay between input and output signals, we want to have
  * a combinational behaviour of the component.
  */
class FullAdder extends Module{

  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a half adder as presented in the lecture
     */
  
    val A    = Input(UInt(1.W))
    val B    = Input(UInt(1.W))
    val Cin  = Input(UInt(1.W))
    val S    = Output(UInt(1.W))
    val Cout = Output(UInt(1.W))

    })


  /* 
   * TODO: Instanciate the two half adders you want to use based on your HalfAdder class
   */

  
  val halfAdder1 = Module(new HalfAdder())
  val halfAdder2 = Module(new HalfAdder())

  halfAdder1.io.A := io.A
  halfAdder1.io.B := io.B
  halfAdder2.io.A := io.Cin
  halfAdder2.io.B := halfAdder1.io.S

  val signalS  = halfAdder2.io.S
  val signalOr = halfAdder2.io.C | halfAdder1.io.C


  /* 
   * TODO: Describe output behaviour based on the input values and the internal signals
   */

  io.S    := signalS
  io.Cout := signalOr

}

/** 
  * 4-bit Adder class 
  * 
  * Your task is to implement a 4-bit ripple-carry-adder. The component's behaviour should 
  * match the characteristics presented in the lecture.  Remember: An n-bit adder can be 
  * build using one half adder and n-1 full adders.
  * The inputs and the result should all be 4-bit wide, the carry-out only needs one bit.
  * There should be no delay between input and output signals, we want to have
  * a combinational behaviour of the component.
  */
class FourBitAdder extends Module{

  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a 4-bit ripple-carry-adder as presented in the lecture
     */
    
    val A = Input(UInt(4.W))
    val B = Input(UInt(4.W))
    val C = Output(UInt(1.W))
    val S = Output(UInt(4.W))


    })

  /* 
   * TODO: Instanciate the full adders and one half adderbased on the previously defined classes
   */
    val halfAdder  = Module(new HalfAdder())
    val fullAdder1 = Module(new FullAdder())
    val fullAdder2 = Module(new FullAdder())
    val fullAdder3 = Module(new FullAdder())

    halfAdder.io.A := io.A(0)
    halfAdder.io.B := io.B(0)

    fullAdder1.io.A   := io.A(1)
    fullAdder1.io.B   := io.B(1)
    fullAdder1.io.Cin := halfAdder.io.C

    fullAdder2.io.A   := io.A(2)
    fullAdder2.io.B   := io.B(2)
    fullAdder2.io.Cin := fullAdder1.io.Cout

    fullAdder3.io.A   := io.A(3)
    fullAdder3.io.B   := io.B(3)
    fullAdder3.io.Cin := fullAdder2.io.Cout

    val signalS = Cat(fullAdder3.io.S, fullAdder2.io.S, fullAdder1.io.S, halfAdder.io.S)
    val signalC = fullAdder3.io.Cout

    io.C := signalC
    io.S := signalS


  /* 
   * TODO: Describe output behaviour based on the input values and the internal 
   */
}
