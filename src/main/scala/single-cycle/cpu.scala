// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // All of the structures required
  val pc              = dontTouch(RegInit(0.U(64.W)))
  val control         = Module(new Control())
  val registers       = Module(new RegisterFile())
  val aluControl      = Module(new ALUControl())
  val alu             = Module(new ALU())
  val immGen          = Module(new ImmediateGenerator())
  val controlTransfer = Module(new ControlTransferUnit())
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  control.io := DontCare
  registers.io := DontCare
  aluControl.io := DontCare
  alu.io := DontCare
  immGen.io := DontCare
  controlTransfer.io := DontCare
  io.dmem <> DontCare

  //FETCH
  io.imem.address := pc
  io.imem.valid := true.B

  val instruction = Wire(UInt(32.W))
  when ((pc % 8.U) === 4.U) {
    instruction := io.imem.instruction(63, 32)
  } .otherwise {
    instruction := io.imem.instruction(31, 0)
  }

  // First, determine the value for aluop
  when(instruction(6, 0) === "b0110011".U){
    // means we have OP so 64-bit R-type
    // means aluop = 001
    aluControl.io.aluop := "b001".U
  }.elsewhen(instruction(6, 0) === "b0111011".U){
    // means we have OP-32 so 32-bit R-type
    // means aluop = 011
    aluControl.io.aluop := "b011".U
  }.otherwise {
    // NOT IMPLEMENTED other instruction types
    // So just make it some invalid value that'll raise an error later
    aluControl.io.aluop := "b111".U
  }

  // Next we need to get funct3 and funct7
  aluControl.io.funct3 := instruction(14, 12)
  aluControl.io.funct7 := instruction(31, 25)

  // We can now get the operation to the alu!
  alu.io.operation := aluControl.io.operation

  // get registers from instruction (rs1, rs2, rd)
  registers.io.readreg2 := instruction(24, 20)
  registers.io.readreg1 := instruction(19, 15)
  registers.io.writereg := instruction(11, 7)

  // set operands as output of register file
  alu.io.operand1 := registers.io.readdata1
  alu.io.operand2 := registers.io.readdata2

  when(instruction(11, 7) === "b00000".U) {
    printf("************************IT'S 0\n")
  }.otherwise{
    printf("************************rip\n")
  }
  
  // enable the write to registers (and actually write) ONLY if the writereg is NOT reg0
  // So apparently the cpu doesn't like it when I don't write *anything*, even if
  // the desired register is reg0 which isn't allowed to change.
  // So to circumvent this, I just used a when.otherwise statment. Whenever the 
  // destination register is r0, I still write to it... but I just write the value
  // 0, effectively not writing to it at all.

  registers.io.wen := true.B

  when(instruction(11, 7) === "b00000".U) {
    // if rd is reg0, write a 0 to it
    registers.io.writedata := 0.U(64.W)
  }.otherwise{
    // otherwise, write the result of the ALU to it
    registers.io.writedata := alu.io.result
  }


  // to be able to handle multiple instructions, increment pc!
  pc := pc + 4.U // 4-byte aligned instructions
  
}

/*
 * Object to make it easier to print information about the CPU
 */
object SingleCycleCPUInfo {
  def getModules(): List[String] = {
    List(
      "dmem",
      "imem",
      "control",
      "registers",
      "csr",
      "aluControl",
      "alu",
      "immGen",
      "controlTransfer"
    )
  }
}
