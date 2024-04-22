// This file contains ALU control logic.

package dinocpu.components

import chisel3._
import chisel3.util._

/**
 * The ALU control unit
 *
 * Input:  aluop        Specifying the type of instruction using ALU
 *                          . 0 for none of the below = 000
 *                          . 1 for 64-bit R-type = 001
 *                          . 2 for 64-bit I-type = 010
 *                          . 3 for 32-bit R-type = 011
 *                          . 4 for 32-bit I-type = 100
 *                          . 5 for non-arithmetic instruction types that uses ALU (auipc/jal/jarl/Load/Store) = 101
 * Input:  funct7       The most significant bits of the instruction.
 * Input:  funct3       The middle three bits of the instruction (12-14).
 *
 * Output: operation    What we want the ALU to do.
 *
 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.
 * This is loosely based on figure 4.12
 */
class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluop     = Input(UInt(3.W))
    val funct7    = Input(UInt(7.W))
    val funct3    = Input(UInt(3.W))

    val operation = Output(UInt(5.W))
  })

  io.operation := "b11111".U // Invalid

  // NOTE: Since we only have to worry about R type instructions for this part,
  // we only really need implementation for 
  // aluop = 001 (64-bit R-type) or
  // aluop = 011 (word-sized R-type)
  // IN ANY OTHER CASE, the operation shall REMAIN INVALID

  when(io.aluop === "b001".U && io.funct3 === "b000".U && io.funct7 === "b0000000".U) {
    // 64-bit ADD instruction
    io.operation := "b00001".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b000".U && io.funct7 === "b0000001".U) {
    // 64-bit MUL instruction
    io.operation := "b00110".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b000".U && io.funct7 === "b0100000".U) {
    // 64-bit SUB instruction
    io.operation := "b00100".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b001".U && io.funct7 === "b0000000".U) {
    // 64-bit SLL instruction
    io.operation := "b10010".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b001".U && io.funct7 === "b0000001".U) {
    // 64-bit MULH instruction
    io.operation := "b00111".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b010".U && io.funct7 === "b0000000".U) {
    // 64-bit SLT instruction
    io.operation := "b10110".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b010".U && io.funct7 === "b0000001".U) {
    // 64-bit MULHSU instruction
    io.operation := "b11000".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b011".U && io.funct7 === "b0000000".U) {
    // 64-bit SLTU instruction
    io.operation := "b10111".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b011".U && io.funct7 === "b0000001".U) {
    // 64-bit MULHU instruction
    io.operation := "b01000".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b100".U && io.funct7 === "b0000000".U) {
    // 64-bit XOR instruction
    io.operation := "b01111".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b100".U && io.funct7 === "b0000001".U) {
    // 64-bit DIV instruction
    io.operation := "b01011".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b101".U && io.funct7 === "b0000000".U) {
    // 64-bit SRL instruction
    io.operation := "b10100".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b101".U && io.funct7 === "b0000001".U) {
    // 64-bit DIVU instruction
    io.operation := "b01010".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b101".U && io.funct7 === "b0100000".U) {
    // 64-bit SRA instruction
    io.operation := "b10000".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b110".U && io.funct7 === "b0000000".U) {
    // 64-bit OR instruction
    io.operation := "b01110".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b110".U && io.funct7 === "b0000001".U) {
    // 64-bit REM instruction
    io.operation := "b11100".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b111".U && io.funct7 === "b0000000".U) {
    // 64-bit AND instruction
    io.operation := "b01101".U
  }.elsewhen(io.aluop === "b001".U && io.funct3 === "b111".U && io.funct7 === "b0000001".U) {
    // 64-bit REMU instruction
    io.operation := "b11011".U
  }.elsewhen(io.aluop === "b011".U && io.funct3 === "b000".U && io.funct7 === "b0000000".U) {
    // 32-bit ADDW instruction
    io.operation := "b00000".U
  }.elsewhen(io.aluop === "b011".U && io.funct3 === "b000".U && io.funct7 === "b0000001".U) {
    // 32-bit MULW instruction
    io.operation := "b00101".U
  }.elsewhen(io.aluop === "b011".U && io.funct3 === "b000".U && io.funct7 === "b0100000".U) {
    // 32-bit SUBW instruction
    io.operation := "b00010".U
  }.elsewhen(io.aluop === "b011".U && io.funct3 === "b001".U && io.funct7 === "b0000000".U) {
    // 32-bit SLLW instruction
    io.operation := "b10011".U
  }.elsewhen(io.aluop === "b011".U && io.funct3 === "b100".U && io.funct7 === "b0000001".U) {
    // 32-bit DIVW instruction
    io.operation := "b01001".U
  }.elsewhen(io.aluop === "b011".U && io.funct3 === "b101".U && io.funct7 === "b0000000".U) {
    // 32-bit SRLW instruction
    io.operation := "b10101".U
  }.elsewhen(io.aluop === "b011".U && io.funct3 === "b101".U && io.funct7 === "b0000001".U) {
    // 32-bit DIVUW instruction
    io.operation := "b01100".U
  }.elsewhen(io.aluop === "b011".U && io.funct3 === "b101".U && io.funct7 === "b0100000".U) {
    // 32-bit SRAW instruction
    io.operation := "b10001".U
  }.elsewhen(io.aluop === "b011".U && io.funct3 === "b110".U && io.funct7 === "b0000001".U) {
    // 32-bit REMW instruction
    io.operation := "b11010".U
  }.elsewhen(io.aluop === "b011".U && io.funct3 === "b111".U && io.funct7 === "b0000001".U) {
    // 32-bit REMUW instruction
    io.operation := "b11001".U
  }.otherwise{
    // invalid operation was entered
    io.operation := "b11111".U 
  }
}
