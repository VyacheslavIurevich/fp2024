(** Copyright 2024, Vyacheslav Kochergin and Roman Mukovenkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Integer Registers *)
type int_register =
  | X0 (** zero - Zero Constant *)
  | X1 (** ra - Return Address *)
  | X2 (** sp - Stack Pointer *)
  | X3 (** gp - Global Pointer *)
  | X4 (** tp - Thread Pointer *)
  | X5 (** t0 - Temporary *)
  | X6 (** t1 - Temporary *)
  | X7 (** t2 - Temporary *)
  | X8 (** s0/fp - Saved/Frame Pointer *)
  | X9 (** s1 - Saved Register *)
  | X10 (** a0 - Function Argument or Return Value *)
  | X11 (** a1 - Function Argument or Return Value *)
  | X12 (** a2 - Function Argument *)
  | X13 (** a3 - Function Argument *)
  | X14 (** a4 - Function Argument *)
  | X15 (** a5 - Function Argument *)
  | X16 (** a6 - Function Argument *)
  | X17 (** a7 - Function Argument *)
  | X18 (** s2 - Saved Register *)
  | X19 (** s3 - Saved Register *)
  | X20 (** s4 - Saved Register *)
  | X21 (** s5 - Saved Register *)
  | X22 (** s6 - Saved Register *)
  | X23 (** s7 - Saved Register *)
  | X24 (** s8 - Saved Register *)
  | X25 (** s9 - Saved Register *)
  | X26 (** s10 - Saved Register *)
  | X27 (** s11 - Saved Register *)
  | X28 (** t3 - Temporary *)
  | X29 (** t4 - Temporary *)
  | X30 (** t5 - Temporary *)
  | X31 (** t6 - Temporary *)
[@@deriving eq, show { with_path = false }]

(** Float Registers *)
type float_register =
  | F0 (** ft0 - Temporary *)
  | F1 (** ft1 - Temporary *)
  | F2 (** ft2 - Temporary *)
  | F3 (** ft3 - Temporary *)
  | F4 (** ft4 - Temporary *)
  | F5 (** ft5 - Temporary *)
  | F6 (** ft6 - Temporary *)
  | F7 (** ft7 - Temporary *)
  | F8 (** fs0 - Saved Register *)
  | F9 (** fs1 - Saved Register *)
  | F10 (** fa0 - Function Argument or Return Value *)
  | F11 (** fa1 - Function Argument or Return Value *)
  | F12 (** fa2 - Function Argument *)
  | F13 (** fa3 - Function Argument *)
  | F14 (** fa4 - Function Argument *)
  | F15 (** fa5 - Function Argument *)
  | F16 (** fa6 - Function Argument *)
  | F17 (** fa7 - Function Argument *)
  | F18 (** fs2 - Saved Register *)
  | F19 (** fs3 - Saved Register *)
  | F20 (** fs4 - Saved Register *)
  | F21 (** fs5 - Saved Register *)
  | F22 (** fs6 - Saved Register *)
  | F23 (** fs7 - Saved Register *)
  | F24 (** fs8 - Saved Register *)
  | F25 (** fs9 - Saved Register *)
  | F26 (** fs10 - Saved Register *)
  | F27 (** fs11 - Saved Register *)
  | F28 (** t8 - Temporary *)
  | F29 (** t9 - Temporary *)
  | F30 (** t10 - Temporary *)
  | F31 (** t11 - Temporary *)
[@@deriving eq, show { with_path = false }]

(** Immediate 12-bit Type *)
type immediate12 = int [@@deriving eq, show { with_path = false }]

(** Immediate 20-bit Type *)
type immediate20 = int [@@deriving eq, show { with_path = false }]

(** Immediate 32-bit Type*)
type immediate32 = int [@@deriving eq, show { with_path = false }]

(** Label Type *)
type label = string [@@deriving eq, show { with_path = false }]

(** Address12 Type to Jump to *)
type address12 =
  | Immediate12 of immediate12 (** Immediate12 to Jump to*)
  | Label of label (** Label to Jump to *)
[@@deriving eq, show { with_path = false }]

(** Address20 Type to Jump to *)
type address20 =
  | Immediate20 of immediate20 (** Immediate20 to Jump to*)
  | Label of label (** Label to Jump to *)
[@@deriving eq, show { with_path = false }]

type instruction =
  | Add of int_register * int_register * int_register (** Addition. rd = rs1 + rs2 *)
  | Sub of int_register * int_register * int_register (** Subtraction. rd = rs1 - rs2 *)
  | Xor of int_register * int_register * int_register (** Exclusive OR. rd = rs1 ^ rs2 *)
  | Or of int_register * int_register * int_register (** OR. rd = rs1 | rs2 *)
  | And of int_register * int_register * int_register (** AND. rd = rs1 & rs2 *)
  | Sll of int_register * int_register * int_register (** Shift Left Logical. rd = rs1 << rs2 *)
  | Srl of int_register * int_register * int_register (** Shift Right Logical. rd = rs1 >> rs2 *)
  | Sra of int_register * int_register * int_register (** Shift Right Arithmetic. rd = rs1 >> rs2 *)
  | Slt of int_register * int_register * int_register (** Set Less Than. rd = (rs1 < rs2) ? 1 : 0 *)
  | Sltu of int_register * int_register * int_register (** Set Less Than (Unsigned) *)
  | Addi of int_register * int_register * immediate12 (** Addition of Immediate. rd = rs1 + imm *)
  | Xori of int_register * int_register * immediate12 (** XOR with Immediate. rd = rs1 ^ imm *)
  | Ori of int_register * int_register * immediate12 (** OR with Immediate. rd = rs1 | imm *)
  | Andi of int_register * int_register * immediate12 (** AND with Immediate. rd = rs1 & imm *)
  | Slli of int_register * int_register * immediate12
  (** Shift Left Logical with Immediate. rd = rs1 << shamt[0:4] *)
  | Srli of int_register * int_register * immediate12
  (** Shift Right Logical with Immediate. rd = rs1 >> shamt[0:4] logical *)
  | Srai of int_register * int_register * immediate12
  (** Shift Right Arithmetic with Immediate. rd = rs1 >> shamt[0:4] arithmetical *)
  | Slti of int_register * int_register * immediate12
  (** Set Less Than Imm. rd = (rs1 < imm) ? 1 : 0 *)
  | Sltiu of int_register * int_register * immediate12 (** Set Less Than Imm (Unsigned) *)
  | Lb of int_register * int_register * immediate12 (** Load Byte. rd = M[rs1 + imm][0:7] *)
  | Lh of int_register * int_register * immediate12 (** Load Half. rd = M[rs1 + imm][0:15] *)
  | Lw of int_register * int_register * immediate12 (** Load Word. rd = M[rs1 + imm][0:31] *)
  | Lbu of int_register * int_register * immediate12 (** Load Byte Unsigned *)
  | Lhu of int_register * int_register * immediate12 (** Load Half Unsigned *)
  | Sb of int_register * immediate12 * int_register
  (** Store Byte. M[rs1 + imm][0:7] = rs2[0:7] *)
  | Sh of int_register * immediate12 * int_register
  (** Store Half. M[rs1 + imm][0:15] = rs2[0:15] *)
  | Sw of int_register * immediate12 * int_register
  (** Store Word. M[rs1 + imm][0:31] = rs2[0:31] *)
  | Beq of int_register * int_register * address12
  (** Branch ==. if (rs1 == rs2) PC += imm. PC is a program counter *)
  | Bne of int_register * int_register * address12 (** Branch !=. if (rs1 != rs2) PC += imm. *)
  | Blt of int_register * int_register * address12 (** Branch <. if (rs1 < rs2) PC += imm. *)
  | Bge of int_register * int_register * address12 (** Branch >=. if (rs1 >= rs2) PC += imm. *)
  | Bltu of int_register * int_register * address12
  (** Branch < (Unsigned). if (rs1 < rs2) PC += imm. *)
  | Bgeu of int_register * int_register * address12
  (** Branch >= (Unsigned). if (rs1 >= rs2) PC += imm. *)
  | Jal of int_register * address20
  (** Jump and Link. rd = PC + 4; PC += imm. 4 bytes = 32 bits - instuction size *)
  | Jalr of int_register * int_register * address12
  (** Jump and Link int_register. rd = PC + 4; PC = rs1 + imm *)
  | Lui of int_register * immediate20 (** Load Upper Immediate. rd = imm << 12 *)
  | Auipc of int_register * immediate20
  (** Add Upper Immediate to PC. rd = PC + (imm << 12) *)
  | Ecall (** EnvironmentCall - a syscall *)
  | Mul of int_register * int_register * int_register (** Multiply. rd = (rs1 * rs2)[31:0] *)
  | Mulh of int_register * int_register * int_register (** Multiply High. rd = (rs1 * rs2)[63:32] *)
  | Mulhsu of int_register * int_register * int_register
  (** Multiply High (Signed * Unsigned). rd = (rs1 * rs2)[63:32] *)
  | Mulhu of int_register * int_register * int_register
  (** Multiply High (Unsigned * Unsigned). rd = (rs1 * rs2)[63:32] *)
  | Div of int_register * int_register * int_register (** Division. rd = rs1 / rs2 *)
  | Divu of int_register * int_register * int_register (** Division (Unsigned). rd = rs1 / rs2 *)
  | Rem of int_register * int_register * int_register (** Remainder. rd = rs1 % rs2 *)
  | Remu of int_register * int_register * int_register (** Remainder (Unsigned). rd = rs1 % rs2 *)
  | Lwu of int_register * int_register * immediate12
  (** Load Word (Unsigned). rd = M[rs1 + imm][0:31] *)
  | Ld of int_register * int_register * immediate12
  (** Load Doubleword (Unsigned). rd = M[rs1 + imm][0:63] *)
  | Sd of int_register * int_register * immediate12
  (** Store Doubleword. M[rs1 + imm][0:63] = rs2[0:63] *)
  | Addiw of int_register * int_register * immediate12
  (** Addition of Immediate Word. rd = (rs1 + imm)[31:0] *)
  | Slliw of int_register * int_register * immediate12
  (** Shift Left Logical with Immediate Word. rd = (rs1 << shamt)[31:0] *)
  | Srliw of int_register * int_register * immediate12
  (** Shift Right Logical with Immediate Word. rd = (rs1 >> shamt)[31:0] *)
  | Sraiw of int_register * int_register * immediate12
  (** Shift Right Arithmetic with Immediate Word. rd = (rs1 >> shamt)[31:0] *)
  | Addw of int_register * int_register * int_register (** Add Word. rd = (rs1 + rs2)[31:0] *)
  | Subw of int_register * int_register * int_register (** Add Word. rd = (rs1 - rs2)[31:0] *)
  | Sllw of int_register * int_register * int_register
  (** Shifl Left Logical Word. rd = (rs1 << rs2)[31:0] *)
  | Srlw of int_register * int_register * int_register
  (** Shifl Right Logical Word. rd = (rs1 >> rs2)[31:0] *)
  | Sraw of int_register * int_register * int_register
  (** Shifl Right Arithmetical Word. rd = (rs1 >> rs2)[31:0] *)
  | Mulw of int_register * int_register * int_register (** Multiply Word. rd = (rs1 * rs2)[31:0] *)
  | Divw of int_register * int_register * int_register (** Division Word. rd = (rs1 / rs2)[31:0] *)
  | Divuw of int_register * int_register * int_register
  (** Division Word (Unsigned). rd = (rs1 / rs2)[31:0] *)
  | Remw of int_register * int_register * int_register (** Remainder Word. rd = (rs1 % rs2)[31:0] *)
  | Remwu of int_register * int_register * int_register
  (** Remainder Word (Unsigned). rd = (rs1 % rs2)[31:0] *)
  | Mv of int_register * int_register (** Copy from rs1 to rd. addi rd, rs1, 0 *)
  | Li of int_register * immediate32
  (** Load Immediate. lui rd, immediate20; addi rd, rd, immediate12 *)
  | Ret (** Return. Jalr x0, x1, 0 *)
[@@deriving eq, show { with_path = false }]

(** Expression in AST *)
type expr =
  | Instruction of instruction (** Instruction *)
  | Label of label (** Label *)
[@@deriving eq, show { with_path = false }]

(** AST is Presented by a List of Expressions *)
type ast = expr list [@@deriving eq, show { with_path = false }]
