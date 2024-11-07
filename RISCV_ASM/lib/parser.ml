(** Copyright 2024, Vyacheslav Kochergin and Roman Mukovenkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let parse_number =
  let sign = option "" (string "-") in
  let digits =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  lift2 ( ^ ) sign digits >>= fun num_str -> return (int_of_string num_str)
;;

let ws =
  take_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

let parse_string =
  take_while1 (function
    | ' ' | '\t' | '\n' | '\r' | ',' | ':' -> false
    | _ -> true)
;;

let parse_escaped_char = char '\\' *> (char 'n' *> return '\n')

let parse_regular_char =
  satisfy (function
    | '"' | '\\' -> false
    | _ -> true)
;;

let char_list_to_string char_list =
  let len = List.length char_list in
  String.init len (List.nth char_list)
;;

let parse_quoted_string =
  char '"' *> many (parse_escaped_char <|> parse_regular_char)
  <* char '"'
  >>| char_list_to_string
;;

let parse_type = char '@' *> parse_string >>= fun str -> return (Type str)

let parse_number_or_quoted_string =
  peek_char
  >>= function
  | Some '"' -> parse_quoted_string >>= fun str -> return (StrValue str)
  | Some '0' .. '9' | Some '-' -> parse_number >>= fun num -> return (IntValue num)
  | _ -> fail "Expected number or quoted string"
;;

let ws_opt p = ws *> p <* ws

let register_map =
  List.sort
    (fun (name1, _) (name2, _) -> compare (String.length name2) (String.length name1))
    [ "x0", 0
    ; "zero", 0
    ; "x1", 1
    ; "ra", 1
    ; "x2", 2
    ; "sp", 2
    ; "x3", 3
    ; "gp", 3
    ; "x4", 4
    ; "tp", 4
    ; "x5", 5
    ; "t0", 5
    ; "x6", 6
    ; "t1", 6
    ; "x7", 7
    ; "t2", 7
    ; "x8", 8
    ; "s0", 8
    ; "fp", 8
    ; "x9", 9
    ; "s1", 9
    ; "x10", 10
    ; "a0", 10
    ; "x11", 11
    ; "a1", 11
    ; "x12", 12
    ; "a2", 12
    ; "x13", 13
    ; "a3", 13
    ; "x14", 14
    ; "a4", 14
    ; "x15", 15
    ; "a5", 15
    ; "x16", 16
    ; "a6", 16
    ; "x17", 17
    ; "a7", 17
    ; "x18", 18
    ; "s2", 18
    ; "x19", 19
    ; "s3", 19
    ; "x20", 20
    ; "s4", 20
    ; "x21", 21
    ; "s5", 21
    ; "x22", 22
    ; "s6", 22
    ; "x23", 23
    ; "s7", 23
    ; "x24", 24
    ; "s8", 24
    ; "x25", 25
    ; "s9", 25
    ; "x26", 26
    ; "s10", 26
    ; "x27", 27
    ; "s11", 27
    ; "x28", 28
    ; "t3", 28
    ; "x29", 29
    ; "t4", 29
    ; "x30", 30
    ; "t5", 30
    ; "x31", 31
    ; "t6", 31
    ]
;;

let parse_register =
  ws_opt
    (choice
       (List.map (fun (name, num) -> string name *> return (Register num)) register_map))
;;

let parse_immediate12 = ws_opt (lift (fun imm -> ImmediateAddress12 imm) parse_number)
let parse_immediate20 = ws_opt (lift (fun imm -> ImmediateAddress20 imm) parse_number)
let parse_immediate32 = ws_opt (lift (fun imm -> ImmediateAddress32 imm) parse_number)
let parse_label_address12 = ws_opt (lift (fun str -> LabelAddress12 str) parse_string)
let parse_label_address20 = ws_opt (lift (fun str -> LabelAddress20 str) parse_string)
let parse_label_address32 = ws_opt (lift (fun str -> LabelAddress32 str) parse_string)

let parse_label_expr =
  ws_opt (lift (fun str -> LabelExpr str) (parse_string <* ws_opt (char ':')))
;;

let parse_address12 = ws_opt (choice [ parse_immediate12; parse_label_address12 ])
let parse_address20 = ws_opt (choice [ parse_immediate20; parse_label_address20 ])
let parse_address32 = ws_opt (choice [ parse_immediate32; parse_label_address32 ])

let parse_directive =
  ws_opt
    (choice
       [ string ".file"
         *> ws_opt (lift (fun str -> DirectiveExpr (File str)) parse_quoted_string)
       ; string ".option"
         *> ws_opt (lift (fun str -> DirectiveExpr (Option str)) parse_string)
       ; string ".attribute"
         *> ws_opt
              (lift2
                 (fun tag value -> DirectiveExpr (Attribute (tag, value)))
                 parse_string
                 (ws_opt (char ',') *> parse_number_or_quoted_string))
       ; string ".text" *> return (DirectiveExpr Text)
       ; string ".align"
         *> ws_opt (lift (fun int -> DirectiveExpr (Align int)) parse_number)
       ; string ".globl"
         *> ws_opt (lift (fun label -> DirectiveExpr (Globl label)) parse_label_address12)
       ; string ".type"
         *> ws_opt
              (lift2
                 (fun str type_str -> DirectiveExpr (TypeDir (str, type_str)))
                 parse_string
                 (ws_opt (char ',') *> parse_type))
       ; string ".cfi_startproc" *> return (DirectiveExpr CfiStartproc)
       ; string ".cfi_endproc" *> return (DirectiveExpr CfiEndproc)
       ; string ".cfi_remember_state" *> return (DirectiveExpr CfiRememberState)
       ; string ".cfi_restore_state" *> return (DirectiveExpr CfiRestoreState)
       ; string ".size"
         *> ws_opt
              (lift2
                 (fun label size -> DirectiveExpr (Size (label, size)))
                 (* FIXME: Size can actually be an expression with +, -, * and operands, . (dot) is current address*)
                 parse_label_address12
                 (ws_opt (char ',') *> parse_string))
       ; string ".section"
         *> ws_opt
              (lift4
                 (fun section_name section_type section_flags section_index ->
                   DirectiveExpr
                     (Section (section_name, section_type, section_flags, section_index)))
                 parse_string
                 (ws_opt (char ',') *> ws_opt parse_quoted_string)
                 (ws_opt (char ',') *> ws_opt parse_type)
                 (peek_char
                  >>= function
                  | Some ',' ->
                    ws_opt (char ',') *> ws_opt (lift (fun i -> Some i) parse_number)
                  | _ -> return None))
       ; string ".string"
         *> ws_opt (lift (fun str -> DirectiveExpr (String str)) parse_quoted_string)
       ; string ".cfi_def_cfa_offset"
         *> ws_opt (lift (fun int -> DirectiveExpr (CfiDefCfaOffset int)) parse_number)
       ; string ".cfi_offset"
         *> ws_opt
              (lift2
                 (fun int1 int2 -> DirectiveExpr (CfiOffset (int1, int2)))
                 parse_number
                 (ws_opt (char ',') *> ws_opt parse_number))
       ; string ".cfi_restore"
         *> ws_opt (lift (fun int -> DirectiveExpr (CfiRestore int)) parse_number)
       ; string ".ident"
         *> ws_opt (lift (fun str -> DirectiveExpr (Ident str)) parse_quoted_string)
       ])
;;

let parse_instruction =
  ws_opt
    (choice
       [ string "add"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Add (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; string "li"
         *> lift2
              (fun r1 imm32 -> InstructionExpr (Li (r1, imm32)))
              parse_register
              (char ',' *> parse_address32)
       ; string "mv"
         *> lift2
              (fun r1 r2 -> InstructionExpr (Mv (r1, r2)))
              parse_register
              (char ',' *> parse_register)
       ; string "beq"
         *> lift3
              (fun r1 r2 adr -> InstructionExpr (Beq (r1, r2, adr)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; string "addiw"
         *> lift3
              (fun r1 r2 imm -> InstructionExpr (Addiw (r1, r2, imm)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; string "mulw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Mulw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; string "bne"
         *> lift3
              (fun r1 r2 adr -> InstructionExpr (Bne (r1, r2, adr)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; string "ld"
         *> lift3
              (fun r1 imm r2 -> InstructionExpr (Ld (r1, r2, imm)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; string "sd"
         *> lift3
              (fun r1 imm r2 -> InstructionExpr (Sd (r1, r2, imm)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; string "lw"
         *> lift3
              (fun r1 imm r2 -> InstructionExpr (Lw (r1, r2, imm)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; string "addi"
         *> lift3
              (fun r1 r2 imm -> InstructionExpr (Addi (r1, r2, imm)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; string "xor"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Xor (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; string "ret" *> return (InstructionExpr Ret)
       ; string "jr" *> lift (fun r1 -> InstructionExpr (Jr r1)) parse_register
       ; string "j" *> lift (fun adr -> InstructionExpr (J adr)) parse_address20
       ; string "call"
         *> ws_opt (lift (fun str -> InstructionExpr (Call str)) parse_string)
       ; string "la"
         *> ws_opt
              (lift2
                 (fun r1 adr -> InstructionExpr (La (r1, adr)))
                 parse_register
                 (char ',' *> parse_address32))
       ; string "lla"
         *> ws_opt
              (lift2
                 (fun r1 adr -> InstructionExpr (Lla (r1, adr)))
                 parse_register
                 (char ',' *> parse_address32))
       ])
;;

let parse_expr = ws_opt (choice [ parse_directive; parse_instruction; parse_label_expr ])

let parse_ast =
  ws_opt (many parse_expr)
  <* (end_of_input
      <|> (Angstrom.pos
           >>= fun pos ->
           Angstrom.peek_char
           >>= function
           | None -> return ()
           | Some c ->
             fail (Printf.sprintf "Unexpected character: '%c' at position %d" c pos)))
;;
