type stackValue = BOOL of bool | INT of int | ERROR | STRING of string | NAME of string | UNIT 

type command = ADD | SUB | MUL | DIV| PUSH of stackValue | POP | REM | NEG | SWAP | TOSTRING | PRINTLN | QUIT | CAT



let interpreter input output = 
  let ic = open_in input in
  let oc = open_out output in
  let rec loop_read acc = 
    try
      let l = String.trim(input_line ic) in loop_read (l :: acc)
    with
    | End_of_file -> List.rev acc
  in
  let strList = loop_read [] in

  let str2sv s = 
    (*first string*)
    let pushfirst = String.get s 5 in
    (*everything after*)
    let pushvalue = String.sub s 5 (String.length s - 5) in
    match pushfirst with
    | '"' -> let pushvalue = String.sub s 6 (String.length s-7) in
                PUSH(STRING(pushvalue))
    (*why is there a empty one ? me forgor*)
    (*| ' ' -> PUSH(INT(int_of_string pushvalue))*)
    | '-' | '0' .. '9' -> PUSH(INT(int_of_string pushvalue))
    (*not accounting for the whole string*)
    | '_' | 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'T'|'V'|'W'|'X'|'Y'|'Z' -> PUSH(NAME(pushvalue))
    | _ -> PUSH (ERROR)
    in
  
  let str2com s = 
    match s with 
    | "add" -> ADD 
    | "sub" -> SUB
    | "mul" -> MUL
    | "div" -> DIV
    | "pop" -> POP
    | "rem" -> REM
    | "neg" -> NEG
    | "swap" -> SWAP
    | "quit" -> QUIT
    | "toString" -> TOSTRING
    | "println" -> PRINTLN
    | "cat" -> CAT
    | "push -0" -> PUSH(INT(0))
    | "push :true:"  -> PUSH(BOOL(true))
    | "push :false:" -> PUSH(BOOL(false))
    | "push :unit:" -> PUSH(UNIT)
    | "push :error:" -> PUSH(ERROR)
    | _ -> str2sv s
  in


  let string_of_element ele = 
    match ele with
    |INT n -> string_of_int n
    |BOOL true -> ":true:"
    |BOOL false -> ":false:"
    |STRING str -> str
    |NAME str -> str
    |UNIT -> ":unit:"
    |ERROR -> ":error:"
    (*|CLOSURE -> ":fun:"*)
  in

let comlist = List.map str2com strList in 

  let rec processor cl stack = 
    match (cl,stack) with 
    |(ADD::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands ( INT(a + b) :: restOfStack)
    |(ADD::restOfCommands, stack) -> processor restOfCommands ( ERROR :: stack)

    |(SUB::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands ( INT(b - a) :: restOfStack)
    |(SUB::restOfCommands,stack) -> processor restOfCommands ( ERROR :: stack)

    |(MUL::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands ( INT(a * b) :: restOfStack)
    |(MUL::restOfCommands, stack) -> processor restOfCommands ( ERROR :: stack)

    |(DIV::restOfCommands, INT(a)::INT(0)::restOfStack) -> processor restOfCommands ( ERROR :: stack)
    |(DIV::restOfCommands, INT(0)::INT(b)::restOfStack) -> processor restOfCommands ( ERROR :: stack)
    |(DIV::restOfCommands, INT(a)::INT(b)::restOfStack) when (b mod a) != 0 -> processor restOfCommands( INT(0) :: restOfStack)
    |(DIV::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands( INT(b / a) ::restOfStack)
    |(DIV::restOfCommands, stack) -> processor restOfCommands (ERROR ::stack)

    |(PUSH(BOOL(true))::restOfCommands, restOfStack) -> processor restOfCommands ( BOOL(true) :: restOfStack)
    |(PUSH(BOOL(false))::restOfCommands, restOfStack) -> processor restOfCommands ( BOOL(false) :: restOfStack)

    |(PUSH(INT(a))::restOfCommands, restOfStack) -> processor restOfCommands(INT(a) :: restOfStack)

    |(PUSH(STRING(a))::restOfCommands, restOfStack) -> processor restOfCommands( STRING(a):: restOfStack)

    |(PUSH(NAME(a))::restOfCommands, restOfStack) -> processor restOfCommands( NAME(a)::restOfStack)

    |(PUSH(UNIT)::restOfCommands, restOfStack) -> processor restOfCommands( UNIT::restOfStack)

    |(PUSH(ERROR)::restOfCommands, restOfStack) -> processor restOfCommands( ERROR::restOfStack)

    |(POP::restOfCommands, _::restOfStack) -> processor restOfCommands(restOfStack)
    |(POP::restOfCommands, []) -> processor restOfCommands (ERROR :: stack)

    |(REM::restOfCommands, INT(a)::INT(0)::restOfStack) -> processor restOfCommands (ERROR :: stack)
    |(REM::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands (INT(b mod a):: restOfStack)
    |(REM::restOfCommands, stack) -> processor restOfCommands (ERROR :: stack)

    |(NEG::restOfCommands, INT(a)::[]) -> processor restOfCommands (INT(a * -1):: [])
    |(NEG::restOfCommands, INT(a)::restOfStack) -> processor restOfCommands (INT(a * -1):: restOfStack)
    |(NEG::restOfCommands, stack) -> processor restOfCommands (ERROR :: stack)
    

    |(SWAP::restOfCommands, a::b::restOfStack) -> processor restOfCommands (b::a::restOfStack)
    |(SWAP::restOfCommands, [a]) -> processor restOfCommands (ERROR :: [a])
    |(SWAP::restOfCommands, []) -> processor restOfCommands (ERROR :: [])

    |(TOSTRING::restOfCommands, []) -> processor restOfCommands (ERROR :: [])
    |(TOSTRING::restOfCommands, a::restOfStack) -> processor restOfCommands (STRING(string_of_element a) ::restOfStack)

    |(PRINTLN::restOfCommands, STRING str :: restOfStack) -> output_string oc (str ^ "\n"); processor restOfCommands restOfStack
    |(PRINTLN::restOfCommands, x :: restOfStack) -> processor restOfCommands (ERROR :: x :: restOfStack)
    |(PRINTLN::restOfCommands, []) -> processor restOfCommands (ERROR :: [])

    |(CAT::restOfCommands, STRING(a)::STRING(b)::restOfStack) -> processor restOfCommands(STRING(a ^ b) :: restOfStack)
    |(CAT::restOfCommands,STRING(a)::_::restOfStack) -> processor restOfCommands (ERROR::stack)
    |(CAT::restOfCommands, _::STRING(a)::restOfStack) -> processor restOfCommands (ERROR::stack)
    |()

    |(QUIT::_, stack) -> stack

   |(_, stack) -> ERROR::stack 
  in 
  let _ = processor comlist [] in 
  close_in ic;
  close_out oc;
  
  (*let () = interpreter Sys.argv.(1) "interpreter/part1/output_testINPUTS2.txt"*)