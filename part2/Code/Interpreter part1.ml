type stackValue = BOOL of bool | INT of int | ERROR | STRING of string | NAME of string | UNIT | CLOSURE

type command = ADD | SUB | MUL | DIV| PUSH of stackValue | POP | REM | NEG | SWAP | TOSTRING | PRINTLN | QUIT | CAT | AND | OR | NOT | EQUAL | LESSTHAN | BIND | IF | LET | END


let rec interpreter input output = 
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
    | "and" -> AND
    | "or" -> OR
    | "not" -> NOT
    | "equal" -> EQUAL
    | "lessThan" -> LESSTHAN
    | "bind" -> BIND
    | "if" -> IF
    | "let" -> LET
    | "end" -> END
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
    |CLOSURE -> ":fun:"
  in


let comlist = List.map str2com strList in 


  let rec lookup key env : stackValue option =
    match env with
    |[] -> None
    |(name, value)::restOfList -> if name = key then Some value else lookup key restOfList
  in



  let rec processor cl stack env = 
    match (cl,stack) with 
    |(ADD::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands ( INT(a + b) :: restOfStack) env
    |(ADD::restOfCommands, stack) -> processor restOfCommands ( ERROR :: stack) env

    |(SUB::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands ( INT(b - a) :: restOfStack) env
    |(SUB::restOfCommands,stack) -> processor restOfCommands ( ERROR :: stack) env

    |(MUL::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands ( INT(a * b) :: restOfStack) env
    |(MUL::restOfCommands, stack) -> processor restOfCommands ( ERROR :: stack) env

    |(DIV::restOfCommands, INT(a)::INT(0)::restOfStack) -> processor restOfCommands ( ERROR :: stack) env
    |(DIV::restOfCommands, INT(0)::INT(b)::restOfStack) -> processor restOfCommands ( ERROR :: stack) env
    |(DIV::restOfCommands, INT(a)::INT(b)::restOfStack) when (b mod a) != 0 -> processor restOfCommands( INT(0) :: restOfStack) env
    |(DIV::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands( INT(b / a) ::restOfStack) env
    |(DIV::restOfCommands, stack) -> processor restOfCommands (ERROR ::stack) env

    |(PUSH(BOOL(true))::restOfCommands, restOfStack) -> processor restOfCommands ( BOOL(true) :: restOfStack) env
    |(PUSH(BOOL(false))::restOfCommands, restOfStack) -> processor restOfCommands ( BOOL(false) :: restOfStack) env

    |(PUSH(INT(a))::restOfCommands, restOfStack) -> processor restOfCommands(INT(a) :: restOfStack) env

    |(PUSH(STRING(a))::restOfCommands, restOfStack) -> processor restOfCommands( STRING(a):: restOfStack) env

    |(PUSH(NAME(a))::restOfCommands, restOfStack) -> processor restOfCommands( NAME(a)::restOfStack) env

    |(PUSH(UNIT)::restOfCommands, restOfStack) -> processor restOfCommands( UNIT::restOfStack) env

    |(PUSH(ERROR)::restOfCommands, restOfStack) -> processor restOfCommands( ERROR::restOfStack) env

    |(POP::restOfCommands, _::restOfStack) -> processor restOfCommands(restOfStack) env
    |(POP::restOfCommands, []) -> processor restOfCommands (ERROR :: stack) env

    |(REM::restOfCommands, INT(a)::INT(0)::restOfStack) -> processor restOfCommands (ERROR :: stack) env
    |(REM::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands (INT(b mod a):: restOfStack) env
    |(REM::restOfCommands, stack) -> processor restOfCommands (ERROR :: stack) env

    |(NEG::restOfCommands, INT(a)::[]) -> processor restOfCommands (INT(a * -1):: []) env
    |(NEG::restOfCommands, INT(a)::restOfStack) -> processor restOfCommands (INT(a * -1):: restOfStack) env
    |(NEG::restOfCommands, stack) -> processor restOfCommands (ERROR :: stack) env
    

    |(SWAP::restOfCommands, a::b::restOfStack) -> processor restOfCommands (b::a::restOfStack) env
    |(SWAP::restOfCommands, [a]) -> processor restOfCommands (ERROR :: [a])env
    |(SWAP::restOfCommands, []) -> processor restOfCommands (ERROR :: [])env

    |(TOSTRING::restOfCommands, []) -> processor restOfCommands (ERROR :: []) env
    |(TOSTRING::restOfCommands, a::restOfStack) -> processor restOfCommands (STRING(string_of_element a) ::restOfStack)env

    |(PRINTLN::restOfCommands, STRING str :: restOfStack) -> output_string oc (str ^ "\n"); processor restOfCommands restOfStack env
    |(PRINTLN::restOfCommands, x :: restOfStack) -> processor restOfCommands (ERROR :: x :: restOfStack)env
    |(PRINTLN::restOfCommands, []) -> processor restOfCommands (ERROR :: [])env

    |(CAT::restOfCommands, STRING(a)::STRING(b)::restOfStack) -> processor restOfCommands(STRING(a ^ b) :: restOfStack)env
    |(CAT::restOfCommands,STRING(a)::_::restOfStack) -> processor restOfCommands (ERROR::stack)env
    |(CAT::restOfCommands, _::STRING(a)::restOfStack) -> processor restOfCommands (ERROR::stack)env
    |(CAT::restOfCommands, _::_::restOfStack) -> processor restOfCommands (ERROR::stack)env
    |(CAT::restOfCommands, []) -> processor restOfCommands (ERROR::stack)env

    |(AND::restOfCommands, BOOL(true)::BOOL(true)::restOfStack) -> processor restOfCommands(BOOL(true)::restOfStack)env
    |(AND::restOfCommands, BOOL(false)::BOOL(true)::restOfStack) -> processor restOfCommands(BOOL(false)::restOfStack)env
    |(AND::restOfCommands, BOOL(true)::BOOL(false)::restOfStack) -> processor restOfCommands(BOOL(false)::restOfStack)env
    |(AND::restOfCommands, BOOL(false)::BOOL(false)::restOfStack) -> processor restOfCommands(BOOL(false)::restOfStack)env
    |(AND::restOfCommands, stack) -> processor restOfCommands(ERROR::stack)env

    |(OR::restOfCommands, BOOL(true)::BOOL(true)::restOfStack) -> processor restOfCommands(BOOL(true)::restOfStack)env
    |(OR::restOfCommands, BOOL(false)::BOOL(true)::restOfStack) -> processor restOfCommands(BOOL(true)::restOfStack)env
    |(OR::restOfCommands, BOOL(true)::BOOL(false)::restOfStack) -> processor restOfCommands(BOOL(true)::restOfStack)env
    |(OR::restOfCommands, BOOL(false)::BOOL(false)::restOfStack) -> processor restOfCommands(BOOL(false)::restOfStack)env
    |(OR::restOfCommands, stack) -> processor restOfCommands(ERROR::stack)env

    |(NOT::restOfCommands, BOOL(true)::restOfStack) -> processor restOfCommands(BOOL(false)::restOfStack)env
    |(NOT::restOfCommands, BOOL(false)::restOfStack) -> processor restOfCommands(BOOL(true)::restOfStack)env
    |(NOT::restOfCommands, stack) -> processor restOfCommands(ERROR::stack)env

    |(EQUAL::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands(BOOL(a=b)::restOfStack)env
    |(EQUAL::restOfCommands, stack) -> processor restOfCommands(ERROR::stack)env

    (*CHECK THIS IN TESTING ORDER OF A AND B*)
    |(LESSTHAN::restOfCommands, INT(a)::INT(b)::restOfStack) -> processor restOfCommands(BOOL(b<a)::restOfStack)env
    |(LESSTHAN::restOfCommands, stack) -> processor restOfCommands(ERROR::stack)env

    |(BIND::restOfCommands, INT(a)::NAME(b)::restOfStack) -> let enviorment = (b,INT(a)) :: env in processor restOfCommands( UNIT :: restOfStack) enviorment
    |(BIND::restOfCommands, STRING(a)::NAME(b)::restOfStack) -> let enviorment = (b,STRING(a)) :: env in processor restOfCommands( UNIT :: restOfStack) enviorment
    |(BIND::restOfCommands, BOOL(a)::NAME(b)::restOfStack) -> let enviorment = (b,BOOL(a)) :: env in processor restOfCommands( UNIT :: restOfStack) enviorment
    
    
    |(IF::restOfCommands, a::b::BOOL(true)::restOfStack) -> processor restOfCommands (a::restOfStack) env
    |(IF::restOfCommands, a::b::BOOL(false)::restOfStack) -> processor restOfCommands (b::restOfStack) env
    |(IF::restOfCommands, stack) -> processor restOfCommands (ERROR::stack) env
    

    |(LET::restOfCommands, stack) -> processor restOfCommands (stack)env
    |(END::restOfCommands, stack) -> processor restOfCommands(stack) env
    
    
    
    |(BIND::restOfCommands, UNIT::NAME(b)::restOfStack) -> let enviorment = (b, UNIT) :: env in processor restOfCommands(UNIT:: restOfStack) enviorment



    |(BIND::restOfCommands, NAME(a)::NAME(b)::restOfStack) -> 
      (match lookup a env with 
      | Some x -> let enviorment = (b , x) :: env in processor restOfCommands(UNIT :: restOfStack) enviorment
      | _ -> processor restOfCommands(ERROR :: NAME(a) :: NAME(b) :: restOfStack) env)

    |(BIND::restOfCommands, stack) -> processor restOfCommands(ERROR::stack) env


    


    |(QUIT::_, stack) -> stack

   |(_, stack) -> ERROR::stack 
  in 
  let _ = processor comlist [] [] in 
  close_in ic;
  close_out oc;
  
  (*let () = interpreter Sys.argv.(1) "output_test_for_input_13.txt"*)