//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// Manav Gupta
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  let beginswith (pattern: string) (token: string) = 
    token.StartsWith(pattern)


  //
  // matchToken
  //
  let private matchToken expected_token (tokens: string list) =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if beginswith expected_token next_token then
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)


  let rec private stmts tokens operators = 
    match tokens with
    | [] -> []
    | "}"::tl -> tokens
    | "int"::tl ->
      let T2 = matchToken "identifier" tl
      let T3 = matchToken ";" T2
      stmts T3 operators
    | "cin"::tl ->
      let T2 = matchToken ">>" tl
      let T3 = matchToken "identifier" T2
      let T4 = matchToken ";" T3
      stmts T4 operators
    | "cout"::tl ->
      let T2 = matchToken "<<" tl
      match T2 with
      | "endl"::tail ->
        let T3 = matchToken ";" tail
        stmts T3 operators
      | _ ->
        let T3 = checkExpr T2
        let T4 = matchToken ";" T3
        stmts T4 operators
    | hd::tl when hd.Contains("identifier") ->
      let T2 = matchToken "=" tl
      let T3 = expr T2 operators
      let T4 = matchToken ";" T3
      stmts T4 operators
    | "if"::tl ->
      let T1 = matchToken "(" tl
      let T2 = expr T1 operators
      let T3 = matchToken ")" T2
      let T4 = stmt T3 operators
      match T4 with
      | "else"::tail ->
        let T5 = stmt tail operators
        stmts T5 operators
      | _ -> stmts T4 operators
    | ";"::tl -> stmts tl operators
    | _ -> failwith ("expecting statement, but found " + List.head tokens)


  and private checkExpr tokens =
    let head = List.head tokens
    match head with
    | id when id.Contains("identifier") ->
      let T1 = matchToken "identifier" tokens
      T1
    | intl when intl.Contains("int_literal") ->
      let T1 = matchToken "int_literal" tokens
      T1
    | strl when strl.Contains("str_literal") ->
      let T1 = matchToken "str_literal" tokens
      T1
    | "true" -> List.tail tokens
    | "false" -> List.tail tokens
    | _ -> failwith ("expecting identifier or literal, but found " + List.head tokens)


  and private expr tokens operators =
    let T1 = checkExpr tokens
    match T1 with
    | ";"::tl -> T1
    | ")"::tl -> T1
    | hd::tl ->
      if List.contains hd operators then
        let T2 = checkExpr tl
        expr (List.tail T1) operators
      else
        T1
    | [] -> []


  and private stmt tokens operators = 
    match tokens with
    | [] -> []
    | "int"::tl ->
      let T2 = matchToken "identifier" tl
      matchToken ";" T2
    | "cin"::tl ->
      let T2 = matchToken ">>" tl
      let T3 = matchToken "identifier" T2
      matchToken ";" T3
    | "cout"::tl ->
      let T2 = matchToken "<<" tl
      match T2 with
      | "endl"::tl -> matchToken ";" tl
      | _::tl ->
        let T3 = checkExpr T2
        matchToken ";" T3
      | [] -> []
    | hd::tl when hd.Contains("identifier") ->
      let T2 = matchToken "=" tl
      let T3 = expr T2 operators
      matchToken ";" T3
    | "if"::tl ->
      let T1 = matchToken "(" tl
      let T2 = expr T1 operators
      let T3 = matchToken ")" T2
      let T4 = stmt T3 operators
      match T4 with
      | "else"::tail -> stmt tail operators
      | _ -> T4
    | ";"::tl -> tl
    | _ -> failwith ("expecting statement, but found " + List.head tokens)
    
  //
  // simpleC
  //
  let private simpleC tokens = 
    let operators = ["+";"-";"*";"/";"^";"<";"<=";">";">=";"==";"!="]
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    match T5 with
    | "{"::"}"::tl -> failwith ("expecting statement, but found }")
    | _ -> 
      let T6 = matchToken "{" T5
      let T7 = stmts T6 operators
      let T8 = matchToken "}" T7
      let T9 = matchToken "$" T8
      T9


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message
