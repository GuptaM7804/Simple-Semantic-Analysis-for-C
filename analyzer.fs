﻿//
// Analyzer for simple C programs.  This component performs
// semantic analysis, in particular collecting variable
// names and their types. The analysis also checks to ensure
// variable names are unique --- no duplicates.
//
// If all is well, a "symbol table" is built and returned,
// containing all variables and their types. A symbol table
// is a list of tuples of the form (name, type).  Example:
//
//   [("x", "int"); ("y", "int"); ("z", "real")]
//
// Modified by:
//   Manav Gupta
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module analyzer =
  //
  // NOTE: all functions in the module must be indented.
  //

  let private matchToken expected_token (tokens: string list) =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation:
    //
    // NOTE: identifier, int_literal and str_literal
    // are special cases because they are followed by
    // the name or literal value. In these cases exact
    // matching will not work, so we match the start 
    // of the token in these cases.
    //
    let next_token = List.head tokens

    if expected_token = "identifier" && next_token.StartsWith("identifier") then
      //
      // next_token starts with identifier, so we have a match:
      //
      List.tail tokens
    elif expected_token = "int_literal" && next_token.StartsWith("int_literal") then
      //
      // next_token starts with int_literal, so we have a match:
      //
      List.tail tokens
    elif expected_token = "real_literal" && next_token.StartsWith("real_literal") then
      //
      // next_token starts with int_literal, so we have a match:
      //
      List.tail tokens
    elif expected_token = "str_literal" && next_token.StartsWith("str_literal") then
      //
      // next_token starts with str_literal, so we have a match:
      //
      List.tail tokens
    elif expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)


  //
  // <expr-value> -> identifier
  //               | int_literal
  //               | real_literal
  //               | str_literal
  //               | true
  //               | false
  //
  let rec private expr_value tokens symbolTable =
    let next_token = List.head tokens
    //
    if next_token = "false" then
      let T2 = matchToken "false" tokens
      T2
    elif next_token = "true" then
      let T2 = matchToken "true" tokens
      T2
    //
    // the others are trickier since we have to look 
    // at the start of the string for a match:
    //
    elif next_token.StartsWith("identifier") then
      let token = List.head tokens
      let parts = token.Split(":")
      let var = parts.[1]
      let check = List.exists (fun (x,_) -> x = var) symbolTable
      if (check) then
        let T2 = matchToken "identifier" tokens
        T2
      else
        failwith ("variable '" + var + "' undefined")
    elif next_token.StartsWith("int_literal") then
      let T2 = matchToken "int_literal" tokens
      T2
    elif next_token.StartsWith("real_literal") then
      let T2 = matchToken "real_literal" tokens
      T2
    elif next_token.StartsWith("str_literal") then
      let T2 = matchToken "str_literal" tokens
      T2
    else
      failwith ("expecting identifier or literal, but found " + next_token)


  //
  // <expr-op> -> +
  //            | -
  //            | *
  //            | /
  //            | ^
  //            | <
  //            | <=
  //            | >
  //            | >=
  //            | ==
  //            | !=
  //
  let rec private expr_op tokens = 
    let next_token = List.head tokens
    //
    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then
      //
      let T2 = matchToken next_token tokens
      T2
    else
      // error
      failwith ("expecting expression operator, but found " + next_token)


  //
  // <expr> -> <expr-value> <expr-op> <expr-value>
  //         | <expr-value>
  //
  let rec private expr tokens symbolTable = 
    //
    // first we have to match expr-value, since both
    // rules start with this:
    //
    let T2 = expr_value tokens symbolTable
    //
    // now let's see if there's more to the expression:
    //
    let next_token = List.head T2
    //
    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then
      //
      let T3 = expr_op T2
      let T4 = expr_value T3 symbolTable
      T4
    else
      // just expr_value, that's it
      T2


  //
  // <empty> -> ;
  //
  let rec private empty tokens = 
    let T2 = matchToken ";" tokens
    T2


  //
  // <vardecl> -> int identifier ;
  //
  let rec private vardecl tokens symbolTable = 
    match tokens with
    | "int"::tail ->
      let token = List.head tail
      let parts = token.Split(":")
      let var = parts.[1]
      let symbol = (var, "int")
      if (List.contains (var, "int") symbolTable) || (List.contains (var, "real") symbolTable) then failwith ("redefinition of variable '" + var + "'")
      let ST = symbol::symbolTable
      let T3 = matchToken "identifier" tail
      let T4 = matchToken ";" T3
      (T4, ST)
    | "real"::tail ->
      let token = List.head tail
      let parts = token.Split(":")
      let var = parts.[1]
      let symbol = (var, "real")
      if (List.contains (var, "int") symbolTable) || (List.contains (var, "real") symbolTable) then failwith ("redefinition of variable '" + var + "'")
      let ST = symbol::symbolTable
      let T3 = matchToken "identifier" tail
      let T4 = matchToken ";" T3
      (T4, ST)
    | _ -> failwith ("expecting real or int, but found " + List.head tokens)


  //
  // <input> -> cin >> identifier ;
  //
  let rec private input tokens symbolTable = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let token = List.head T3
    let parts = token.Split(":")
    let var = parts.[1]
    let check = List.exists (fun (x,_) -> x = var) symbolTable
    if (check) then
      let T4 = matchToken "identifier" T3
      let T5 = matchToken ";" T4
      T5
    else
      failwith ("variable '" + var + "' undefined")


  //
  // <output-value> -> <expr-value>
  //                 | endl
  //
  let rec private output_value tokens symbolTable = 
    let next_token = List.head tokens
    //
    if next_token = "endl" then
      let T2 = matchToken "endl" tokens
      T2
    else
      let T2 = expr_value tokens symbolTable
      T2


  //
  // <output> -> cout << <output-value> ;
  //
  let rec private output tokens symbolTable = 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3 symbolTable
    let T5 = matchToken ";" T4
    T5


  //
  // <assignment> -> identifier = <expr> ;
  //
  let rec private assignment tokens symbolTable = 
    let T2 = matchToken "identifier" tokens
    let token = List.head tokens
    let parts = token.Split(":")
    let var = parts.[1]
    let check = List.exists (fun (x,_) -> x = var) symbolTable
    if (check) then
      let T3 = matchToken "=" T2
      let T4 = expr T3 symbolTable
      let T5 = matchToken ";" T4
      T5
    else
      failwith ("variable '" + var + "' undefined")


  //
  // <stmt> -> <empty>
  //         | <vardecl>
  //         | <input>
  //         | <output>
  //         | <assignment>
  //         | <ifstmt>
  //
  let rec private stmt tokens symbolTable = 
    let next_token = List.head tokens
    //
    // use the next token to determine which rule
    // to call; if none match then it's a syntax
    // error:
    //
    if next_token = ";" then
      let T2 = empty tokens
      (T2, symbolTable)
    elif next_token = "int" then
      let (T2, ST2) = vardecl tokens symbolTable
      (T2, ST2)
    elif next_token = "real" then
      let (T2, ST2) = vardecl tokens symbolTable
      (T2, ST2)
    elif next_token = "cin" then
      let T2 = input tokens symbolTable
      (T2, symbolTable)
    elif next_token = "cout" then
      let T2 = output tokens symbolTable
      (T2, symbolTable)
    elif next_token.StartsWith("identifier") then
      let T2 = assignment tokens symbolTable
      (T2, symbolTable)
    elif next_token = "if" then
      let (T2, ST2) = ifstmt tokens symbolTable
      (T2, ST2)
    else
      failwith ("expecting statement, but found " + next_token)
  //
  // <ifstmt> -> if ( <condition> ) <then-part> <else-part>
  //
  and private ifstmt tokens symbolTable = 
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3 symbolTable
    let T5 = matchToken ")" T4
    let (T6, ST6) = then_part T5 symbolTable
    let (T7, ST7) = else_part T6 ST6
    (T7, ST7)
  //
  // <condition> -> <expr>
  //
  and private condition tokens symbolTable= 
    let T2 = expr tokens symbolTable
    T2
  //
  // <then-part> -> <stmt>
  //
  and private then_part tokens symbolTable = 
    let (T2, ST2) = stmt tokens symbolTable
    (T2, ST2)
  //
  // <else-part> -> else <stmt>
  //              | EMPTY
  //
  and private else_part tokens symbolTable = 
    let next_token = List.head tokens
    //
    if next_token = "else" then
      let T2 = matchToken "else" tokens
      let (T3, ST3) = stmt T2 symbolTable
      (T3, ST3)
      else 
        // EMPTY => do nothing, just return tokens back
        (tokens, symbolTable)


  //
  // <morestmts> -> <stmt> <morestmts>
  //              | EMPTY
  //
  let rec private morestmts tokens symbolTable = 
    //
    // if the next token denotes the start of a stmt 
    // then process stmt and morestmts, otherwise apply
    // EMPTY
    //
    let next_token = List.head tokens
    //
    if next_token = ";"    ||
       next_token = "int"  ||
       next_token = "real" ||
       next_token = "cin"  ||
       next_token = "cout" ||
       next_token.StartsWith("identifier") ||
       next_token = "if" then
     
      let (T2, ST2) = stmt tokens symbolTable
      let (T3, ST3) = morestmts T2 ST2
      (T3, ST3)
    else 
      // EMPTY => do nothing, just return tokens back
      (tokens, symbolTable)


  //
  // <stmts> -> <stmt> <morestmts>
  // 
  let rec private stmts tokens symbolTable = 
    let (T2, ST2) = stmt tokens symbolTable
    let (T3, ST3) = morestmts T2 ST2
    (T3, ST3)


  //
  // <simpleC> -> void main ( ) { <stmts> } $
  //
  let private simpleC tokens = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7, symbolTable) = stmts T6 []
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8  // $ => EOF, there should be no more tokens
    (T9, symbolTable)


  //
  // build_symboltable tokens
  //
  // Given a list of tokens, analyzes the program by looking
  // at variable declarations and collecting them into a
  // list. This list is known as a symbol table. Returns
  // a tuple (result, symboltable), where result is a string 
  // denoting "success" if valid, otherwise a string of the 
  // form "semantic_error:...".
  //
  // On success, the symboltable is a list of tuples of the
  // form (name, type), e.g. [("x","int"); ("y","real")]. On 
  // an error, the returned list is empty [].
  //
  let build_symboltable tokens = 
    try
      let (T2, symboltable) = simpleC tokens
      ("success", symboltable)
    with 
      | ex -> ("semantic_error: " + ex.Message, [])
