let read _ _ =
  let command = read_line () in
  Lexing.from_string command |> Parser.program Lexer.token |> List.hd |> Result.ok
