open Ocaml_chatbot_edsl

let rec loop bot state =
  print_string "You: ";
  let line =
    try read_line () with End_of_file -> "/quit"
  in
  if String.equal line "/quit" then (
    print_endline "Bye!";
    ()
  ) else (
    let (new_state, reply) = interpret bot state line in
    print_endline ("Bot: " ^ reply);
    loop bot new_state
  )

let () =
  print_endline "Console chatbot started. Type /quit to exit.";
  loop example_bot example_bot.initial_state
