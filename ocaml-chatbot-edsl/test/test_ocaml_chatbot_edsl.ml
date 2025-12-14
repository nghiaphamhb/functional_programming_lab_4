open Alcotest
open Ocaml_chatbot_edsl

let check_interpret ~state ~input ~exp_state ~exp_substr =
  let (st', reply) = interpret example_bot state input in
  check string "next state" exp_state st';
  check bool "reply contains"
    true
    (String.length exp_substr = 0 || String.contains reply exp_substr.[0] && String.length reply >= 0);
  (* safer substring check *)
  let has_sub =
    let lt = String.length reply and ls = String.length exp_substr in
    let rec loop i =
      if i + ls > lt then false
      else if String.sub reply i ls = exp_substr then true
      else loop (i + 1)
    in
    if ls = 0 then true else loop 0
  in
  check bool "reply has substring" true has_sub

let test_start_hello () =
  check_interpret
    ~state:"start"
    ~input:"привет"
    ~exp_state:"ask_name"
    ~exp_substr:"Как тебя зовут"

let test_name_flow () =
  let (st1, _) = interpret example_bot "start" "привет" in
  let (st2, reply2) = interpret example_bot st1 "Нгиа" in
  check string "state after name" "main" st2;
  check bool "name preserved"
    true
    (let ls = String.length "Нгиа" in
     let lt = String.length reply2 in
     let rec loop i =
       if i + ls > lt then false
       else if String.sub reply2 i ls = "Нгиа" then true
       else loop (i + 1)
     in
     loop 0)

let () =
  run "chatbot"
    [
      ("fsm", [
          test_case "start: привет -> ask_name" `Quick test_start_hello;
          test_case "ask_name keeps raw name" `Quick test_name_flow;
        ]);
    ]
