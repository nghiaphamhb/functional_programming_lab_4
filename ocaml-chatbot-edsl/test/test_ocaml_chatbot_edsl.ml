open Alcotest
open Ocaml_chatbot_edsl

(* Helpers *)
let check_interpret ~state ~input ~exp_state ~exp_substr =
  (* check the expected state *)
  let st', reply = interpret my_bot state input in
  check string "next state" exp_state st';
  (* check the expected substring *)
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

(* ==== Simple tests ==== *)

(** get user's message "hi" and bot's state is "start", check if the response
    contains "What is your name"*)
let test_start_hello () =
  check_interpret ~state:"start" ~input:"hi" ~exp_state:"ask_name"
    ~exp_substr:"What is your name"

(** test input name
    - step 1: "start" + "hi" -> check if: new_state == "ask_name"
    - step 2: new_state + "Нгиа" -> check if: new_state_2 = "main" && reply
      contains "Нгиа"*)
let test_name_flow () =
  let st1, _ = interpret my_bot "start" "hi" in
  let st2, reply2 = interpret my_bot st1 "Нгиа" in
  check string "state after name" "main" st2;
  check bool "name preserved" true
    (let ls = String.length "Нгиа" in
     let lt = String.length reply2 in
     let rec loop i =
       if i + ls > lt then false
       else if String.sub reply2 i ls = "Нгиа" then true
       else loop (i + 1)
     in
     loop 0)

(* ==== Run tests ==== *)
let () =
  run "chatbot"
    [
      ( "fsm",
        [
          test_case "start: hi -> ask_name" `Quick test_start_hello;
          test_case "ask_name keeps raw name" `Quick test_name_flow;
        ] );
    ]
