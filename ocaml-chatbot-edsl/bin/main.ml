open Lwt.Infix
open Ocaml_chatbot_edsl

(* Immutable state store: association list chat_id -> state_name *)
type chat_states = (int * string) list

let get_state (states : chat_states) (chat_id : int) (default : string) : string
    =
  match List.assoc_opt chat_id states with Some s -> s | None -> default

let set_state (states : chat_states) (chat_id : int) (state : string) :
    chat_states =
  (chat_id, state) :: List.remove_assoc chat_id states

let handle_update bot states (u : Telegram.update) =
  let current = get_state states u.chat_id bot.initial_state in
  let next_state, reply = interpret bot current u.text in
  let states' = set_state states u.chat_id next_state in
  (states', reply)

let rec loop ~token ~offset bot states =
  Telegram.get_updates ~token ~offset >>= fun updates ->
  match updates with
  | [] -> loop ~token ~offset bot states
  | _ ->
      let new_offset =
        List.fold_left
          (fun mx (u : Telegram.update) -> max mx u.update_id)
          offset updates
        + 1
      in
      (* process updates sequentially, keep immutable state *)
      let process_one (st : chat_states) (u : Telegram.update) =
        let st', reply = handle_update bot st u in
        Telegram.send_message ~token ~chat_id:u.chat_id ~text:reply
        >|= fun () -> st'
      in
      Lwt_list.fold_left_s process_one states updates >>= fun states' ->
      loop ~token ~offset:new_offset bot states'

let () =
  let token =
    match Sys.getenv_opt "TELEGRAM_BOT_TOKEN" with
    | Some t -> t
    | None ->
        prerr_endline "ERROR: TELEGRAM_BOT_TOKEN is not set.";
        exit 1
  in
  print_endline "Telegram bot started (long polling)...";
  Lwt_main.run (loop ~token ~offset:0 example_bot [])
