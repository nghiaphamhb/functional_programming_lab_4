(* ==== Definition of bot structure ====*)
(* Pattern to match user input *)
type pattern =
  | Exact of string
  | Contains of string
  | Starts_with of string
  | Any

(* Action performed by the bot *)
type action = Say of (string -> string) | Goto of string

(* Rule: pattern + list of actions *)
type rule = { pattern : pattern; actions : action list }

(* State of chatbot *)
type state = { name : string; rules : rule list }

(* Chatbot definition *)
type bot = { initial_state : string; states : state list }

(* ==== Declaration functions ==== *)
let rule pattern actions = { pattern; actions }
let state name rules = { name; rules }
let chatbot initial_state states = { initial_state; states }

(* ==== Bot's actions ==== *)
(* bot send fixed reply *)
let say text = Say (fun _ -> text)

(* bot send dynamic reply (using sent user's message) *)
let sayf f = Say f

(* change bot's state *)
let goto state_name = Goto state_name

(* ==== Helpers: do lowercase user's input (except username) ==== *)
(* trim text: "\t\rhi bot\n" -> "hi bot" *)
let string_trim (s : string) : string = String.trim s

(* Apply lowercase action to command only *)
let normalize_for_command (s : string) : string =
  s |> string_trim |> String.lowercase_ascii

(* ===== Simple calculator for "calc a [op] b" ===== *)
(* definition of calculator's errors *)
type calc_error = Parse_error | Division_by_zero

(* parse string input to int *)
let parse_int (s : string) : int option =
  try Some (int_of_string (string_trim s)) with _ -> None

(* find first operator among + - * / and its index in the line input*)
(* "10+5" → Some (2, '+'); "123" → None *)
let find_first_op (s : string) : (int * char) option =
  let n = String.length s in
  let is_op c = c = '+' || c = '-' || c = '*' || c = '/' in
  let rec loop i =
    if i >= n then None
    else
      let c = s.[i] in
      if is_op c then Some (i, c) else loop (i + 1)
  in
  if n > 0 && s.[0] = '-' then loop 1 else loop 0

(* calculate expression *)
let eval_simple_expr (expr : string) : (int, calc_error) result =
  let e = string_trim expr in
  match find_first_op e with
  | None -> Error Parse_error
  | Some (idx, op) -> (
      (* take a & b *)
      let left = String.sub e 0 idx in
      let right = String.sub e (idx + 1) (String.length e - idx - 1) in
      match (parse_int left, parse_int right) with
      | Some a, Some b -> (
          match op with
          | '+' -> Ok (a + b)
          | '-' -> Ok (a - b)
          | '*' -> Ok (a * b)
          | '/' -> if b = 0 then Error Division_by_zero else Ok (a / b)
          | _ -> Error Parse_error)
      | _ -> Error Parse_error)

(* handle command "calc a op b" *)
let handle_calc_command (text : string) : string option =
  let prefix = "calc " in
  (* check if the text starts with this prefix *)
  if String.length text < String.length prefix then None
  else if String.sub text 0 (String.length prefix) <> prefix then None
  else
    let expr =
      String.sub text (String.length prefix)
        (String.length text - String.length prefix)
    in
    match eval_simple_expr expr with
    | Ok v -> Some ("The answer is: " ^ string_of_int v)
    | Error Division_by_zero -> Some "Error: division by zero"
    | Error Parse_error ->
        Some "I can't make out the expression. Example: calc 10 + 5"

(* ===== Interpreter's functions ===== *)
(* check if a "pattern" matches the text message *)
let matches (p : pattern) (text : string) : bool =
  match p with
  | Any -> true (* Any message is suitable *)
  | Exact s -> String.equal text s
  | Contains sub ->
      let len_t = String.length text in
      let len_s = String.length sub in
      let rec loop i =
        if i + len_s > len_t then false
        else if String.sub text i len_s = sub then true
        else loop (i + 1)
      in
      if len_s = 0 then true else loop 0
  | Starts_with pref ->
      let lt = String.length text in
      let lp = String.length pref in
      lp <= lt && String.sub text 0 lp = pref

(* find the bot's state by name *)
let find_state (b : bot) (name : string) : state option =
  List.find_opt (fun st -> String.equal st.name name) b.states

(* find the first rule that matches the message *)
let find_first_matching_rule (rules : rule list) (text : string) : rule option =
  List.find_opt (fun r -> matches r.pattern text) rules

(* Execute actions and return the new bot's state & string list of answers *)
let exec_actions (current_state : string) (text_raw : string)
    (actions : action list) : string * string list =
  List.fold_left
    (fun (st, replies_rev) act ->
      match act with
      | Say f -> (st, f text_raw :: replies_rev)
      | Goto s -> (s, replies_rev))
    (current_state, []) actions

(* ===== Interpreter ===== *)
let interpret (b : bot) (current_state : string) (text_raw : string) :
    string * string =
  let text_cmd = normalize_for_command text_raw in

  match find_state b current_state with
  | None -> (b.initial_state, "Internal error: unknown state, resetting.")
  | Some st -> (
      match find_first_matching_rule st.rules text_cmd with
      | None -> (current_state, "Internal error: no rule matched.")
      | Some r ->
          let next_state, replies_rev =
            exec_actions current_state text_raw r.actions
          in
          let replies = List.rev replies_rev in
          let reply_text =
            match replies with [] -> "" | xs -> String.concat "\n" xs
          in
          (next_state, reply_text))

(* ===== Init chatbot ===== *)
let my_bot =
  chatbot "start"
    [
      state "start"
        [
          rule (Exact "hi")
            [ say "Hi! What is your name?\n-----\n(＾▽＾)"; goto "ask_name" ];
          rule Any [ say "Write 'hi' to get started\n-----\n(O_O)" ];
        ];
      state "ask_name"
        [
          rule (Exact "hi")
            [
              say "OK, let's start over. What's your name?\n-----\n(＾▽＾)";
              goto "ask_name";
            ];
          rule Any
            [
              sayf (fun name -> "Nice to meet you, " ^ name ^ "!");
              say
                "Write 'help' and I will show you the list of commands\n\
                 -----\n\
                 (づ｡◕‿‿◕｡)づ";
              goto "main";
            ];
        ];
      state "main"
        [
          rule (Exact "hi")
            [
              say "OK, let's start over. What's your name?\n-----\n(＾▽＾)";
              goto "ask_name";
            ];
          rule (Exact "help")
            [
              say
                "Available commands:\n\
                 - help       : I'll show you the list of commands;\n\
                 - info       : I'll show you my information;\n\
                 - author     : I'll show you who created me;\n\
                 - calc a + b : I'll calculate your calculation. Operators \
                 such as: (+, -, *, /);\n\
                 - bye        : You can exit;\n\
                 What do you want from me? Just say it!\n\
                 -----\n\
                 ヽ(´▽`)/";
            ];
          rule (Exact "info")
            [
              say
                "I am an OCaml educational chatbot.\n\
                 The logic is described via an eDSL (rules and states)\n\
                 and executed by an interpreter.\n\
                 Integration: Telegram Bot API\n\
                 -----------------------------\n\
                 (≧◡≦) ♡\n\
                 /| |\\\n\
                 | |\n\
                 /   \\\n";
            ];
          rule (Exact "author")
            [
              say
                "Author: Фам Данг Чунг Нгиа - 3rd year student at ITMO \
                 University\n\
                 -----\n\
                 (•̀ᴗ•́)و ̑̑\n\
                 OCaml enthusiast\n";
            ];
          rule (Exact "bye")
            [ say "Goodbye! See you again!\n-----\n(づ｡◕‿‿◕｡)づ"; goto "start" ];
          rule (Starts_with "calc")
            [
              sayf (fun t ->
                  match handle_calc_command t with
                  | Some ans -> ans
                  | None ->
                      "Something is wrong. You can try again.\n\
                       Example: calc 10 + 5\n\
                       -----\n\
                       (•ㅅ•)ノ");
            ];
          rule Any [ say "I don't understand. Let try again\n-----\n(╥﹏╥)" ];
        ];
    ]
