(* Pattern to match user input *)
type pattern =
  | Exact of string
  | Contains of string
  | Any

  (* Action performed by the bot *)
type action =
  | Say of string
  | Goto of string

(* Rule: pattern + list of actions *)
type rule = {
  pattern : pattern;
  actions : action list;
}

(* State of chatbot *)
type state = {
  name : string;
  rules : rule list;
}

(* Chatbot definition *)
type bot = {
  initial_state : string;
  states : state list;
}

(**  DSL helpers : giúp DSL đọc giống ngôn ngữ khai báo *)
let rule pattern actions =
  { pattern; actions }

let state name rules =
  { name; rules }

let chatbot initial_state states =
  { initial_state; states }

let say text = Say text
let goto state_name = Goto state_name

(* ===== Example chatbot definition ===== *)

let example_bot =
  chatbot "start" [
    state "start" [
      rule (Exact "привет") [ say "Здравствуйте!" ];
      rule (Contains "имя") [ say "Я учебный чат-бот." ];
      rule Any               [ say "Извините, я вас не понял." ];
    ]
  ]

(* ===== Step 3: Interpreter ===== *)

(* kiểm tra xem một “pattern” (mẫu) có khớp với nội dung tin nhắn text hay không *)
let matches (p : pattern) (text : string) : bool =
  match p with
  | Any -> true (* accept random input -> fallback rule *)
  | Exact s -> String.equal text s (* chỉ khớp nếu tin nhắn bằng đúng chuỗi s *)
  | Contains sub ->
      (* simple substring check *)
      let len_t = String.length text in
      let len_s = String.length sub in
      let rec loop i =
        if i + len_s > len_t then false
        else if String.sub text i len_s = sub then true
        else loop (i + 1)
      in
      if len_s = 0 then true else loop 0

(* return bot's states *)
let find_state (b : bot) (name : string) : state option =
  List.find_opt (fun st -> String.equal st.name name) b.states

let find_first_matching_rule (rules : rule list) (text : string) : rule option =
  List.find_opt (fun r -> matches r.pattern text) rules

(* Execute actions in an immutable way *)
let exec_actions (current_state : string) (actions : action list)
  : (string * string list) =
  List.fold_left
    (fun (st, replies_rev) act ->
      match act with
      | Say s -> (st, s :: replies_rev)
      | Goto s -> (s, replies_rev))
    (current_state, [])
    actions

let interpret (b : bot) (current_state : string) (text : string)
  : (string * string) =
  match find_state b current_state with
  | None ->
      (b.initial_state, "Internal error: unknown state, resetting.")
  | Some st -> (
      match find_first_matching_rule st.rules text with
      | None ->
          (current_state, "Internal error: no rule matched.")
      | Some r ->
          let (next_state, replies_rev) = exec_actions current_state r.actions in
          let replies = List.rev replies_rev in
          let reply_text =
            match replies with
            | [] -> ""
            | xs -> String.concat "\n" xs
          in
          (next_state, reply_text)
    )

