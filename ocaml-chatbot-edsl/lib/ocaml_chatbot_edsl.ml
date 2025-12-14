(* Pattern to match user input *)
type pattern =
  | Exact of string
  | Contains of string
  | Starts_with of string
  | Any

(* Action performed by the bot *)
type action =
  | Say of (string -> string) (* builds reply using user message *)
  | Goto of string

(* Rule: pattern + list of actions *)
type rule = { pattern : pattern; actions : action list }

(* State of chatbot *)
type state = { name : string; rules : rule list }

(* Chatbot definition *)
type bot = { initial_state : string; states : state list }

(** DSL helpers : giúp DSL đọc giống ngôn ngữ khai báo *)
let rule pattern actions = { pattern; actions }

let state name rules = { name; rules }
let chatbot initial_state states = { initial_state; states }

(* trả lời cố định (không phụ thuộc tin nhắn người dùng) *)
let say text = Say (fun _ -> text)

(* trả lời động (có dùng nội dung user vừa nhập) *)
let sayf f = Say f
let goto state_name = Goto state_name

(* ===== Step 3: Interpreter ===== *)

(* kiểm tra xem một “pattern” (mẫu) có khớp với nội dung tin nhắn text hay không *)
let matches (p : pattern) (text : string) : bool =
  match p with
  | Any -> true
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

(* return bot's states *)
let find_state (b : bot) (name : string) : state option =
  List.find_opt (fun st -> String.equal st.name name) b.states

let find_first_matching_rule (rules : rule list) (text : string) : rule option =
  List.find_opt (fun r -> matches r.pattern text) rules

(* Execute actions in an immutable way *)
let exec_actions (current_state : string) (text_raw : string)
    (actions : action list) : string * string list =
  List.fold_left
    (fun (st, replies_rev) act ->
      match act with
      | Say f -> (st, f text_raw :: replies_rev)
      | Goto s -> (s, replies_rev))
    (current_state, []) actions
(* ===== Core utils ===== *)

let string_trim (s : string) : string =
  let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in
  let n = String.length s in
  let rec left i = if i < n && is_space s.[i] then left (i + 1) else i in
  let rec right i = if i >= 0 && is_space s.[i] then right (i - 1) else i in
  let l = left 0 in
  let r = right (n - 1) in
  if r < l then "" else String.sub s l (r - l + 1)

let unicode_lower (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let add_uchar (u : Uchar.t) = Uutf.Buffer.add_utf_8 buf u in

  let add_mapping (u : Uchar.t) =
    match Uucp.Case.Map.to_lower u with
    | `Self -> add_uchar u
    | `Uchars us -> List.iter add_uchar us
  in

  let dec = Uutf.decoder (`String s) in
  let rec loop () =
    match Uutf.decode dec with
    | `Uchar u ->
        add_mapping u;
        loop ()
    | `End -> ()
    | `Malformed _ ->
        (* Replacement character for malformed UTF-8 *)
        add_uchar Uutf.u_rep;
        loop ()
    | `Await -> ()
  in
  loop ();
  Buffer.contents buf

(* Apply to command matching only (safe) *)
let normalize_for_command (s : string) : string =
  (* lowercasing is ASCII-only; safe for Latin aliases, numbers, operators *)
  s |> string_trim |> unicode_lower

let interpret (b : bot) (current_state : string) (text_raw : string) :
    string * string =
  (* Only for rule matching *)
  let text_cmd = normalize_for_command text_raw in

  match find_state b current_state with
  | None -> (b.initial_state, "Internal error: unknown state, resetting.")
  | Some st -> (
      match find_first_matching_rule st.rules text_cmd with
      | None -> (current_state, "Internal error: no rule matched.")
      | Some r ->
          (* IMPORTANT: pass raw text into Say f, so names keep original case *)
          let next_state, replies_rev =
            exec_actions current_state text_raw r.actions
          in
          let replies = List.rev replies_rev in
          let reply_text =
            match replies with [] -> "" | xs -> String.concat "\n" xs
          in
          (next_state, reply_text))

(* ===== Step 6.4.2: Simple calculator for "вычисли a op b" ===== *)

type calc_error = Parse_error | Division_by_zero

let string_trim (s : string) : string =
  let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in
  let n = String.length s in
  let rec left i = if i < n && is_space s.[i] then left (i + 1) else i in
  let rec right i = if i >= 0 && is_space s.[i] then right (i - 1) else i in
  let l = left 0 in
  let r = right (n - 1) in
  if r < l then "" else String.sub s l (r - l + 1)

let parse_int (s : string) : int option =
  try Some (int_of_string (string_trim s)) with _ -> None

let find_first_op (s : string) : (int * char) option =
  (* Find first operator among + - * / (excluding leading sign) *)
  let n = String.length s in
  let is_op c = c = '+' || c = '-' || c = '*' || c = '/' in
  let rec loop i =
    if i >= n then None
    else
      let c = s.[i] in
      if is_op c then Some (i, c) else loop (i + 1)
  in
  if n > 0 && s.[0] = '-' then loop 1 else loop 0

let eval_simple_expr (expr : string) : (int, calc_error) result =
  let e = string_trim expr in
  match find_first_op e with
  | None -> Error Parse_error
  | Some (idx, op) -> (
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

let handle_calc_command (text : string) : string option =
  (* Accept: "вычисли <expr>" where expr is "a op b" *)
  let prefix = "вычисли " in
  if String.length text < String.length prefix then None
  else if String.sub text 0 (String.length prefix) <> prefix then None
  else
    let expr =
      String.sub text (String.length prefix)
        (String.length text - String.length prefix)
    in
    match eval_simple_expr expr with
    | Ok v -> Some ("Результат: " ^ string_of_int v)
    | Error Division_by_zero -> Some "Ошибка: деление на ноль"
    | Error Parse_error ->
        Some "Не могу разобрать выражение. Пример: вычисли 10 + 5"

(* ===== Example chatbot definition ===== *)

let example_bot =
  chatbot "start"
    [
      state "start"
        [
          rule (Exact "привет")
            [ say "Привет! Как тебя зовут?"; goto "ask_name" ];
          rule Any [ say "Напишите 'привет', чтобы начать" ];
        ];
      state "ask_name"
        [
          rule (Exact "привет")
            [ say "Ок, начнём сначала. Как тебя зовут?"; goto "ask_name" ];
          rule Any
            [
              sayf (fun name -> "Приятно познакомиться, " ^ name ^ "!");
              say "Напишите 'помоги' для списка команд";
              goto "main";
            ];
        ];
      state "main"
        [
          rule (Exact "привет")
            [ say "Ок, начнём сначала. Как тебя зовут?"; goto "ask_name" ];
          rule (Exact "помоги")
            [
              say
                "Доступные команды:\n\
                 - помоги   : покажу список команд\n\
                 - инфо   : покажу мою информацию\n\
                 - автор : покажу того, кто мне создал\n\
                 - вычисли a + b : вычислю ваш расчет. Операторы такие как: \
                 (+, -, *, /)\n\
                 - пока    : можно выйти (вернуться в начало)";
            ];
          rule (Exact "инфо")
            [
              say
                "Я учебный чат-бот на OCaml. Логика описана через eDSL \
                 (правила и состояния) и выполняется интерпретатором. \
                 Интеграция: Telegram Bot API.";
            ];
          rule (Exact "автор")
            [
              say
                "Автор: Фам Данг Чунг Нгиа - студент 3 курса ИТМО. \
                 Лабораторная работа №4 (eDSL для чат-бота).";
            ];
          rule (Exact "пока") [ say "Пока! Возвращайся :)"; goto "start" ];
          rule (Starts_with "вычисли ")
            [
              sayf (fun t ->
                  match handle_calc_command t with
                  | Some ans -> ans
                  | None -> "Пример: вычисли 10 + 5");
            ];
          rule (Starts_with "вычисли ")
            [
              sayf (fun t ->
                  match handle_calc_command t with
                  | Some ans -> ans
                  | None -> "Пример: вычисли 10 + 5");
            ];
          rule Any [ say "Я тебя не понимаю" ];
        ];
    ]
