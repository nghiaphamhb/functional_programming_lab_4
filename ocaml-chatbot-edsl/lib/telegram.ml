open Lwt.Infix

(* Builds the base URL of the Telegram Bot API *)
let api_url token method_name =
  "https://api.telegram.org/bot" ^ token ^ "/" ^ method_name

(* Make an HTTP GET and return JSON *)
let http_get_json url =
  Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= fun (_resp, body) ->
  (* get response *)
  Cohttp_lwt.Body.to_string body >|= Yojson.Safe.from_string

(* Make an HTTP POST and return JSON *)
let http_post_form url params =
  (* prepare HTTP form *)
  let headers =
    Cohttp.Header.init_with "Content-Type" "application/x-www-form-urlencoded"
  in
  let body =
    params
    |> List.map (fun (k, v) -> (k, [ v ]))
    |> Uri.encoded_of_query |> Cohttp_lwt.Body.of_string
  in
  (* send an HTTP POST *)
  Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string url)
  >>= fun (_resp, body) ->
  (* get response *)
  Cohttp_lwt.Body.to_string body >|= Yojson.Safe.from_string

(* record of Telegram updates:*)
type update = { update_id : int; chat_id : int; text : string }

(* parse the Telegram getUpdates response and get the message list *)
let parse_updates (json : Yojson.Safe.t) : update list =
  let open Yojson.Safe.Util in
  let results = json |> member "result" |> to_list in
  results
  |> List.filter_map (fun u ->
         try
           let upd_id = u |> member "update_id" |> to_int in
           let msg = u |> member "message" in
           let chat = msg |> member "chat" in
           let chat_id = chat |> member "id" |> to_int in
           let text = msg |> member "text" |> to_string in
           Some { update_id = upd_id; chat_id; text }
         with _ -> None)

(* ==== Adapter's general actions ==== *)
(* get new message list from user in telegram *)
let get_updates ~token ~offset =
  let url =
    api_url token "getUpdates" ^ "?timeout=25" ^ "&offset="
    ^ string_of_int offset
  in
  http_get_json url >|= parse_updates

(* send message to user *)
let send_message ~token ~chat_id ~text =
  let url = api_url token "sendMessage" in
  let params = [ ("chat_id", string_of_int chat_id); ("text", text) ] in
  http_post_form url params >|= fun _ -> ()
