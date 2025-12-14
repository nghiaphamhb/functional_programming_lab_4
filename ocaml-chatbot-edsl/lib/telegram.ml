open Lwt.Infix

let api_url token method_name =
  "https://api.telegram.org/bot" ^ token ^ "/" ^ method_name

let http_get_json url =
  Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >|= Yojson.Safe.from_string

let http_post_form url params =
  let headers =
    Cohttp.Header.init_with "Content-Type" "application/x-www-form-urlencoded"
  in
  let body =
    params
    |> List.map (fun (k, v) -> (k, [ v ]))
    |> Uri.encoded_of_query |> Cohttp_lwt.Body.of_string
  in
  Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string url)
  >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >|= Yojson.Safe.from_string

type update = { update_id : int; chat_id : int; text : string }

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

let get_updates ~token ~offset =
  let url =
    api_url token "getUpdates" ^ "?timeout=25" ^ "&offset="
    ^ string_of_int offset
  in
  http_get_json url >|= parse_updates

let send_message ~token ~chat_id ~text =
  let url = api_url token "sendMessage" in
  let params = [ ("chat_id", string_of_int chat_id); ("text", text) ] in
  http_post_form url params >|= fun _ -> ()
