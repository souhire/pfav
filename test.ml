(*
let rec fonction_test x = let rec fonc l = match l with
| [] -> 0
| (s,`Int n)::r when s="runtime" -> n
| (_,j)::r -> fonction_test j in
match x with 
| `Assoc l -> fonc l 
| _ -> -1;;

let f1 p = match p with
| `Assoc l -> l
| _ -> failwith "erreur pas assoc";;

let rec f2 l nom = match l with
| (s,j)::r when s=nom -> j
| _::r -> f2 r nom
| [] -> failwith "pas de runtime";;

let f3 j = match j with 
| `Int n -> n
| _ -> failwith "Erreur";;

f3 (f2 (f1 (f2 (f1 p) "movie")) "runtime");;

let fonction_runtime p = f3 (f2 (f1 (f2 (f1 p) "movie")) "runtime");;


let f4 id = fonction_runtime (Yojson.Safe.from_string (string_of_uri ("http://api.allocine.fr/rest/v3/movie?partner=YW5kcm9pZC12M3M&format=json&code="^id))) ;;


let g p = match p with
| (s,x)::r when s="movie" -> 
	(match x with
	| (s', x') when s'="runtime" -> x'  
	| _ -> failwith "erreur no runtime"
	)
| _ -> failwith "erreur";;
*)

#use "topfind";;
#require "curl";;
#require "yojson";;

let string_of_uri ?(headers=[]) ?query uri = 
    let uri = match query with
	  None -> uri
	| Some q -> uri^"?"^q in
    try let connection = Curl.init () and write_buff = Buffer.create 1763 in
        Curl.set_writefunction connection
                (fun x -> Buffer.add_string write_buff x; String.length x);
        Curl.set_url connection uri;
        Curl.set_httpheader connection headers; 
        Curl.perform connection;
        Curl.global_cleanup ();
        Buffer.contents write_buff;
    with 
    | Curl.CurlException(curlcode,n,s) -> 
        failwith (Printf.sprintf "Curl error: %s (error code: %d, error symbol: %s)" 
                    s n s)
    | e -> let e = Printexc.to_string e in failwith "Error: "^e;;


let p = Yojson.Safe.from_string (string_of_uri "http://api.allocine.fr/rest/v3/movie?partner=YW5kcm9pZC12M3M&format=json&code=199070") ;;


(* Bonne fonction pr recup runtime (additionne ts les runtime) *)
let rec runtime_of_json (j : Yojson.Safe.json) : int = match j with 
| `Assoc l -> aux l
| `Int n -> n
(*| `List l | `Tuple l -> List.fold_left (fun e -> runtime_of_json e cle) 0 l *)
| `Variant (s, jo) -> (match jo with
    | Some j -> runtime_of_json j
    | None -> 0)
| _ -> 0
and aux l = match l with
| (s,j)::r when s = "runtime" -> runtime_of_json j
| (s,`Int n):: r -> aux r 
| (s,j)::r -> (runtime_of_json j )+(aux r)
| [] -> 0;;

let rec runtime_of_allocine_json (j : Yojson.Safe.json) : int =
  match j with
    | `Assoc ((_, `Assoc ((cle, valeur) :: l')) :: _) when cle = "runtime" ->
      (match valeur with `Int n -> n | _ -> failwith "bloublou") 
    | `Assoc ((c1, `Assoc (_ :: l')) :: c2) ->
      runtime_of_allocine_json (`Assoc ((c1, `Assoc l') :: c2))
    | _ -> failwith "dkfsdhf"
;;



(* Modules *)

module type FilmSig =
    sig
      type t
      val getTitle : t -> string
      val getRuntime : t -> int
      val create : string -> int -> t
    end;;


module Film : FilmSig =
  struct
    type t = { title : string ; runtime : int }
    let getTitle f = f.title
    let getRuntime f = f.runtime
    let create titre duree = { title = titre ; runtime = duree }
end;;

let film1 : Film.t = Film.create "Les amours gays" 99999999999;;
Film.getTitle film1;;
Film.getRuntime film1;;



(* Titre du film *)


(*
 Moi : Modules, réfléchir à google places pour récupérer restaurants, bars...
 Hai : *_of_allocine_json, réfléchir à google places en géolocalisation
  *)

runtime_of_json p ;;


let rec title_of_json (j : Yojson.Safe.json) : int = match j with 
| `Assoc l -> aux l
| `Int n -> n
(*| `List l | `Tuple l -> List.fold_left (fun e -> runtime_of_json e cle) 0 l *)
| `Variant (s, jo) -> (match jo with
    | Some j -> runtime_of_json j
    | None -> "")
| _ -> 0
and aux l = match l with
| (s,j)::r when s = "runtime" -> runtime_of_json j
| (s,`Int n):: r -> aux r 
| (s,j)::r -> (runtime_of_json j )+(aux r)
| [] -> 0;;
