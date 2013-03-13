
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
                    (Curl.strerror curlcode) n s)
    | e -> let e = Printexc.to_string e in failwith "Error: "^e;;


let p = Yojson.Safe.from_string (string_of_uri "http://api.allocine.fr/rest/v3/movie?partner=YW5kcm9pZC12M3M&format=json&code=199070") ;;


(* Bonne fonction pr recup runtime (additionne ts les runtime) *)
let rec fon1 j cle= match j with 
| `Assoc l -> fon2 l
| `Int n -> n
| `List l | `Tuple l -> fon2 l
| `Variant (s, jo) -> match jo with
    | Some j -> fon1 j cle
    | None -> 0
| _ -> 0
and fon2 l = match l with
| (s,j)::r when s = cle -> fon1 j cle
| (s,`Int n):: r -> fon2 r
| (s,j)::r -> (fon1 j cle)+(fon2 r)
| [] -> 0;;

(* Titre du film *)



