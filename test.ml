#use "topfind";;
#require "curl";;
#require "yojson";;

(* Fonction string_of_uri *)
(* Copyright RDC *)
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
    | e -> let e = Printexc.to_string e in failwith "Error: "^e
;;

(**
   utilisateur donne nom d'un film
   -> récupérer l'identifiant du film sur Allociné
*)
let movie_name = "amours%20imaginaires";;

(* Fonction récupérant un identifiant Allociné à partir d'un nom de film possible entré par l'utilisateur *)
let allocine_id_of_movie_name movie_name =
  let json_ast =
    Yojson.Safe.from_string
      (string_of_uri ("http://api.allocine.fr/rest/v3/search?partner=YW5kcm9pZC12M3M&filter=movie&format=json&q=" ^ movie_name)) in
  match json_ast with
    | `Assoc (("feed", `Assoc l) :: _) ->
      let movie_field = List.assoc "movie" l in (
	match movie_field with
	  | `List ((`Assoc l') :: _) ->
	    let code = List.assoc "code" l' in (
	      match code with
		| `Int n -> n
		| _ -> failwith "AST problem"
	    )
	  | _ -> failwith "AST problem"
      )
    | _ -> failwith "Feed field not found"
;;

(* Exemple d'arbre de syntaxe abstraite d'un film *)
let movie_id = allocine_id_of_movie_name movie_name;;
let p = Yojson.Safe.from_string (
  string_of_uri (
    "http://api.allocine.fr/rest/v3/movie?partner=YW5kcm9pZC12M3M&format=json&code=" ^ (string_of_int movie_id)
  )
);;

(**
   Fonctions récupérant les informations de base à partir de l'AST de type JSON d'un film sur Allociné
*)

(* Fonction runtime_of_allocine_json *)
(* Récupère la durée de film à partir d'un AST JSON d'un film sur Allocine *)
let rec runtime_of_allocine_json (j : Yojson.Safe.json) : int =
  match j with
    | `Assoc ((_, `Assoc (("runtime", valeur) :: l')) :: _) ->
      (match valeur with `Int n -> n | _ -> failwith "AST problem") 
    | `Assoc ((c1, `Assoc (_ :: l')) :: c2) ->
      runtime_of_allocine_json (`Assoc ((c1, `Assoc l') :: c2))
    | _ -> failwith "Runtime field not found"
;;

(* Fonction title_of_allocine_json *)
(* Récupère le titre du film à partir d'un AST JSON d'un film sur Allocine *)
let rec title_of_allocine_json (j : Yojson.Safe.json) : string =
  match j with
    | `Assoc ((_, `Assoc (("title", valeur) :: l')) :: _) ->
      (match valeur with `String s -> s | _ -> failwith "AST problem") 
    | `Assoc ((c1, `Assoc (_ :: l')) :: c2) ->
      title_of_allocine_json (`Assoc ((c1, `Assoc l') :: c2))
    | _ -> failwith "Title field not found"
;;

(* Fonction production_year_of_allocine_json *)
(* Récupère l'année de production du film à partir d'un AST JSON d'un film sur Allocine *)
let rec production_year_of_allocine_json (j : Yojson.Safe.json) : int =
  match j with
    | `Assoc ((_, `Assoc (("productionYear", valeur) :: l')) :: _) ->
      (match valeur with `Int n -> n | _ -> failwith "AST problem") 
    | `Assoc ((c1, `Assoc (_ :: l')) :: c2) ->
      production_year_of_allocine_json (`Assoc ((c1, `Assoc l') :: c2))
    | _ -> failwith "Production year field not found"
;;

(* Fonction nationality_of_allocine_json *)
(* Récupère le(s) nationalité(s) du film à partir d'un AST JSON d'un film sur Allocine *)
let rec nationality_of_allocine_json (j : Yojson.Safe.json) : string list =
  let rec un_assoc c = match c with
    | `Assoc (("$", `String s) :: _) -> s
    | `Assoc ((_, _) :: c') -> un_assoc (`Assoc c')
    | `Assoc [] -> failwith "AST problem"
    | _ -> failwith "AST problem" in
  match j with
    | `Assoc ((_, `Assoc (("nationality", `List l) :: _)) :: _) ->
      List.map (un_assoc) l
    | `Assoc ((c1, `Assoc (_ :: l')) :: c2) ->
      nationality_of_allocine_json (`Assoc ((c1, `Assoc l') :: c2))
    | _ -> failwith "Nationality field not found"
;;

(* Fonction genre_of_allocine_json *)
(* Récupère le(s) genre(s) du film à partir d'un AST JSON d'un film sur Allocine *)
let rec genre_of_allocine_json (j : Yojson.Safe.json) : string list =
  let rec un_assoc c = match c with
    | `Assoc (("$", `String s) :: _) -> s
    | `Assoc ((_, _) :: c') -> un_assoc (`Assoc c')
    | `Assoc [] -> failwith "AST problem"
    | _ -> failwith "AST problem" in
  match j with
    | `Assoc ((_, `Assoc (("genre", `List l) :: _)) :: _) ->
      List.map (un_assoc) l
    | `Assoc ((c1, `Assoc (_ :: l')) :: c2) ->
      genre_of_allocine_json (`Assoc ((c1, `Assoc l') :: c2))
    | _ -> failwith "Genre field not found"
;;



allocine_id_of_movie_name movie_name;;


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

