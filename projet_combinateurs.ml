(**
   Projet de programmation fonctionnelle M1
   Souhire KENAWI - Hai NGUYEN VAN
*)

#use "topfind";;
#require "curl";;
#require "yojson";;

type location = float * float;;

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

(**
   L'utilisateur donne sa position géographique à l'aide d'un emplacement informel (nom de rue, métro)
*)

(*  Une instance possible d'AST JSON par Google Places pour la position géographique d'un emplacement informel *)

let adresse_ex = "https://maps.googleapis.com/maps/api/place/textsearch/json?sensor=true&query=rue+camille+desmoulins+cachan&key=AIzaSyA2kG_PpWlQG91CVNXKfyeaKdxbuUK-CeE";;


(* Position géographique (latitude, longitude) à partir d'un emplacement informel *)
let geographic_location_of_informal_location (l : string) : float * float =
  (* a changer en attendant le support de HTTPS *)
  let uri = "https://maps.googleapis.com/maps/api/place/textsearch/json?sensor=true&query="^l^"&key=AIzaSyA2kG_PpWlQG91CVNXKfyeaKdxbuUK-CeE" in
  let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
  match json_ast with
    | `Assoc (l) -> (
      match (List.assoc "results" l) with
	| `List ((`Assoc l') :: _) -> (
	  match (List.assoc "geometry" l') with
	    | `Assoc l'' -> (
	      match (List.assoc "location" l'') with 
		| `Assoc (("lat", `Float f1) :: ("lng", `Float f2) :: _) -> (f1, f2)
		| _ -> failwith "AST problem"
	    )
	    | _ -> failwith "AST problem"
	)
	| _ -> failwith "AST problem"
    )
    | _ -> failwith "AST problem"
;;

(**
   Googles Places renvoie tous les restaurants à un emplacement géographique suivant un rayon précis
*)

(*
  Une instance possible d'AST JSON par Google Places pour la position géographique d'un restaurant proche d'un emplacement géographique *)

let restaurants_example = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?language=fr&location=48.7928702,2.3331357&radius=100&sensor=true&key=AIzaSyA2kG_PpWlQG91CVNXKfyeaKdxbuUK-CeE&types=restaurant";;

let restaurants_at_geographic_location (emplacement : location) (radius : int) : float * float * string * string =
  (* a changer en attendant le support de HTTPS *)
  (* regler le pb de choisir l'emplacement le plus et NON le plus "interessant" *)
  let uri = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?language=fr&location="^string_of_float(fst emplacement)^","^string_of_float(snd emplacement)^"&radius="^string_of_int radius^"&sensor=true&key=AIzaSyA2kG_PpWlQG91CVNXKfyeaKdxbuUK-CeE&types=restaurant" in
  let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
  let (lat_location, lng_location) = match json_ast with
    | `Assoc (l) -> (
      match (List.assoc "results" l) with
	| `List ((`Assoc l') :: _) -> (
	  match (List.assoc "geometry" l') with
	    | `Assoc l'' -> (
	      match (List.assoc "location" l'') with 
		| `Assoc (("lat", `Float f1) :: ("lng", `Float f2) :: _) -> (f1, f2)
		| _ -> failwith "AST problem"
	    )
	    | _ -> failwith "AST problem"
	)
	| _ -> failwith "AST problem"
    )
    | _ -> failwith "AST problem"
  and name = match json_ast with
    | `Assoc (l) -> (
      match (List.assoc "results" l) with
	| `List ((`Assoc l') :: _) -> (
	  match (List.assoc "name" l') with
	    | `String s -> s
	    | _ -> failwith "AST problem"
	)
	| _ -> failwith "AST problem"
    )
    | _ -> failwith "AST problem"
  and address = match json_ast with
    | `Assoc (l) -> (
      match (List.assoc "results" l) with
	| `List ((`Assoc l') :: _) -> (
	  match (List.assoc "vicinity" l') with
	    | `String s -> s
	    | _ -> failwith "AST problem"
	)
	| _ -> failwith "AST problem"
    )
    | _ -> failwith "AST problem"
  in (lat_location, lng_location, name, address)
;;

(**
   Tests et debug
*)
geographic_location_of_informal_location "";;
allocine_id_of_movie_name movie_name;;
restaurants_at_geographic_location (0., 0.) 0;;



(* Information sur les cinemas  *)

(* let cine_name_of_approx_location loca;;  *)


let allocine_code_of_cine_name cine_name = 
  let uri = "http://api.allocine.fr/rest/v3/search?partner=YW5kcm9pZC12M3M&q="^cine_name^"&format=json&filter=theater" in
  let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
  




(* Modules *)

(*
 module pour les films
 -> table de hashage 
 module pour les personnes
 module spectable 
 -> horaire debut et fin
 
 Chercher un time pour le temps....

 module pour le restaurant 
  *)


(*

Module Film : id, titre, durée, acteurs, nationalité
Module Séance: hash_film, heure_deb, hash_cinema, VO/VF
Module Cinema : id, nom, addr, pos
Module Personne : choix, (heure, marge), (lieu, marge), nom
Module Restaurant : nom, addr, pos, id

*)

module type FilmSig =
    sig
      type t
      val create : string -> t
      val getTitle : t -> string
      val getRuntime : t -> int
      val getActors : t -> string
      val getNationality : t -> string list
      (* val show : t -> string *)
    end;;

module Film : FilmSig =
  struct
    type t = {title : string ; runtime : int ; actors : string ; nationality : string list}
    let create movie_name = 
      let id = allocine_id_of_movie_name movie_name in
      let p = Yojson.Safe.from_string (
	string_of_uri (
	   "http://api.allocine.fr/rest/v3/movie?partner=YW5kcm9pZC12M3M&format=json&code=" ^ (string_of_int id))) in
      {title = title_of_allocine_json p;
       runtime = runtime_of_allocine_json p;
       actors = "";
       nationality = nationality_of_allocine_json p}
    let getTitle f = f.title
    let getRuntime f = f.runtime
    let getActors f = f.actors
    let getNationality f = f.nationality
    (* let show f = 
      let infos = "Le titre du film est : " ^ f.title 
      ^ "\n Durée du film : " ^ (string_of_int f.runtime)
      ^ "\n Acteurs : " ^ f.actors
      ^ "\n Nationalité : " ^ (List.iter print_string f.nationality) 
      in
      print_string infos *)
  end;;


let test = Film.create "monde+oz";;
Film.getTitle test;;
Film.getRuntime test;;
Film.getNationality test;;


module type CineSig =
  sig
    type t
    val create : unit -> t
    val getId : t -> int
    val getName : t -> string
    val getAddr : t -> string
    val getPos : t -> location
  end;;

module Cine =
  struct
    type t = { id : int; name : string; addr : string; pos: location}
    let create id =   


module type SeanceSig =
  sig
    type t
    val create : Film.t -> Cine.t -> t
    val getBegin : t -> int
    val getLang : t -> bool
    (* val show : t -> unit *)
  end;;

module Seance : SeanceSig =
  struct
    type t = { beg : int ; lang : bool }
    let create f c -> 
    let getBegin s = s.beg
    let getLang s = s.lang
    let 


module type PersonSig = 
  sig 
    type t
    val create : unit -> t
    val getName : t -> string
  end;;

module type RestauSig =
  sig
    type t
    val create : unit -> t
    val getId : t -> int
    val getName : t -> string
    val getAddr : t -> string
    val getPos : t -> (float * float)
  end;;


module Film : FilmSig =
  struct
    type t = { title : string ; runtime : int }
    let getTitle f = f.title
    let getRuntime f = f.runtime
    let create titre duree = { title = titre ; runtime = duree }
end;;





(* Titre du film *)


(*
 Moi : Modules, réfléchir à google places pour récupérer restaurants, bars...
 Hai : *_of_allocine_json, réfléchir à google places en géolocalisation
  *)
