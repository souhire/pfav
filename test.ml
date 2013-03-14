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
(*
  Une instance possible d'AST JSON par Google Places pour la position géographique d'un emplacement informel
  https://maps.googleapis.com/maps/api/place/textsearch/json?sensor=true&query=rue%20camille%20desmoulins%20cachan&key=AIzaSyA2kG_PpWlQG91CVNXKfyeaKdxbuUK-CeE
*)
let google_place_of_informal_location_example = "{\"html_attributions\" : [],\"results\" : [{\"formatted_address\" : \"Rue Camille Desmoulins, 94230 Cachan, France\",\"geometry\" : {\"location\" : {\"lat\" : 48.79287020,\"lng\" : 2.33313570}},\"icon\" : \"http://maps.gstatic.com/mapfiles/place_api/icons/geocode-71.png\",\"id\" : \"2cc24e810b79496d378a42193017752ee9386dd7\",\"name\" : \"Rue Camille Desmoulins\",\"reference\" : \"CqQBlAAAAGLxwhv6v7vfsVUC-84upQyOxLHj5CrcvIxZ4fCjVXX0yfvd2vrCx2wdFpLDa-pVMBSNR3DILkQklONvk94cBKB_1_4vYr5X9naQJhi7SNJrHgCJjsxRgK6PoM-10-jpjU_XPGrQn7sSXTxbd8Yu2IkWMUvIcUZlEX4MUsuiQVF8KHsc4S_27qndMDOxg1_dyLg3p3DiIr5BdAN5A1CpP0ASEBq770oZbf0Ii3QqfxtRpa0aFOihM0ApZOZw9nCBw-WBQyTs39uH\",\"types\" : [ \"route\" ]}],\"status\" : \"OK\"}";;

(* Position géographique (latitude, longitude) à partir d'un emplacement informel *)
let geographic_location_of_informal_location (l : string) : float * float =
  (* a changer en attendant le support de HTTPS *)
  let json_ast = Yojson.Safe.from_string google_place_of_informal_location_example in
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
  Une instance possible d'AST JSON par Google Places pour la position géographique d'un restaurant proche d'un emplacement géographique
  https://maps.googleapis.com/maps/api/place/nearbysearch/json?language=fr&location=48.7928702,2.3331357&radius=100&sensor=true&key=AIzaSyA2kG_PpWlQG91CVNXKfyeaKdxbuUK-CeE&types=restaurant
*)
let restaurants_example = "{\"html_attributions\" : [],\"results\" : [{\"geometry\" : {\"location\" : {\"lat\" : 48.7936230,\"lng\" : 2.3334760}},\"icon\" : \"http://maps.gstatic.com/mapfiles/place_api/icons/restaurant-71.png\",\"id\" : \"8cad79432858e1a9629c6f370b4d1da5b7a1c392\",\"name\" : \"La Maison des Pizzas\",\"reference\" : \"CoQBcgAAAHP9UbDj2XB5MPvHOHrtv3IueOAVfL-va8apcyDF17qrM48KbmU_ebDzentFZf_EsmUYiSI4OiOLgT0USGQ4rNkptI3wd3p0w1_bBkv69mgYJVbEOZ3TiJvlARBs7Tt9L0uwPrTlfrWMfkAyDPlM6aCYJNdKmM8wax3dWbwxlbIfEhBahtZkX1s6l2iIprQmAXwbGhSRdna5Krw6dDchgkN6wECVevKjcg\",\"types\" : [ \"restaurant\", \"food\", \"establishment\" ],\"vicinity\" : \"26 Rue Camille Desmoulins, Cachan\"}],\"status\" : \"OK\"}";;

let restaurants_at_geographic_location (emplacement : location) (radius : int) : float * float * string * string =
  (* a changer en attendant le support de HTTPS *)
  (* regler le pb de choisir l'emplacement le plus et NON le plus "interessant" *)
  let json_ast = Yojson.Safe.from_string restaurants_example in
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

