(**
   Projet de programmation fonctionnelle M1
   Souhire KENAWI - Hai NGUYEN VAN
*)

#use "topfind";;
#require "curl";;
#require "yojson";;

type location = float * float;;
type heure = int * int;;
type date = int * int * int;;
type plage = heure * heure;;
type horaire = string * plage;;

(* Pretty string pour une date *)
let string_of_date (delimiter : string) ((jj, mm, aaaa) : date) : string =
  let jour_string = if jj < 10 then "0" ^ string_of_int jj else string_of_int jj
  and mois_string = if mm < 10 then "0" ^ string_of_int mm else string_of_int mm
  and annee_string = string_of_int aaaa
  in annee_string ^ delimiter ^ mois_string ^ delimiter ^ jour_string
;;

(* Little parser vers un temps horaire *)
(* Exemple : "10:24" -> (10, 24) *)
let heure_of_string (s : string) : heure =
  (int_of_string (String.sub s 0 2), int_of_string (String.sub s 3 2))
;;

(* Ordre lexicographique sur N × N *)
let (@<) ((hh1, mm1) : heure) ((hh2, mm2) : heure) =
  if (hh1 < hh2) then true
  else if (hh2 = hh1)
  then mm1 < mm2
  else false
;;

let (@<=) t1 t2 = (t1 @< t2) || (t1 = t2);;


(**
   Déclaration des modules
*)

(*
Module Film : id, titre, durée, acteurs, nationalité
Module Séance: hash_film, heure_deb, hash_cinema, VO/VF
Module Cinema : id, nom, addr, pos
Module Personne : choix, (heure, marge), (lieu, marge), nom
Module Restaurant : nom, addr, pos, id
*)

(* Module d'une salle de projection *)
module type CineSig =
  sig
    type t
    val create : string -> t
    val getId : t -> int
    val getName : t -> string
    val getAddr : t -> string
    val getPos : t -> location
  end;;

module Cine (* : CineSig *) =
  struct
    type t = { id : string ; name : string ; addr : string ; pos: location}
    let create cine_id cine_name cine_addr cine_pos =
	{
	  id   = cine_id ;
	  name = cine_name ;
	  addr = cine_addr ;
	  pos  = cine_pos 
	}
    let getId f = f.id
    let getName f = f.name
    let getAddr f = f.addr
    let getPos f = f.pos
  end
;;

(* Module d'un film produit *)
module type FilmSig =
    sig
      type t
      val create : int -> string -> int -> string -> string list -> t
      val getTitle : t -> string
      val getRuntime : t -> int
      val getActors : t -> string
      val getNationality : t -> string list
      val show : t -> unit 
    end;;

module Film (* : FilmSig *) =
  struct
    type t = { id : int ; title : string ; runtime : int ; actors : string ; nationality : string list}
    let create movie_id movie_title movie_runtime movie_actors movie_nationality =
      {
	id = movie_id ;
	title = movie_title ;
	runtime = movie_runtime ;
	actors = movie_actors ;
	nationality = movie_nationality
      }
(*
    let informal_create movie_name = 
      let id = allocine_id_of_movie_name movie_name in
      let p = Yojson.Safe.from_string (
	string_of_uri (
	   "http://api.allocine.fr/rest/v3/movie?partner=E00024954332&format=json&code=" ^ (string_of_int id))) in
      {title = title_of_allocine_json p;
       runtime = runtime_of_allocine_json p;
       actors = "";
       nationality = nationality_of_allocine_json p}
*)
    let getId f = f.id
    let getTitle f = f.title
    let getRuntime f = f.runtime
    let getActors f = f.actors
    let getNationality f = f.nationality
    let show f =
      let infos = "Le titre du film est : " ^ f.title 
	^ "\n Durée du film : " ^ (string_of_int f.runtime)
	^ "\n Acteurs : " ^ f.actors
	^ "\n Nationalité : " ^ (String.concat " " f.nationality) 
      in
      print_string infos 
  end;;


(*
let test = Film.create "monde+oz";;
Film.getTitle test;;
Film.getRuntime test;;
Film.getNationality test;;
Film.show test;;
*)

(* Module d'une séance de projection d'un film *)
module type SeanceSig =
  sig
    type t
    val create : int -> Film.t -> Cine.t -> date -> heure -> bool option -> bool option -> t
    val getId : t -> int
    val getDate : t -> heure
    val getBeginTime : t -> heure
    val is_vo : t -> bool option
    val is_3d : t -> bool option
    (* val show : t -> unit *)
  end;;

module Seance (* : SeanceSig *) =
  struct
    type t = { id : int ; film : Film.t ; cinema : Cine.t ; date : date ; begin_time : heure ; is_vo : bool option ; is_3d : bool option }
    let create seance_id seance_film seance_cinema seance_date seance_begin_time seance_is_vo seance_is_3d = 
      {
	id = seance_id ;
	film = seance_film ;
	cinema = seance_cinema ;
	date = seance_date ;
	begin_time = seance_begin_time ;
	is_vo = seance_is_vo ;
	is_3d = seance_is_3d
      }
    let getId f = f.id
    let getDate f = f.date
    let getBeginTime f = f.begin_time
    let isVO f = f.is_vo
    let is3D f = f.is_3d
  end;;

(* Module d'un utilisateur *)
module type PersonSig = 
sig 
  type t
  val create : unit -> t
  val getName : t -> string
end;;

(* Module d'un lieu de restauration *)
module type RestauSig =
sig
  type t
  val create : string -> string -> string -> location -> horaire list -> t
  val getId : t -> string
  val getName : t -> string
  val getAddr : t -> string
  val getPos : t -> location
  val getTime : t -> horaire list
  val show : t -> unit
end;;

module Restau (* : RestauSig *) =
struct
  type t = { id : string ; name : string ; addr : string ; pos : location; time : horaire list}
  let create restau_id restau_name restau_addr restau_pos restau_time =
    { id = restau_id;
      name = restau_name;
      addr = restau_addr;
      pos = restau_pos;
      time = restau_time}
  let getId r = r.id
  let getName r = r.name
  let getAddr r = r.addr
  let getPos r = r.pos
  let getTime r = r.time
  let show r =
    print_string ("Le restau est "^getName r^"\n Addresse : "^getAddr r^"\n")
end;;


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
let allocine_id_of_movie_name (movie_name : string) : int =
  let json_ast =
    Yojson.Safe.from_string
      (string_of_uri ("http://api.allocine.fr/rest/v3/search?partner=E00024954332&filter=movie&format=json&q=" ^ movie_name)) in
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
    "http://api.allocine.fr/rest/v3/movie?partner=E00024954332&format=json&code=" ^ (string_of_int movie_id)
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

(* REQUETE 6 : obtenir la liste des restaurants à une distance donnée d'un point donné *)

let restaurants_at_geographic_location (emplacement : location) (radius : int)  =
  (* a changer en attendant le support de HTTPS *)
  (* regler le pb de choisir l'emplacement le plus et NON le plus "interessant" *)
  let uri = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?language=fr&location="^(string_of_float(fst emplacement))^","^(string_of_float(snd emplacement))^"&radius="^(string_of_int radius)^"&sensor=true&key=AIzaSyA2kG_PpWlQG91CVNXKfyeaKdxbuUK-CeE&types=restaurant" in
  let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
  (match json_ast with
    | `Assoc l -> 
      (match (List.assoc "results" l) with
	| `List l' ->
	  let rec restau liste = 
	    (match liste with 
	      | [] -> []
	      | `Assoc l3 :: r ->
		let pos = (match List.assoc "geometry" l3 with
		  | `Assoc [("location",
			     `Assoc [("lat", `Float f1); 
				     ("lng", `Float f2)])] -> (f1,f2)
		  | _ -> failwith "AST problem"
		) in 
		let id = (match List.assoc "reference" l3 with
		  | `String s -> s
		  | _ -> failwith "AST problem"
		) in
		let name = (match List.assoc "name" l3 with
		  | `String s -> s
		  | _ -> failwith "AST problem"
		) in
		let addr = (match List.assoc "vicinity" l3 with
		  | `String s -> s
		  | _ -> failwith "AST problem"
		) in
		let res = Restau.create id name addr pos []
		in res::restau r
	      | _ -> failwith "AST problem"
	    ) in restau l'
	| _ -> failwith "AST problem"
      )
    | _ -> failwith "AST problem"
  )
;;

(* BUG loc ?*)
let l = restaurants_at_geographic_location loc 500;;
Restau.getTime (List.nth l 0);;
Restau.show (List.nth l 0);;
List.map (Restau.show) l;;



(* REQUETE 7 : Selectionner les restaurants ouverts dans une plage horaire donnée *)

let aux l =
  let day_of_week_number_day = function
    | 0 -> "dimanche"
    | 1 -> "lundi"
    | 2 -> "mardi"
    | 3 -> "mercredi"
    | 4 -> "jeudi"
    | 5 -> "vendredi"
    | 6 -> "samedi"
    | _ -> assert false
  in
  let heure_temps_of_string (s : string) : heure =
    (int_of_string (String.sub s 0 2), int_of_string (String.sub s 2 2))
  in
  let aux2 = function
    | `Assoc (
      ("close", `Assoc (("day", `Int n) :: ("time", `String heure_fermeture_string) :: [])) ::
	("open", `Assoc (("day", `Int _) :: ("time", `String heure_ouverture_string) :: [])) ::
	[]) ->
      (day_of_week_number_day n, (heure_temps_of_string heure_ouverture_string, heure_temps_of_string heure_fermeture_string))
    | _ -> assert false
  in  List.map (aux2) l
;;


let ajout_horaires_resto (l : Restau.t list) (l': (string * (horaire list)) list) = 
  let assoc_restau_list cle l = 
    match List.filter (fun restau -> Restau.getId restau = cle) l with
      | [r] -> (Restau.getName r , Restau.getAddr r , Restau.getPos r) 
      | _ -> assert false
  in
  List.map (fun (id, horaire) -> let (n,a,p) = (assoc_restau_list id l) 
				 in Restau.create id n a p horaire ) l'
;; 
      

      
let rec horaires_restaurants l =
  match l with
    | [] -> []
    | resto :: r -> let id = Restau.getId resto in 
      let uri =
	"https://maps.googleapis.com/maps/api/place/details/json?key=AIzaSyA2kG_PpWlQG91CVNXKfyeaKdxbuUK-CeE&reference="^id^"&sensor=false" in
      let json_ast = Yojson.Safe.from_string (string_of_uri uri)
      in
      (match json_ast with
	| `Assoc l -> 
	  (match List.assoc "result" l with
	    | `Assoc l' ->
	      (try(
		(match List.assoc "opening_hours" l' with
		  | `Assoc l'' -> 
		    (match List.assoc "periods" l'' with
		      | `List liste -> 
			let res = (id, aux liste)
			in res::(horaires_restaurants r) 
			| _ -> failwith "AST problem"
		    ) 
		  | _ -> failwith "AST problem"
		)) with Not_found -> horaires_restaurants r)
		| _ -> failwith "AST problem"
	  )
	| _ -> failwith "AST problem"
      ) ;;

restaurants_at_geographic_location loc 500;;
let loc = geographic_location_of_informal_location "boulevard+des+capucines+paris";;
let l = restaurants_at_geographic_location loc 500;;
let l' = horaires_restaurants l;;
ajout_horaires_resto l l';;

(*
let is_intersection_not_null (hut : plage) (hre : plage) : bool =
  match hre with
    | (t1,t2) ->
      (match hut with
	|(t1',t2') -> if (t1' @< t1) && (t1 @< t2') then true else 
	    if (t1 @< t1') && (t2' @< t2) then true else
	      if (t1 @< t1') && (t1' @< t2) then true else false
	| _ -> assert false
      )
    | _ -> assert false
;;
*)

let is_intersection_not_null ((t1', t2') : plage) ((t1, t2) : plage) : bool =
  (t1' @< t1) && (t1 @< t2') || (t1 @< t1') && (t2' @< t2) || (t1 @< t1') && (t1' @< t2);;

(* BUG TODO *)
let open_restaurants_at_precise_time (l : Restau.t list) : Restau.t list =
  let l' = horaires_restaurants l in
  let restaurants = ajout_horaires_resto l l' in
  List.map (fun ) restaurants
;;


(**
   Tests et debug
*)
geographic_location_of_informal_location "paris";;
allocine_id_of_movie_name movie_name;;
restaurants_at_geographic_location (0., 0.) 0;;



(* Information sur les cinemas  *)

(* let cine_name_of_approx_location loca;;  *)


let allocine_code_of_cine_name cine_name = 
  let uri = "http://api.allocine.fr/rest/v3/search?partner=E00024954332&q="^cine_name^"&format=json&filter=theater" in
  let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
  let id = match json_ast with
    | `Assoc (l) -> 
      (match (List.assoc "feed" l) with
	| `Assoc l' ->
	  (match (List.assoc "theater" l') with
	    | `List (l'') -> 
	      (match (l'') with
		| `Assoc l3 :: _ -> 
		  (match (List.assoc "code" l3) with
		    | `String s -> s
		    | _ -> failwith "problème AST"
		  )
		| _ -> failwith "problème AST"
	      )
	    | _ -> failwith "problème AST"
	  )
	| _ -> failwith "problème AST"
      )
    | _ -> failwith "problème AST"
  in id;;


(* REQUETE 3 : obtenir la liste des cinémas à une distance donée d'un point donné *)

let cinemas_at_geographic_location (emplacement: location) (radius: int)  =
  let uri = "http://api.allocine.fr/rest/v3/theaterlist?partner=E00024954332&lat="^(string_of_float(fst emplacement))^"&long="^(string_of_float(snd emplacement))^"&radius="^(string_of_int radius)^"&format=json" in
  let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
  match json_ast with
    | `Assoc (l) -> 
      (match (List.assoc "feed" l) with 
	| `Assoc l' ->
	  (match (List.assoc "theater" l') with
	    | `List (l'') -> 
	      let rec cines liste = 
		(match (liste) with
		  | [] -> []
		  | `Assoc l3 :: r -> 
		    let id = 
		      (match (List.assoc "code" l3) with
			| `String s -> s
			| _ -> failwith "problème AST"
		      ) in
		    let name = 
		      (match (List.assoc "name" l3) with
			| `String s -> s
			| _ -> failwith "problème AST"
		      ) in
		    let addr = 
		      (match (List.assoc "address" l3) with
			| `String s -> s
			| _ -> failwith "problème AST"
		      )
		    in
		    let postal = 
		      (match (List.assoc "postalCode" l3) with
			| `String s -> s
			| _ -> failwith "problème AST"
		      )
		    in
		    let city = 
		       (match (List.assoc "city" l3) with
			| `String s -> s
			| _ -> failwith "problème AST"
		      )
		    in
		    let lat =
		      (match (List.assoc "geoloc" l3) with
			|`Assoc l4 -> (match (List.assoc "lat" l4) with
			    |`Float f -> f
			    | _ -> failwith "problème AST"
			)
			| _ -> failwith "problème AST"
		      )
		    in
		    let long = 
		      (match (List.assoc "geoloc" l3) with
			|`Assoc l4 -> (match (List.assoc "long" l4) with
			    |`Float f -> f
			    | _ -> failwith "problème AST"
			)
			| _ -> failwith "problème AST"
		      )
		    in let res = Cine.create id name (addr ^ " " ^ postal ^ " " ^ city) (lat, long)
		       in res :: cines r 
		  | _ -> failwith "problème AST"
		) in cines (l'') 
	    | _ -> failwith "problème AST"
	  )
	| _ -> failwith "problème AST"
      )
    | _ -> failwith "problème AST"
;;

(* REQUETE : Cine.t × Film.t -> Séance.t list *)
(* TODO Hai *)



(* changed *)
(* REQUETE 1 : obtenir la liste des films projettés dans un cinema donné *)

let films_in_precise_cinema (cine : Cine.t) : Film.t list =
  let uri = "http://api.allocine.fr/rest/v3/showtimelist?partner=E00024954332&theaters=" ^ (Cine.getId cine) ^ "&format=json" in
  let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
  match json_ast with
    | `Assoc (("feed", `Assoc (l)) :: _) -> (match (List.assoc "theaterShowtimes" l) with
	| `List (`Assoc l' :: _) -> (match (List.assoc "movieShowtimes" l') with
	    | `List films_list ->
	      let cine_module_of_movie_json_ast film_ast : Film.t = 
		let movie_id = match film_ast with 
		  | `Assoc l -> (match (List.assoc "onShow" l) with
		      | `Assoc (("movie", `Assoc l'') :: _) -> (match List.assoc "code" l'' with
			  | `Int n -> n
			  | _ -> assert false
		      )
		      | _ -> assert false
		  )
		  | _ -> assert false
		and movie_title = match film_ast with 
		  | `Assoc l -> (match (List.assoc "onShow" l) with
		      | `Assoc (("movie", `Assoc l'') :: _) -> (match List.assoc "title" l'' with
			  | `String s -> s
			  | _ -> assert false
		      )
		      | _ -> assert false
		  )
		  | _ -> assert false
		and movie_runtime = match film_ast with 
		  | `Assoc l -> (match (List.assoc "onShow" l) with
		      | `Assoc (("movie", `Assoc l'') :: _) -> (match List.assoc "runtime" l'' with
			  | `Int n -> n
			  | _ -> assert false
		      )
		      | _ -> assert false
		  )
		  | _ -> assert false
		and movie_actors = match film_ast with 
		  | `Assoc l -> (match (List.assoc "onShow" l) with
		      | `Assoc (("movie", `Assoc l'') :: _) -> (match List.assoc "castingShort" l'' with
			  | `Assoc l''' -> (match List.assoc "actors" l''' with
			      | `String s -> s
			      | _ -> assert false
			  )
			  | _ -> assert false
		      )
		      | _ -> assert false
		  )
		  | _ -> assert false
		and movie_nationality = []
		in Film.create movie_id movie_title movie_runtime movie_actors movie_nationality
	      in List.map (cine_module_of_movie_json_ast) films_list
	    | _ -> assert false
	)
	| _ -> assert false
    )       
    | _ -> assert false
;;

let cine = Cine.create "C0026" " " " " (0.0,0.0);; 
let test = films_in_precise_cinema cine;;

let test = List.nth test 0;;


(* REQUETE 2 : obtenir la liste des cinemas projetant un film donné dans une plage horaire donnée *)
let json_ast = Yojson.Safe.from_string (string_of_uri "http://api.allocine.fr/rest/v3/showtimelist?partner=E00024954332&theaters=C0026&movie=139589&format=json");;

  




let films_in_cinemas_at_precise_time (l : Cine.t list) (f : Film.t) ((t1, t2) : plage) (d : date) : Cine.t list = 
  let movie_showtimes_list_exists (cine : Cine.t) : bool = 
    let uri = "http://api.allocine.fr/rest/v3/showtimelist?partner=E00024954332&theaters="^ Cine.getId cine ^ "&movie=" ^ string_of_int (Film.getId f) ^ "&format=json" in
    let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
    match json_ast with
    | `Assoc (("feed", `Assoc (l)) :: _) ->
      (match (List.assoc "theaterShowtimes" l)  with
	| `List (`Assoc l :: _) ->
	  List.exists (fun (x, _) -> x = "movieShowtimes") l
	| _ -> assert false
      )
    | _ -> assert false
  in
  let movie_showtimes_of_theater (cine : Cine.t) : bool = 
    let uri = "http://api.allocine.fr/rest/v3/showtimelist?partner=E00024954332&theaters="^ Cine.getId cine ^ "&movie=" ^ string_of_int (Film.getId f) ^ "&format=json" in
    let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
    match json_ast with
      | `Assoc (("feed", `Assoc (l)) :: _) ->
	(match (List.assoc "theaterShowtimes" l)  with
	  | `List (`Assoc l :: _) ->
	    let is_there_a_screening json_ast_of_screening_type =
	      (match json_ast_of_screening_type with
		| `Assoc l' -> (match List.assoc "scr" l' with
		    | `List l'' ->
		      let l''' = List.filter (function `Assoc (("d", `String s) :: _) -> s = (string_of_date "-" d) | _ -> assert false) l'' in
		      (match l''' with
			| `Assoc (l4) :: _ -> (match (List.assoc "t" l4) with
			    | `List l5 ->
			      List.exists (function
				| `Assoc (_ :: _ :: ("$", `String s) :: []) ->
				  (t1 @<= (heure_of_string s)) && ((heure_of_string s) @<= t2)
				| _ -> assert false
			      ) l5
			    | _ -> assert false
			)
			| _ -> assert false
		      )	
		    | _ -> assert false
		)
		| _ -> assert false
	      )
	    in
	    (match (List.assoc "movieShowtimes" l) with
	      | `List (json_ast_of_screening_type_1 :: json_ast_of_screening_type_2 :: []) -> 
		 (* ne pas oublier le 2eme*)
		(is_there_a_screening json_ast_of_screening_type_1) || (is_there_a_screening json_ast_of_screening_type_2)
	       | `List (json_ast_of_screening_type :: []) ->
		 is_there_a_screening json_ast_of_screening_type
	       | _ -> assert false
	    )
	  | _ -> assert false
	)
      | _ -> assert false 
  in
  let salles_projetant_le_film = List.filter (movie_showtimes_list_exists) l 
  in List.filter (movie_showtimes_of_theater) salles_projetant_le_film
;;

(**
   Construire la requête pour avoir la liste des cinémas aux alentours d'un point précis
   Google Places + Allocine
*)
let json_ast_of_informal_cine_id (cine_id : string) = 
  Yojson.Safe.from_string (
    string_of_uri (
      "http://api.allocine.fr/rest/v3/theaterlist?partner=E00024954332&format=json&theater=" ^ cine_id
    ))
;;

let test = json_ast_of_informal_cine_id "C0026";;

let cine_name_of_allocine_json (j : Yojson.Safe.json) : string  = match j with
  | `Assoc (("feed", `Assoc l) :: _) -> (match (List.assoc "theater" l) with
      | `List l ->
	let l1 = List.map (function (`Assoc l) -> l | _ -> assert false) l in
	let l2 = List.filter (fun l -> List.exists (fun a -> a = ("code", `String "C0026")) l) l1 in
	let l3 = List.nth l2 0 in
	(match (List.assoc "name" l3) with
	  | `String name -> name
	  | _ -> assert false
	)   
      | _ -> assert false
  )
  | _        -> assert false
;;

cine_name_of_allocine_json test;;

let cine_addr_of_allocine_json (j : Yojson.Safe.json) : string  = match j with
  | `Assoc (("feed", `Assoc l) :: _) -> (match (List.assoc "theater" l) with
      | `List l ->
	let l1 = List.map (function (`Assoc l) -> l | _ -> assert false) l in
	let l2 = List.filter (fun l -> List.exists (fun a -> a = ("code", `String "C0026")) l) l1 in
	let l3 = List.nth l2 0 in
	(match (List.assoc "address" l3) with
	  | `String name -> name
	  | _ -> assert false
	)
	^ " " 
	^ (match (List.assoc "postalCode" l3) with
	  | `String name -> name
	  | _ -> assert false
	)
	^ " " 
	^ (match (List.assoc "city" l3) with
	  | `String name -> name
	  | _ -> assert false
	)

      | _ -> assert false
  )
  | _        -> assert false
;;

cine_addr_of_allocine_json test;;

let cine_pos_of_allocine_json (j : Yojson.Safe.json) : location = match j with
  | `Assoc (("feed", `Assoc l) :: _) -> (match (List.assoc "theater" l) with
      | `List l ->
	let l1 = List.map (function (`Assoc l) -> l | _ -> assert false) l in
	let l2 = List.filter (fun l -> List.exists (fun a -> a = ("code", `String "C0026")) l) l1 in
	let l3 = List.nth l2 0 in
	(match (List.assoc "geoloc" l3) with
	  | `Assoc (("lat", `Float f1) :: ("long", `Float f2) :: _) -> (f1, f2)
	  | _ -> assert false
	)   
      | _ -> assert false
  )
  | _        -> assert false
;;

cine_pos_of_allocine_json test;;


(* REQUETE COMBINEE 1 *)
(* TODO *)
let requete_combinee_1 
