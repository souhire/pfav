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

(* Décide si une année est bissextile *)
let est_bissextile (aaaa : int) : bool = 
  ((aaaa mod 4 = 0) && (aaaa mod 100 <> 0)) || (aaaa mod 400 = 0)
;;

(* A une date jj/mm/aaaa, on associe le jour de la semaine *)
(* http://fr.wikibooks.org/wiki/Trouver_le_jour_de_la_semaine_avec_une_date_donn%C3%A9e *)
let day_of_week_of_date ((jj, mm, aaaa) : date)  = 
  let deux_chiffres_annee = aaaa - (aaaa / 100) * 100 in
  let ajout_1_quart = deux_chiffres_annee + deux_chiffres_annee / 4 in
  let ajout_jour_mois = ajout_1_quart + jj in
  let ajout_code_mois = ajout_jour_mois + (match mm with
    | 1	 -> 1
    | 2	 -> 4
    | 3	 -> 4
    | 4	 -> 0
    | 5	 -> 2
    | 6	 -> 5
    | 7	 -> 0
    | 8	 -> 3
    | 9	 -> 6
    | 10 -> 1
    | 11 -> 4
    | 12 -> 6
    | _  -> failwith "Impossible date"
  ) in
  let if_bissex = if (est_bissextile aaaa) && ((mm = 1) || (mm = 2)) then ajout_code_mois - 1 else ajout_code_mois in
  let selon_siecle = if_bissex + match ((aaaa / 100) * 100) with
    | 1600 -> 6
    | 1700 -> 4
    | 1800 -> 2
    | 1900 -> 0
    | 2000 -> 6
    | 2100 -> 4
    | _    -> failwith "Impossible date" in
  let divise_garde_reste = selon_siecle mod 7 in
  let jour_de_semaine = match divise_garde_reste with
    | 1	-> "dimanche"
    | 2	-> "lundi"
    | 3	-> "mardi"
    | 4	-> "mercredi"
    | 5	-> "jeudi"
    | 6	-> "vendredi"
    | 0	-> "samedi"
    | _ -> assert false
  in jour_de_semaine
;;

(* retire les espaces par des plus *)
let no_spaces_of_string (s : string) : string = 
 let s' = ref "" in
 let () = String.iter (fun c -> if (c=' ') then s':=!s'^"+" else s':=!s'^(String.make 1 c)) s in !s';;

(* changed *)
(* Retire les doublons dans une liste *)
let no_repeated_elements_of_list ((@=) : 'a -> 'a -> bool) (l : 'a list) =
  let rec aux l' acc = match l' with
    | []       -> acc
    | e :: l'' ->
      if List.exists (fun x -> x @= e) acc
      then aux l'' acc
      else aux l'' (e :: acc)
  in List.rev (aux l [])
;;

(* Pretty string pour une date *)
let string_of_date (delimiter : string) ((jj, mm, aaaa) : date) : string =
  let jour_string = if jj < 10 then "0" ^ string_of_int jj else string_of_int jj
  and mois_string = if mm < 10 then "0" ^ string_of_int mm else string_of_int mm
  and annee_string = string_of_int aaaa
  in annee_string ^ delimiter ^ mois_string ^ delimiter ^ jour_string
;;

let date_of_string (s : string) : date =
  (int_of_string (String.sub s 8 2), int_of_string (String.sub s 5 2), int_of_string (String.sub s 0 4))
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

(* A une heure, on retourne l'heure auquel on aura additionné des minutes *)
let (@+) ((hh, mm) : heure) (minutes : int) : heure =
  let h_of_m = minutes / 60
  and remainding_m = minutes mod 60 in
  ((hh + h_of_m + (if mm + remainding_m >= 60 then 1 else 0)) mod 24, (mm + remainding_m) mod 60)
;;

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

(* Position géographique (latitude, longitude) à partir d'un emplacement informel *)
let geographic_location_of_informal_location (l : string) : float * float =
  (* a changer en attendant le support de HTTPS *)
  let uri = "https://maps.googleapis.com/maps/api/place/textsearch/json?sensor=true&query="^ no_spaces_of_string l ^"&key=AIzaSyA2kG_PpWlQG91CVNXKfyeaKdxbuUK-CeE" in
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

(* Module d'une salle de projection *)
module type CineSig =
  sig
    type t
    val create : string -> string -> string -> location -> t
    val informal_create : string -> t
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

    let informal_create (cine_name : string) : t = 
      let uri = "http://api.allocine.fr/rest/v3/search?partner=E00024954332&q=" ^ no_spaces_of_string cine_name ^ "&format=json&filter=theater" in
      let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
      match json_ast with
	| `Assoc (l) -> 
	  (match (List.assoc "feed" l) with
	    | `Assoc l' ->
	      (match (List.assoc "theater" l') with
		| `List (l'') -> 
		  (match (l'') with
		    | `Assoc l3 :: _ -> 
		      let theater_id = (match (List.assoc "code" l3) with
			| `String s -> s
			| _ -> failwith "problème AST"
		      )
		      and theater_name = (match (List.assoc "name" l3) with
			| `String s -> s
			| _ -> failwith "problème AST"
		      )
		      and theater_addr = (match (List.assoc "address" l3) with
			| `String s -> s
			| _ -> failwith "problème AST"
		      )
		      and theater_postal_code = (match (List.assoc "address" l3) with
			| `String s -> s
			| _ -> failwith "problème AST"
		      )
		      and theater_city = (match (List.assoc "address" l3) with
			| `String s -> s
			| _ -> failwith "problème AST"
		      )
		      in let theater_location = geographic_location_of_informal_location (theater_name ^ "+" ^ theater_addr ^ "+" ^ theater_postal_code ^ "+" ^ theater_city)
			 in
			 {
			   id = theater_id ;
			   name = theater_name ;
			   addr = theater_addr ^ " " ^ theater_postal_code ^ " " ^ theater_city ;
			   pos = theater_location
			 }
		    | _ -> failwith "problème AST"
		  )
		| _ -> failwith "problème AST"
	      )
	    | _ -> failwith "problème AST"
	  )
	| _ -> failwith "problème AST"

    let getId f = f.id
    let getName f = f.name
    let getAddr f = f.addr
    let getPos f = f.pos
  end
;;

Cine.informal_create "mk2 bibliotheque";;

(* Module d'un film produit *)
module type FilmSig =
    sig
      type t
      val create : int -> string -> int -> string -> string list -> t
      val informal_create : string -> t
      val getTitle : t -> string
      val getRuntime : t -> int
      val getActors : t -> string
      val getNationality : t -> string list
      val show : t -> unit 
    end;;

(* changed *)
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

    let informal_create movie_name = 
      (* Fonction récupérant un identifiant Allociné à partir d'un nom de film possible entré par l'utilisateur *)
      let allocine_id_of_movie_name (movie_name : string) : int =
	let json_ast =
	  Yojson.Safe.from_string
	    (string_of_uri ("http://api.allocine.fr/rest/v3/search?partner=E00024954332&filter=movie&format=json&q=" ^ no_spaces_of_string movie_name)) in
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
	  | _ -> failwith "Feed field not found" in
      (* Fonction runtime_of_allocine_json *)
      (* Récupère la durée de film à partir d'un AST JSON d'un film sur Allocine *)
      let rec runtime_of_allocine_json (j : Yojson.Safe.json) : int =
	match j with
	  | `Assoc ((_, `Assoc (("runtime", valeur) :: l')) :: _) ->
	    (match valeur with `Int n -> n | _ -> failwith "AST problem") 
	  | `Assoc ((c1, `Assoc (_ :: l')) :: c2) ->
	    runtime_of_allocine_json (`Assoc ((c1, `Assoc l') :: c2))
	  | _ -> failwith "Runtime field not found" in
      (* Fonction title_of_allocine_json *)
      (* Récupère le titre du film à partir d'un AST JSON d'un film sur Allocine *)
      let rec title_of_allocine_json (j : Yojson.Safe.json) : string =
	match j with
	  | `Assoc ((_, `Assoc (("title", valeur) :: l')) :: _) ->
	    (match valeur with `String s -> s | _ -> failwith "AST problem") 
	  | `Assoc ((c1, `Assoc (_ :: l')) :: c2) ->
	    title_of_allocine_json (`Assoc ((c1, `Assoc l') :: c2))
	  | _ -> failwith "Title field not found" in
      (* Fonction production_year_of_allocine_json *)
      (* Récupère l'année de production du film à partir d'un AST JSON d'un film sur Allocine *)
      let rec production_year_of_allocine_json (j : Yojson.Safe.json) : int =
	match j with
	  | `Assoc ((_, `Assoc (("productionYear", valeur) :: l')) :: _) ->
	    (match valeur with `Int n -> n | _ -> failwith "AST problem") 
	  | `Assoc ((c1, `Assoc (_ :: l')) :: c2) ->
	    production_year_of_allocine_json (`Assoc ((c1, `Assoc l') :: c2))
	  | _ -> failwith "Production year field not found" in
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
	  | _ -> failwith "Nationality field not found" in
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
	| _ -> failwith "Genre field not found" in
    (* Fonction actors_of_allocine_json *)
    let rec actors_of_allocine_json (j : Yojson.Safe.json) : string =
      match j with
	| `Assoc (("movie", `Assoc l) :: _) -> (match List.assoc "castingShort" l with
	    | `Assoc l' -> (match List.assoc "actors" l' with
		| `String s -> s
		| _ -> assert false
	    )
	    | _ -> assert false
	)
	| _ -> failwith "Actors field not found"	  
    in
    let id_of_movie = allocine_id_of_movie_name movie_name in
    let p = Yojson.Safe.from_string (
      string_of_uri (
	"http://api.allocine.fr/rest/v3/movie?partner=E00024954332&format=json&code=" ^ (string_of_int id_of_movie))) in
    {
      id = id_of_movie ;
      title = title_of_allocine_json p;
      runtime = runtime_of_allocine_json p;
      actors = actors_of_allocine_json p;
      nationality = nationality_of_allocine_json p
    }
      
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
    let weak_equal (f1 : t) (f2 : t) : bool = f1.id = f2.id
  end
;;


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
    val getFilm : t -> Film.t
    val isVO : t -> bool option
    val is3D : t -> bool option
    (* val show : t -> unit *)
  end;;

(* changed *)
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
    let getFilm f = f.film
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

(**
   Googles Places renvoie tous les restaurants à un emplacement géographique suivant un rayon précis
*)


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

let loc = geographic_location_of_informal_location "MK2+bibliotheque+paris";;
let l = restaurants_at_geographic_location loc 500;;
Restau.getTime (List.nth l 0);;
Restau.show (List.nth l 0);;
List.map (Restau.show) l;;



(* REQUETE 7 : Selectionner les restaurants ouverts dans une plage horaire donnée *)


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
  in
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
let resto = ajout_horaires_resto l l';;

let is_intersection_not_null ((t1', t2') : plage) ((t1, t2) : plage) : bool =
  (t1' @< t1) && (t1 @< t2') || (t1 @< t1') && (t2' @< t2) || (t1 @< t1') && (t1' @< t2);;

let open_restaurants_at_precise_time (l : Restau.t list) (j : string) (p : plage)  =
  let l' = horaires_restaurants l in
  let liste = (List.map (fun elmt -> 
    let horaires = snd elmt in ((fst elmt),( 
    (List.filter ( fun e -> ((fst e) = j ) && (is_intersection_not_null p (snd e))) horaires)))) l') in
  let liste' = List.filter (fun e -> not((snd e)=[])) liste in
  let res = List.filter (fun (str,_) -> List.exists (fun (str',_) -> str = str') liste') l' in ajout_horaires_resto l res
;;


open_restaurants_at_precise_time resto  "lundi" ((09,00),(11,00));;


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
let seances_of_film_at_cinema (film : Film.t) (cine : Cine.t) = 
  let uri = "http://api.allocine.fr/rest/v3/showtimelist?partner=E00024954332&theaters=" ^ (Cine.getId cine) ^ "&movie=" ^ (string_of_int (Film.getId film)) ^ "&format=json" in
  let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
  let seance_list_of_screening_type (j : Yojson.Safe.json) = 
  let is_3d (j : Yojson.Safe.json) = match j with
    | `Assoc l -> (match (List.assoc "screenFormat" l) with
	| `Assoc (("code", `Int n) :: ("$", `String _) :: []) -> n = 110003
	| _ -> assert false
    )
    | _ -> assert false
  and is_numerique (j : Yojson.Safe.json) = match j with
  | `Assoc l -> (match (List.assoc "screenFormat" l) with
      | `Assoc (("code", `Int n) :: ("$", `String _) :: []) -> n = 110002
      | _ -> assert false
  )
  | _ -> assert false
  and is_vo (j : Yojson.Safe.json) = match j with
  | `Assoc l -> (match (List.assoc "version" l) with
      | `Assoc (("original", `String b) :: _) -> (b = "true")
      | _ -> assert false
  )
  | _ -> assert false in
  let is_this_seance_type_3d = is_3d j
  and is_this_seance_type_numerique = is_numerique j
  and is_this_seance_vo = is_vo j in
  let list_whose_dates_are_not_seperated = match j with
    | `Assoc l -> (match List.assoc "scr" l with
	| `List l' ->
	  List.map
	    (fun e -> match e with
	      | `Assoc (("d", `String date_string) :: ("t", `List l'') :: []) ->
		List.map
		  (fun e' -> match e' with 
		    | `Assoc (("code", `Int movie_code) :: ("p", _) :: ("$", `String heure_string) :: []) ->
		      Seance.create movie_code film cine (date_of_string date_string) (heure_of_string heure_string) (Some is_this_seance_vo) (Some (is_this_seance_type_3d && (not is_this_seance_type_numerique)))
		    | _ -> assert false
		  )
		  l''
	      | _ -> assert false
	    )
	    l'
	| _ -> assert false
    )
    | _ -> assert false
  in List.flatten list_whose_dates_are_not_seperated
  in
  match json_ast with
    | `Assoc (("feed", `Assoc (l)) :: _) ->
      (match (List.assoc "theaterShowtimes" l)  with
	| `List (`Assoc l :: _) -> (match (List.assoc "movieShowtimes" l) with
	    | `List (json_ast_of_screening_type_1 :: json_ast_of_screening_type_2 :: []) -> 
	      (seance_list_of_screening_type json_ast_of_screening_type_1)
	      @ (seance_list_of_screening_type json_ast_of_screening_type_2)
	    | `List (json_ast_of_screening_type :: []) ->
	      seance_list_of_screening_type json_ast_of_screening_type
	    | _ -> assert false
	)
	| _ -> assert false
      )
    | _ -> assert false
;;

let film_test = Film.informal_create "la cage doree";;
let cine_test = Cine.informal_create "ugc bercy";;
let test_seance_type = seances_of_film_at_cinema film_test cine_test;;



(* REQUETE 1 : obtenir la liste des films projettés dans un cinema donné *)

let films_in_precise_cinema (cine : Cine.t) : Film.t list =
  let uri = "http://api.allocine.fr/rest/v3/showtimelist?partner=E00024954332&theaters=" ^ (Cine.getId cine) ^ "&format=json" in
  let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
  match json_ast with
    | `Assoc (("feed", `Assoc (l)) :: _) -> (try (match (List.assoc "theaterShowtimes" l) with
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
		and movie_actors = try (
		  match film_ast with 
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
		)
		  with _ -> ""   
		and movie_nationality = []
		in Film.create movie_id movie_title movie_runtime movie_actors movie_nationality
	      in List.map (cine_module_of_movie_json_ast) films_list
	    | _ -> assert false
	)
	| _ -> assert false
    ) with _ -> [])
    | _ -> assert false
;;

let cine = Cine.create "C1159" " " " " (0.0,0.0);;
let test = films_in_precise_cinema cine;;



(* REQUETE 2 : obtenir la liste des cinemas projetant un film donné dans une plage horaire donnée *)

  
let films_in_cinemas_at_precise_time (l : Cine.t list) (f : Film.t) ((t1, t2) : plage) (d : date) : Cine.t list = 
  let movie_showtimes_list_exists (cine : Cine.t) : bool = 
    let uri = "http://api.allocine.fr/rest/v3/showtimelist?partner=E00024954332&theaters="^ Cine.getId cine ^ "&movie=" ^ string_of_int (Film.getId f) ^ "&format=json" in
    let json_ast = Yojson.Safe.from_string (string_of_uri uri) in
    match json_ast with
      | `Assoc (("feed", `Assoc (l)) :: _) ->
	(try (match (List.assoc "theaterShowtimes" l)  with
	  | `List (`Assoc l :: _) ->
	    List.exists (fun (x, _) -> x = "movieShowtimes") l
	  | _ -> failwith "false 1"
	) with Not_found -> false)
      | _ -> failwith "false 2"
  and  movie_showtimes_of_theater (cine : Cine.t) : bool = 
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
			let l''' = List.filter (function `Assoc (("d", `String s) :: _) -> s = (string_of_date "-" d) | _ -> failwith "false 3") l'' in
			(match l''' with
			  | `Assoc (l4) :: _ -> (match (List.assoc "t" l4) with
			      | `List l5 ->
				List.exists (function
				  | `Assoc (_ :: _ :: ("$", `String s) :: []) ->
				    (t1 @<= (heure_of_string s)) && ((heure_of_string s) @<= t2)
				  | _ -> failwith "false 4"
				) l5
			      | _ -> failwith "false 5"
			  )
			  | _ -> false
			)	
			| _ -> failwith "false 6"
		)
		| _ -> failwith "false 7"
	      )
	    in
	    (match (List.assoc "movieShowtimes" l) with
	      | `List (json_ast_of_screening_type_1 :: json_ast_of_screening_type_2 :: []) -> 
		 (* ne pas oublier le 2eme*)
		(is_there_a_screening json_ast_of_screening_type_1) || (is_there_a_screening json_ast_of_screening_type_2)
	       | `List (json_ast_of_screening_type :: []) ->
		 is_there_a_screening json_ast_of_screening_type
	       | _ -> false
	    )
	  | _ -> failwith "false 9"
	)
      | _ -> failwith "false 10"
  in
  let salles_projetant_le_film = List.filter (movie_showtimes_list_exists) l 
  in List.filter (movie_showtimes_of_theater) salles_projetant_le_film
;;

let cine = [Cine.create "C0026" " " " " (0.0,0.0);
	    Cine.create "CO144" " " " " (0.0,0.0);
	    Cine.create "C0146" " " " " (0.0,0.0);
	    Cine.create "C2954" " " " " (0.0,0.0);
	    Cine.create "B0193" " " " " (0.0,0.0)];;
let cine2 = [Cine.create "C0144" " " " " (0.0,0.0)];;
let film = Film.create 139589 "" 0 "" [];;

films_in_cinemas_at_precise_time cine film ((10,00),(20,00)) (11,5,2013);;


(* REQUETE COMBINEE 1 : Quels sont les cinemas projettant le film informal_film_name dans la plage horaire (t1,t2) le jour d et qui sont dans un rayon de radius de emplacement *)

(* changed *)
let requete_combinee_1 (emplacement: string) (radius: int) (informal_film_name : string) ((t1, t2) : plage) (d : date) : Cine.t list =
  let position : location = geographic_location_of_informal_location emplacement in
  let film : Film.t = Film.informal_create informal_film_name
  and cine_list : Cine.t list = cinemas_at_geographic_location position radius in
  films_in_cinemas_at_precise_time cine_list film (t1, t2) d
;;

requete_combinee_1  "bibliotheque+francois+mitterand+paris" 1000 "iron man 3" ((11, 0), (20, 0)) (11, 5, 2013);;

(*
let loc = geographic_location_of_informal_location "bibliotheque+francois+mitterand+paris";;
let film = Film.informal_create "iron man 3";;
let cine = cinemas_at_geographic_location loc 1000;;

films_in_cinemas_at_precise_time cine film ((11, 0), (20, 0)) (11, 5, 2012);;*)


(*TODO*)


(* REQUETE COMBINEE 2 : Quels sont les films projettés entre t1 et t2 dans un cinema à moins de radius de emplacement *)
let requete_combinee_2 (emplacement : string) (radius : int) ((t1, t2) : plage) (d : date)  =
  let position : location = geographic_location_of_informal_location emplacement in
  let cinema_dans_le_rayon : Cine.t list = cinemas_at_geographic_location position radius in
  let films_projetes_dans_le_rayon : Film.t list = List.flatten (List.map (films_in_precise_cinema) cinema_dans_le_rayon) in
  let film_list_sans_doublons : Film.t list = no_repeated_elements_of_list (Film.weak_equal) films_projetes_dans_le_rayon in
  film_list_sans_doublons
;;


requete_combinee_2 "bibliotheque francois mitterand" 1000 ((10, 00), (22, 00)) (12, 5, 2013) ;;


(* REQUETE COMBINEE 3 : Quels sont les restaurants ouverts entre t1 et t2 et qui sont amoins de radius de emplacement *)  

let requete_combinee_3 (emplacement : string) (radius : int) ((t1, t2) : plage) (d : date) : Restau.t list =
  let position : location = geographic_location_of_informal_location emplacement in
  let restaurants_dans_le_rayon : Restau.t list = restaurants_at_geographic_location position radius in
  let jour_de_la_semaine : string = day_of_week_of_date d in
  let restaurants_ouverts_pendant : Restau.t list = open_restaurants_at_precise_time restaurants_dans_le_rayon jour_de_la_semaine (t1, t2) in
  restaurants_ouverts_pendant
;;

requete_combinee_3 "bibliothque francois mitterand" 500 ((20, 00), (21, 00)) (12, 5, 2013);;


(* REQUETE INTERMEDIAIRE *)
(* Seance.t × Restau.t -> bool *)
(* Décide si une séance seance après sa projection permettra au groupe d'amis d'aller manger au restaurant restau *)
let est_possible_de_manger_apres_seance (seance : Seance.t) (restau : Restau.t) = 
  let jour_semaine_de_seance : string = day_of_week_of_date (Seance.getDate seance) in
  let heure_de_fin_seance : heure = (Seance.getBeginTime seance) @+ (Film.getRuntime (Seance.getFilm seance)) in
  let horaires_du_jour : horaire list = List.filter (fun (x, _) -> x = jour_semaine_de_seance) (Restau.getTime restau) in
  match horaires_du_jour with
    | [] -> false
    | _  ->
      List.exists
	(fun (_, (t1, t2)) ->
	  (t1 @<= heure_de_fin_seance) && (heure_de_fin_seance @<= t2)
	)
	horaires_du_jour
;;


(* REQUETE COMBINEE 4 : Quels sont les films, parmi une liste donnée, comprenant des criteres tels que VO ou 3D,
   qui sont projetes entre t1 et t2 dans un cinema a moins de radius de emplacement, 
   et qui permettent d'aller manger ensuite dans un restaurant ouvert a moins de radius de emplacement *)

let requete_combinee_4
    (emplacement : string)
    (radius_cine : int)
    ((t1, t2) : plage)
    (d : date)
    (film_noms_informels_a_criteres : (string * bool * bool) list)
    (radius_restau : int)
     (* : Film.t list *) =
  let position : location = geographic_location_of_informal_location emplacement in
  let films : (Film.t (* * bool * bool *) ) list =
    List.map (fun (n, _, _) -> Film.informal_create n) film_noms_informels_a_criteres in
  let cinemas_dans_le_rayon : Cine.t list = cinemas_at_geographic_location position radius_cine in
  let restaurants_dans_le_rayon : Restau.t list = restaurants_at_geographic_location position radius_restau in
  let films_satisfaisant_les_cond_cinema : Film.t list = 
    List.flatten
      (List.flatten
	 (List.map
	    (fun f ->
	      List.map
		(fun c ->
		  match films_in_cinemas_at_precise_time [c] f (t1, t2) d with
		    | [] -> []
		    | _  -> [f]
		)
		cinemas_dans_le_rayon
	    )
	    films
	 )
      ) in
  let seances_satisfaisant_les_cond_cinema : Seance.t list =
    List.flatten (List.flatten (List.map (fun film -> List.map (fun cine -> seances_of_film_at_cinema film cine) cinemas_dans_le_rayon) films_satisfaisant_les_cond_cinema)) in
  let seances_satisfaisant_la_condition_manger_apres : Seance.t list =
    List.filter (fun seance -> List.exists (fun restau -> est_possible_de_manger_apres_seance seance restau) restaurants_dans_le_rayon) seances_satisfaisant_les_cond_cinema in
  let films_satisfaisant_la_condition_manger_apres : Film.t list =
    List.map (fun seance -> Seance.getFilm seance) seances_satisfaisant_la_condition_manger_apres in
  films_satisfaisant_la_condition_manger_apres
;;

requete_combinee_4 "bibliotheque francois mitterand" 500 ((10, 00), (20, 00)) (12, 5, 2013) [("trance", false, false) ; ("gatsby", false, false) ; ("iron man 3", false, false)] 1000;;

(* REQUETE COMBINEE 5 : Quels sont les films, parmi une liste donnee, qui sont projetes a des horaires permettant
   a votre groupe d'amis de ne pas attendre plus d'une demi-heure, avant ou apres, et dans
   des cinemas distant moins de radius l'un de l'autre *)



(* REQUETE COMBINEE 6 : Quels sont les films, parmi une liste donnee, comprenant des criteres tels que VO ou 3D,
qui sont projetes entre t1 et t2 dans des cinema a moins de radius d'un point autour
duquel existe un restaurant ouvert a moins de radius2 permettent d'aller manger ensuite *)
 
