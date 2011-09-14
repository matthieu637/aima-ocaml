open Aspi_env;;

type action = Aspirer | Droite | Gauche;;

let agent emplacement etat = if ( etat == Sale ) then Aspirer
	else if (emplacement == 0 ) then Droite
	else if (emplacement == 1 ) then Gauche
	else Aspirer ;;

let rec lancerAgent env f i n = if(n > 0) then match (i_to_xy i env) with
		| (x, y) -> match (f i (get x y env)) with
				| Aspirer -> lancerAgent (nettoyer x y env) f i (n - 1)
				| Droite -> lancerAgent (addPerf env point_perdu) f (i + 1) (n - 1)
				| Gauche -> lancerAgent (addPerf env point_perdu) f (i - 1) (n - 1)
	else (perfOf env);;


let env = (creerEnvAspi 2 1);;
let env = salirEnv env;;

afficher env;;

print_string "-------\n";;

print_int (perfOf env);;
print_string "\n";;
print_int (lancerAgent env agent 1 10);;


(* ### 2.8 ### *)
(* *)
(* S,S -> 12 *)
(* S,P -> 1 *)
(* P,S -> 1 *)
(* P,P -> -10 *)
(**)
(* moyenne : 1 *)

(* ### 2.9 a&b) *)
(* Un agent reflexe simple ne peut maximiser la meusure de performance de déplacement, car il ne se souvient *)
(* pas des états précédents, et donc continue l'analyse sans arret. *)
(* Un agent à état peut résoudre ce problème *)  
	
(* Implémentation abstraite : agent_reflex_simple_aspi_abstrait *)

(* c) Si le percept de l'aspi donne des infos sur toute la piece, un agent reflex simple suffit, il n'y a plus*)
(* besoin d'état.*)
(* *)
(* *)


(* ### 2.9 ### *)
(* a) un agent reflexe simple n'a aucun critère de décision quand à la direction prendre, de plus il ne se souvient *)
(* meme pas quelle case, il a nettoyé, il ne peut etre rationnel, il suit une direction jusqu'à blocage*)
(* *)
(* b) oui, avec de la chance *)
(* *)
(* c) il suffit d'un environnement avec des taches éloignés avec des couloirs *)
(* *)
(* *)



