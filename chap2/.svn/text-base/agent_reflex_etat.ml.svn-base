open Aspi_env;;

type percept = P of case * etat
and case = A | B;;

type etatA = E of (case * etat) list;;

type action = Nettoyer | Droite | Gauche | NeRienFaire;;
type regle = action;;

let actualiser_etat etat action percept = match percept with
	| P(c, e) -> match action with
			| Nettoyer | Droite | Gauche | NeRienFaire -> match etat with
					| E l -> (E((c, e) :: List.remove_assoc c l));;

let trouver_regle etat = match etat with
	| E[] -> NeRienFaire
	| E[(A, Propre); (B, Propre)] -> NeRienFaire
	| E((A, Propre)::_ ) -> Droite
	| E[(B, Propre); (A, Propre)] -> NeRienFaire
	| E((B, Propre)::_) -> Gauche
	| E((A, Sale)::_) -> Nettoyer
	| E((B, Sale)::_) -> Nettoyer;;

let regle_action regle = regle;;

let etat = ref (E[]);;
let action = ref NeRienFaire;;

let agent_reflex_etat percept =
	etat := (actualiser_etat !etat !action percept);
	let regle = trouver_regle !etat in
	action := regle_action regle;
	!action;;

let creerPercept i = match i with
	| 0 -> A
	| _ -> B;;

let rec lancerAgent env f i n = if(n > 0) then match (i_to_xy i env) with
		| (x, y) -> match (f (P(creerPercept i, (get x y env)))) with
				| Nettoyer -> lancerAgent (nettoyer x y env) f i (n - 1)
				| Droite -> lancerAgent (addPerf env point_perdu) f (i + 1) (n - 1)
				| Gauche -> lancerAgent (addPerf env point_perdu) f (i - 1) (n - 1)
				| NeRienFaire -> lancerAgent env f i (n - 1)
	else (perfOf env);;

let env = (creerEnvAspi 2 1);;
let env = salirEnv env;;

afficher env;;

print_string "-------\n";;

print_int (perfOf env);;
print_string "\n";;
print_int (lancerAgent env agent_reflex_etat 0 10);;


(* Emplacement B*)
(* P,S 9 *)
(* S,S 19 *)
(* P,P -1 *)
(* S,P 9 *)
(* Emplacement A*)
(* S,S 19 *)
(* P,P -1 *)
(* P,S 9 *)
(* S,P 9 *)

(* moyenne : 9  augmentation  88% *)
