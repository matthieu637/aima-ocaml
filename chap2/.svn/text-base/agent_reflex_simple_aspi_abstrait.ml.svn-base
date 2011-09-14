open Aspi_env;;

type percept = P of (case * etat)
and case = A | B;;

type etatA = E of case * etat;;

type regle = Droite | Gauche | Nettoyer;;

type action = Aspirer | ADroite | AGauche;;

let interpreter_entree p = match p with
	| P(c, e) -> E(c, e);;

let trouver_regle e = match e with
	| E(A, Propre ) -> Droite
	| E(A, Sale ) -> Nettoyer
	| E(B, Propre) -> Gauche
	| E(B, Sale ) -> Nettoyer;;

let regle_action r = match r with
	| Droite -> ADroite
	| Gauche -> AGauche
	| Nettoyer -> Aspirer;;

let agent_reflex_simple p =
	let e = interpreter_entree p in
	let	regle = trouver_regle e in
	regle_action regle;;


let creerPercept i = match i with
	| 0 -> A
	| _ -> B;;
	
let rec lancerAgent env f i n = if(n > 0) then match (i_to_xy i env) with
		| (x, y) -> match (f (P(creerPercept i,(get x y env)))) with
				| Aspirer -> lancerAgent (nettoyer x y env) f i (n - 1)
				| ADroite -> lancerAgent (addPerf env point_perdu) f (i + 1) (n - 1)
				| AGauche -> lancerAgent (addPerf env point_perdu) f (i - 1) (n - 1)
	else (perfOf env);;


let env = (creerEnvAspi 2 1);;
let env = salirEnv env;;

afficher env;;

print_string "-------\n";;

print_int (perfOf env);;
print_string "\n";;
print_int (lancerAgent env agent_reflex_simple 1 10);;