open Taquin;;

type noeud = Noeud of (etat * noeud * action * int * int) | Racine of (etat * int * int );;

type retour = Echec | Solution of (action list);;

let developper noeud probleme =
	match noeud with
	| Noeud(e, _, _, c, p) | Racine(e, c, p) -> match probleme with
			| Pb(ei, fn, ef, i ) ->
					let rec ss_f l s = match l with
						| [] -> s
						| (ac, et):: q -> ss_f q ((Noeud(et, noeud, ac, c + i, p + 1)):: s)
					in ss_f (fn e) [];;

let rec solution noeud = match noeud with
	| Noeud(e, pa, a, c, pf) -> (solution pa)@[a]
	| Racine(_, _, _) -> [];;


(* Exploration en largeur d'abord *)
let exploration_en_arbre pb =
	let frontiere : noeud list ref = { contents = []} in
	match pb with
	| Pb(ei, _, ef, _) -> frontiere := (Racine (ei, 0, 0))::!frontiere;
			let rec ss_f f = match f with
				| [] -> Echec
				| (Noeud(e, _, _, _, p)):: q | (Racine(e, _, p)):: q ->
						let noeud = List.hd f in
						(* print_endline ("p : "^(string_of_int p)); *)
						if ef = e then Solution (solution noeud)
						else ss_f (q@(developper noeud pb))
			in ss_f !frontiere;;



let seq : action list ref = {contents = []};;

let lancer : bool ref= {contents = false};;

let rec agent_simple_resolution_probleme pb = 
match (!lancer,!seq ) with
	| (true, [] ) -> failwith "finit"
	| (true, t::q ) -> seq := q; t
	| (false, _ ) -> lancer := true;
										match (exploration_en_arbre pb) with
											| Echec -> failwith "Pas de solution"
											| Solution(l) -> seq := l ; agent_simple_resolution_probleme pb;;


let lancerAgent pb = 
	let rec ss_f etat = afficher etat; ss_f (successeur etat (agent_simple_resolution_probleme pb))
		in match pb with
			| Pb(ei, _, _, _ ) -> ss_f ei;;



lancerAgent taquinPb;;