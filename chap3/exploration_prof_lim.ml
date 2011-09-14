open Taquin;;

type noeud = Noeud of (etat * noeud * action * int * int) | Racine of (etat * int * int );;

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

type retourDLS = Echec | Interruption | Solution of (action list);;

let rec dls noeud pb limite =
	match noeud with
	| Noeud (e, _, _, _, p) | Racine(e, _, p) ->
			match pb with Pb(_, _, ef, _) ->
					if (e = ef) then Solution( solution noeud)
					else if (p = limite) then Interruption
					else
						let succs = developper noeud pb in
						let rec ss_f l = match l with
							| [] -> Echec
							| t:: q -> let result = dls t pb limite in
									match result with
									| Interruption -> ss_f q
									| Solution(l2) -> result
									| _ -> ss_f q
						in let res = (ss_f succs) in res ;;
						
					

let exploration_en_profondeur_limite pb limite =
	match pb with
	| Pb(ei, _, _, _) -> dls (Racine (ei, 0, 0)) pb limite;;

(* explo *)

let seq : action list ref = { contents = []};;

let lancer : bool ref = { contents = false };;

let rec agent_simple_resolution_probleme pb lim =
	match (!lancer,!seq ) with
	| (true, [] ) -> failwith "finit"
	| (true, t:: q ) -> seq := q; t
	| (false, _ ) -> lancer := true;
			match (exploration_en_profondeur_limite pb lim) with
			| Echec -> failwith "Pas de solution"
			| Interruption -> failwith "Solution is out of limit"
			| Solution(l) -> seq := l ; agent_simple_resolution_probleme pb lim;;

let lancerAgent pb lim =
	let rec ss_f etat = afficher etat; ss_f (successeur etat (agent_simple_resolution_probleme pb lim))
	in match pb with
	| Pb(ei, _, _, _ ) -> ss_f ei;;

lancerAgent taquinPb 15;;

