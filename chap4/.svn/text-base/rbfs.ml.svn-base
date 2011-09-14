open Taquin;;

type noeud = Noeud of (etat * noeud * action * float * int) | Racine of (etat * float * int );;

type retour = Echec | Solution of (action list);;

let developper noeud probleme =
	match noeud with
	| Noeud(e, _, _, c, p) | Racine(e, c, p) -> match probleme with
			| Pb(ei, fn, ef, i ) ->
					let rec ss_f l s = match l with
						| [] -> s
						| (ac, et):: q -> ss_f q ((Noeud(et, noeud, ac, (float_of_int p) +. float_of_int (h2 et), p + 1)):: s)
					in ss_f (fn e) [];;

let rec solution noeud = match noeud with
	| Noeud(e, pa, a, c, pf) -> (solution pa)@[a]
	| Racine(_, _, _) -> [];;

let etat_to_int etat = match etat with
	| E(s) -> int_of_string s;;

let comparer n1 n2 = match n1 with
	| Noeud(_, _, _, c1, _) | Racine(_, c1, _) -> match n2 with
			| Noeud(_, _, _, c2, _) | Racine(_, c2, _) -> int_of_float c1 -  int_of_float c2;;

let best l =
	let sl = List.fast_sort comparer l in
	((List.hd sl), (List.tl sl));;



let rec rbfs pb noeud lim =
	match pb with Pb(_, _, ef, _ ) ->
			match noeud with Noeud(e, _, _, _, _) | Racine(e, _, _) ->
					let sol : (retour * float) ref = { contents = (Solution ([]), 0.) } in
					if (e = ef ) then
						(sol := (Solution (solution noeud), 0.))
					else
						(let successeurs : noeud list ref = { contents = (developper noeud pb) } in
							if (!successeurs = [] ) then
								(sol := (Echec, infinity ))
							else
								(while ( (fst !sol) = Solution([]) ) do
										let (m, al) = best !successeurs in
										match m with Noeud(_, _, _, cm, _) | Racine(_, cm, _) ->
												if( cm > lim ) then
													(sol := (Echec, cm))
												else 
														(let a = List.hd al in
															match a with Noeud(_, _, _, ca, _) | Racine(_, ca, _) ->
																	let (r, n_cm) = rbfs pb m (min lim ca) in
																	if(r = Echec ) then
																		(match m with
																			| Noeud(et, pa, ac, c, pf ) -> successeurs := ((Noeud (et, pa, ac, n_cm, pf)):: al)
																			| Racine(et, c, pf ) -> successeurs := ((Racine (et, n_cm, pf)):: al);
																		)
																	else
																		(sol := (r, 0.)) 
														) ;
														
									done
								) ;
						) ;
							!sol;;

let exploration_brfs pb = 
	match pb with Pb(ei, _, _, _ ) -> 
		fst (rbfs pb (Racine(ei, infinity, 0)) infinity);;







let seq : action list ref = { contents = []};;

let lancer : bool ref = { contents = false };;

let rec agent_simple_resolution_probleme pb =
	match (!lancer,!seq ) with
	| (true, [] ) -> failwith "finit"
	| (true, t:: q ) -> seq := q; t
	| (false, _ ) -> lancer := true;
			match (exploration_brfs pb) with
			| Echec -> failwith "Pas de solution"
			| Solution(l) -> seq := l ; print_endline ("sol : "^(string_of_int (List.length l))); agent_simple_resolution_probleme pb;;

let lancerAgent pb =
	let rec ss_f etat = afficher etat; ss_f (successeur etat (agent_simple_resolution_probleme pb))
	in match pb with
	| Pb(ei, _, _, _ ) -> ss_f ei;;

lancerAgent taquinPb;;




