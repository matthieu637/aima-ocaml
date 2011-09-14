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

let etat_to_int etat = match etat with
	| E(s) -> int_of_string s;;

let exploration_en_graphe pb =
	let sol : action list ref = { contents =[]} in
	let frontiere : noeud list ref = { contents = []} in
	let liste_ferme = (Hashtbl.create 362881) in
	match pb with
	| Pb(ei, _, ef, _) -> frontiere := (Racine (ei, 0, 0))::!frontiere;
			while ((!frontiere != []) && (!sol = []) ) do
				match !frontiere with
				| [] -> failwith "never append"
				| (Noeud(e, _, _, _, p)):: q | (Racine(e, _, p)):: q ->
						let noeud = List.hd !frontiere in
						frontiere := List.tl !frontiere;
						if ef = e then
							(sol := (solution noeud))
						else if ((Hashtbl.mem liste_ferme (etat_to_int e)) = false) then
							begin
								Hashtbl.add liste_ferme (etat_to_int e) true;
								print_endline
									("taille : "^(string_of_int (Hashtbl.length liste_ferme))^(" prof : "^(string_of_int p)));
								frontiere := !frontiere@(developper noeud pb)
							end
						else ()
			done;
			if (!sol = [] ) then Echec
			else Solution (!sol)
;;

let seq : action list ref = { contents = []};;

let lancer : bool ref = { contents = false };;

let rec agent_simple_resolution_probleme pb =
	match (!lancer,!seq ) with
	| (true, [] ) -> failwith "finit"
	| (true, t:: q ) -> seq := q; t
	| (false, _ ) -> lancer := true;
			match (exploration_en_graphe pb) with
			| Echec -> failwith "Pas de solution"
			| Solution(l) -> seq := l ; agent_simple_resolution_probleme pb;;

let lancerAgent pb =
	let rec ss_f etat = afficher etat; ss_f (successeur etat (agent_simple_resolution_probleme pb))
	in match pb with
	| Pb(ei, _, _, _ ) -> ss_f ei;;

lancerAgent taquinPb;;

