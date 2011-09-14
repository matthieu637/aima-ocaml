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

let rec solution_inverse noeud = match noeud with
	| Noeud(e, pa, a, c, pf) -> [(contraire a)]@(solution_inverse pa)
	| Racine(_, _, _) -> [];;

let etat_to_int etat = match etat with
	| E(s) -> int_of_string s;;

let exploration_en_graphe pb =
	let sol : action list ref = { contents =[]} in
	let frontiereD : noeud list ref = { contents = []} in
	let frontiereF : noeud list ref = { contents = []} in
	let liste_fermeD = (Hashtbl.create 362881) in
	let liste_fermeF = (Hashtbl.create 362881) in
	let tour : bool ref = { contents = false } in
	match pb with
	| Pb(ei, _, ef, _) ->
			frontiereD := (Racine (ei, 0, 0))::!frontiereD;
			frontiereF := (Racine (ef, 0, 0))::!frontiereF;
			while (((!frontiereD != []) && (!frontiereF != [])) && (!sol == []) ) do
				tour := !tour = false;
				if(!tour) then
					(match !frontiereD with
						| [] -> failwith "never append"
						| (Noeud(e, _, _, _, p)):: q | (Racine(e, _, p)):: q ->
								let noeud = List.hd !frontiereD in
								let hash = etat_to_int e in
								frontiereD := List.tl !frontiereD;
								if ef = e then
									(sol := (solution noeud))
								else if (Hashtbl.mem liste_fermeF hash) then
									(sol := (solution noeud)@(solution_inverse (Hashtbl.find liste_fermeF hash)))
								else if ((Hashtbl.mem liste_fermeD hash) = false) then
									begin
										Hashtbl.add liste_fermeD hash noeud;
										print_string
											("tailleD : "^(string_of_int (Hashtbl.length liste_fermeD))^
												(" profD : "^(string_of_int p)));
										frontiereD := !frontiereD@(developper noeud pb)
									end
								else (print_string
											("tailleD : "^(string_of_int (Hashtbl.length liste_fermeD))^
												(" profD : "^(string_of_int p)))))
				else
					(match !frontiereF with
						| [] -> failwith "never append"
						| (Noeud(e, _, _, _, p)):: q | (Racine(e, _, p)):: q ->
								let noeud = List.hd !frontiereF in
								let hash = etat_to_int e in
								frontiereF := List.tl !frontiereF;
								if ei = e then
									(sol := (solution noeud))
								else if (Hashtbl.mem liste_fermeD hash) then
									(sol := (solution (Hashtbl.find liste_fermeD hash))@(solution_inverse noeud))
								else if ((Hashtbl.mem liste_fermeF hash) = false) then
									begin
										Hashtbl.add liste_fermeF hash noeud;
										print_endline
											("\ttailleF : "^(string_of_int (Hashtbl.length liste_fermeF))^
												(" profF : "^(string_of_int p)));
										frontiereF := !frontiereF@(developper noeud pb)
									end
								else (print_endline
											("\ttailleF : "^(string_of_int (Hashtbl.length liste_fermeF))^
												(" profF : "^(string_of_int p))))
					)
			done;
			print_endline (string_of_bool !tour);
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
