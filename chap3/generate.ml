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
(* bonus *)

let profmax pb limit =
	let sol : etat ref = { contents = E"" } in
	let frontiere : noeud list ref = { contents = []} in
	let liste_ferme = (Hashtbl.create 362881) in
	match pb with
	| Pb(_, _, ef, _) -> frontiere := (Racine (ef, 0, 0))::!frontiere;
			while ((!frontiere != []) && (!sol = E"") ) do
				match !frontiere with
				| [] -> failwith "never append"
				| (Noeud(e, _, _, _, p)):: q | (Racine(e, _, p)):: q ->
						let noeud = List.hd !frontiere in
						frontiere := List.tl !frontiere;
						if (p = limit) then
							(sol := e)
						else if ((Hashtbl.mem liste_ferme (etat_to_int e)) = false) then
							begin
								Hashtbl.add liste_ferme (etat_to_int e) true;
								frontiere := (!frontiere)@(developper noeud pb)
							end
						else ()
			done;
			!sol
;;

let tryme pb debut =
	let sol : etat ref = { contents = E"" } in
	let lim : int ref = { contents = debut } in
	while (!lim != - 1) do
		sol := profmax pb !lim;
		print_endline (string_of_int !lim);
		if(!sol == E"") then
			(lim := - 1)
		else
			(lim := !lim + 1)
	done;
	!sol;;

profmax taquinPb 45000;; (* 93542<  < 93538 *)
profmax taquinPb 93509;; (* 93542<  < 93538 *)
tryme taquinPb 1;;
