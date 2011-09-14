open Reines;;

type noeud = Noeud of (etat * int * int);;

type retour = Echec | Solution of (etat);;

let developper noeud probleme =
	match noeud with
	| Noeud(e, _, p) -> match probleme with
			| Pb(ei, fn, tf, h ) ->
					let rec ss_f l s = match l with
						| [] -> s
						| (_, _, et):: q -> ss_f q ((Noeud(et,(h et), p + 1)):: s)
					in ss_f (fn e) [];;

developper (Noeud(etat_initial, croisement etat_initial, 0)) reinesPb;;

let comparer n1 n2 = match n1 with 
		| Noeud(_,c1, _) -> match n2 with
			|  Noeud(_, c2, _) -> c1 - c2;; 

(* a_etoile glouton *)
let a_etoile pb =
	let sol : etat ref = { contents =E([])} in
	let frontiere : noeud list ref = { contents = []} in
	let liste_ferme = (Hashtbl.create (362881)) in
	match pb with
	| Pb(ei, _, ef, h2) -> frontiere := (Noeud (ei, (h2 ei), 0))::!frontiere;
			while ((!frontiere != []) && (!sol = (E[])) ) do
				match !frontiere with
				| [] -> failwith "never append"
				| (Noeud(e, c, p)):: q ->
						let noeud = List.hd !frontiere in
						frontiere := List.tl !frontiere;
						if (ef e) then
							(sol := e)
						else if ((Hashtbl.mem liste_ferme e) = false) then
							begin
								Hashtbl.add liste_ferme e true;
								print_endline
									("taille : "^(string_of_int (Hashtbl.length liste_ferme))^(" prof : "^(string_of_int p))^(" cout : "^(string_of_int c)));
								frontiere := (List.sort comparer ((developper noeud pb)@(!frontiere)))
							end
						else ()
			done;
			if (!sol = (E[]) ) then Echec
			else Solution (!sol)
;;

a_etoile reinesPb;;
