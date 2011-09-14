open Reines;;

type noeud = Noeud of (etat * int * int);;

type retour = Echec | Solution of (etat);;

let developper noeud probleme =
	match noeud with
	| Noeud(e, _, p) -> match probleme with
			| Pb(ei, fn, tf, h ) ->
					let rec ss_f l s = match l with
						| [] -> s
						| (_, _, et):: q -> ss_f q ((Noeud(et, (h et), p + 1)):: s)
					in ss_f (fn e) [];;

developper (Noeud(etat_initial, croisement etat_initial, 0)) reinesPb;;

let comparer n1 n2 = match n1 with 
		| Noeud(_,c1, _) -> match n2 with
			|  Noeud(_, c2, _) -> c1 - c2;; 


(* ----- *)
let escalade pb = match pb with
	| Pb (ei, _, _, h) ->
			let rec ss_f courant =
				let voisin = List.hd (List.sort comparer (developper courant pb)) in
				match voisin with
				| Noeud(_, cv, _) -> match courant with
						| Noeud(_, cc, _ ) ->
								if (cv >= cc) then
									courant
								else ss_f voisin
			in ss_f (Noeud(ei, (h ei), 0)) ;;

Random.self_init ();;
let escalade_ran pb = match pb with
	| Pb (ei, _, _, h) ->
			let rec ss_f courant =
				let ll = (List.sort comparer (developper courant pb)) in
				let Noeud( _,min_c,_) = List.hd (ll) in
				let lll = List.filter (fun (Noeud(_,c,_)) -> min_c = c ) ll in
				let voisin = List.nth lll (Random.int (List.length lll)) in
				match voisin with
				| Noeud(_, cv, _) -> match courant with
						| Noeud(_, cc, _ ) ->
								if (cv >= cc) then
									courant
								else ss_f voisin
			in ss_f (Noeud(ei, (h ei), 0)) ;;

let cherche_escalade = let n = escalade reinesPb in
	match n with Noeud(e, c, p) -> print_endline ("pf : "^(string_of_int p));
			print_endline ("perfect : "^(string_of_int c));
			e;;

let cherche_escalade_ran = let n = escalade_ran reinesPb in
	match n with Noeud(e, c, p) -> print_endline ("pf : "^(string_of_int p));
			print_endline ("perfect : "^(string_of_int c));
			e;;


let ran_ei = let rec ss_f i s =
		if (i > 8 ) then s
		else
			let (x, y) = ((Random.int 8), (Random.int 8)) in
			if (List.mem (x, y) s) then
				ss_f i s
			else ss_f (i + 1) ((x, y):: s)
	in ss_f 0 [];;

let reinesPb = Pb ( etat_initial, fn_successeurs, etat_final, croisement) ;;

let cherche_escalade_ran = let n = escalade_ran reinesPb in
	match n with Noeud(e, c, p) -> print_endline ("pf : "^(string_of_int p));
			print_endline ("perfect : "^(string_of_int c));
			e;;




(*-----------------*)

let escalade_ran_2 pb = match pb with
	| Pb (ei, _, _, h) ->
			let rec ss_f i courant =
				let ll = (List.sort comparer (developper courant pb)) in
				let Noeud( _,min_c,_) = List.hd (ll) in
				let lll = List.filter (fun (Noeud(_,c,_)) -> min_c = c ) ll in
				let voisin = List.nth lll (Random.int (List.length lll)) in
				match voisin with
				| Noeud(_, cv, _) -> match courant with
						| Noeud(_, cc, _ ) ->
								if ((cv >= cc && i=0) || cc = 0) then
									courant
								else if (cv >= cc) then
									ss_f (i-1) voisin
								else ss_f i voisin
			in ss_f 60 (Noeud(ei, (h ei), 0)) ;;


let ran_ei = let rec ss_f i s =
		if (i > 8 ) then s
		else
			let (x, y) = ((Random.int 8), (Random.int 8)) in
			if (List.mem (x, y) s) then
				ss_f i s
			else ss_f (i + 1) ((x, y):: s)
	in ss_f 0 [];;

let reinesPb = Pb ( etat_initial, fn_successeurs, etat_final, croisement) ;;

let cherche_escalade_ran = let n = escalade_ran_2 reinesPb in
	match n with Noeud(e, c, p) -> print_endline ("pf : "^(string_of_int p));
			print_endline ("perfect : "^(string_of_int c));
			e;;


