Load Csp_reines;;

let raisonnable aff csp vari =
	match csp with CSP (vr, vl, c) ->
		let rec ss_f s =
			match s with
				| [] -> 0
				| t::q ->
					if (aff_consistante csp aff vari t) then
						1 + ss_f q
					else ss_f q
		in ss_f vl;;

(*
BT simple
let select_var_non_affec aff csp =
	match csp with CSP (vr, vl, c) ->
			List.hd (List.filter (fun x -> not (List.mem_assoc x aff) ) vr);;
*)

(* MRV *)
let select_var_non_affec aff csp =
	match csp with CSP (vr, vl, c) ->
		let ll = (List.filter (fun x -> not (List.mem_assoc x aff) ) vr) in
		if (List.length ll == 1 ) then
			List.hd ll
	  else
		 let lll = List.map (fun x -> (x,raisonnable aff csp x) ) ll in
		 let llll = List.sort (fun x-> fun y-> let (z1,z2) = x in let (z3,z4) = y in
															z2 - z4) lll
		in fst (List.hd llll);;

let ordonner_valeur_domaine var aff csp =
	match csp with CSP (vr, vl, c) ->
			vl;;

let rec backtracking aff csp =
	if (affectation_complete aff csp) then
		Sol (aff)
	else
		match csp with CSP (vr, vl, c) ->
				let var = select_var_non_affec aff csp in
				let iter = ordonner_valeur_domaine var aff csp in
				let rec ss_f l = match l with
					| [] -> Echec
					| t:: q ->
							if (aff_consistante csp aff var t) then
								let resu = backtracking ((var, t):: aff) csp in
								if (resu == Echec) then
									ss_f q
								else resu
							else ss_f q
				in ss_f iter;;


(*
select_var_non_affec [("R1", 2); ("R2", 2); ("R3", 2); ("R4", 2); ("R5", 2); ("R6", 2); ("R7", 2); ("R8", 2)] rcsp;;
affectation_complete [("R1", 4); ("R2", 3); ("R3", 1); ("R4", 7); ("R5", 2); ("R6", 8); ("R7", 6); ("R8", 4)] rcsp;;
affectation_complete [("R1", 5); ("R2", 3); ("R3", 1); ("R4", 7); ("R5", 2); ("R6", 8); ("R7", 6); ("R8", 4)] rcsp;;

aff_consistante rcsp [("R3", 1); ("R4", 7); ("R5", 2); ("R6", 8); ("R7", 6); ("R8", 4)] "R1" 4;;

let rcsp = reines 4;;
select_var_non_affec [("R3", 2); ("R2", 4); ] rcsp;;
*)

let rec limit i = 
	let rcsp = reines i in
	let t = Sys.time () in
	afficher (backtracking [] rcsp) rcsp;
	let t2 = Sys.time () in
	let diff = (t2 -. t) in
	print_int i ;
	print_newline ();
	print_float ( diff );
	print_endline "s";
	if ( diff >= 60.0 ) then
		(print_endline "limite attente")
	else
	limit (i+1);;


limit 36;;

(* Backtracing simple : limite : 16 *)
(* BT + MRV : lim : 35 ~~*)
(* *)
(* *)
(* *)
(* *)
(* *)