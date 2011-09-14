type ('a, 'b) csp = CSP of (('a list) * ('b list) * ((('a -> 'b -> 'a -> 'b -> bool) * 'a * 'a) list) );;

let affectation_complete aff csp =
	match csp with CSP (vr, vl, c) ->
			if ((List.length vr) != (List.length aff)) then
				false
			else
				let rec ss_f l = match l with
					| [] -> true
					| (f, v1, v2):: q -> ((f v1 (List.assoc v1 aff) v2 (List.assoc v2 aff) )) &&	ss_f q
				in ss_f c;;

let aff_consistante csp aff vari valu =
	let aff = (vari, valu):: aff in
	match csp with CSP (vr, vl, c) ->
			let rec ss_f l = match l with
				| [] -> true
				| (f, v1, v2):: q ->
						if( (List.mem_assoc v1 aff) && (List.mem_assoc v2 aff) ) then
							((f v1 (List.assoc v1 aff) v2 (List.assoc v2 aff) )) &&	ss_f q
						else
							ss_f q
			in ss_f c;;


type ('a, 'b) result = Sol of (('a * 'b) list) | Echec;;

(* reines *)

let non_meme_ligne (vr1:'a) v1 (vr2:'a) v2 = v1 != v2;;
let non_diag vr1 v1 vr2 v2 =
	let x1 = int_of_string (String.sub vr1 1 ((String.length vr1) - 1)) in
	let x2 = int_of_string (String.sub vr2 1 ((String.length vr2) - 1)) in
	abs (x2 - x1) != abs (v2 - v1);;

let rec contraintes_reine i k = if k = 0 then []
	else let r = ("R"^string_of_int i) in
		let kp = ("R"^string_of_int (k)) in
		(non_meme_ligne, r, kp):: (non_diag, r, kp):: (contraintes_reine i (k - 1));;

(*
contraintes_reine 8 7;;
*)

let reines n =
	let rec ss_f i s1 s2 s3 =
		if i = 0 then (s1, s2, s3)
		else
			let r = ("R"^(string_of_int i)) in
			if i = 1 then ss_f (i - 1) (r:: s1) (i:: s2) s3
			else ss_f (i - 1) (r:: s1) (i:: s2) ((contraintes_reine i (i - 1))@s3)
	in CSP (ss_f n [] [] []);;

(*
reines 8;;
*)


let afficher s csp = 
	match csp with CSP (vr, vl, c) ->
		let size = List.length vl in 
	match s with
	| Echec -> print_endline "pas de sol"
	| Sol(l) -> let ll = List.sort (fun x -> fun y -> let(z1, z2) = x in let (z3, z4) = y in
										z4 - z2) l in
			let rec ss_f s i tmp =
				if (s == 0) then
					()
				else if (s == i) then ( print_string "R|" ; ss_f (s-1) i tmp )
				else if (((s + tmp) mod 2) == 0) then (print_string "_|" ; ss_f (s-1) i tmp)
				else (print_string "_|" ; ss_f (s-1) i tmp)
			in 
			List.iter (fun z -> let (vr, va) = z in
							let x1 = int_of_string (String.sub vr 1 ((String.length vr) - 1)) in
							print_newline () ; ss_f size x1 va) ll;;
							
							
							