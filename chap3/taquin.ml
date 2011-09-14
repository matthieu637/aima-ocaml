(* Formulation du probleme *)

type etat = E of string;;

type action = Droite | Gauche | Haut | Bas ;;

let contraire a = match a with
	| Droite -> Gauche
	| Gauche -> Droite
	| Haut -> Bas
	| Bas -> Haut;;

let etat_initial = E "351408627";; (* middle : 11 pf all *)
let etat_initial = E "312475068";; (* light : 5 pf  all *)
let etat_initial = E "071325468";; (* 50 rand : 15 pf prof limi & graphe*)
let etat_initial = E "724506831";; (* harder on book : pf = 26  bi *)
let etat_initial = E "260315874";; (* rand 1000 : pf = 22 bi*)
let etat_initial = E "178502643";; (* rand 10^6 : pf = 24 bi*)
let etat_initial = E "803524617";; (* pas de solution *)
let etat_initial = E "876543210";; (* pf = 28 bi*)

let etat_final = E "012345678";;

let afficher etat = match etat with
	| E s ->
			print_string "\n";
			print_string (String.sub s 0 3); print_string "\n";
			print_string (String.sub s 3 3); print_string "\n";
			print_string (String.sub s 6 3); print_string "\n";;

afficher etat_initial;;
afficher etat_final;;

let action_possible etat = match etat with
	| E s -> match (String.index s '0') with
			| 0 -> [Droite; Bas]
			| 1 -> [Gauche; Bas; Droite ]
			| 2 -> [Gauche; Bas]
			| 3 -> [Haut; Droite; Bas]
			| 4 -> [Haut; Bas; Droite; Gauche]
			| 5 -> [Gauche; Haut; Bas]
			| 6 -> [Haut; Droite]
			| 7 -> [Haut; Droite; Gauche]
			| 8 -> [Haut; Gauche]
			| _ -> failwith "action_possible";;

let successeur etat action = match etat with
	| E s -> let index = (String.index s '0') in
			let replace st i j = let min_index = min i j in
				let max_index = max i j in
				(String.sub st 0 min_index)^
				(Char.escaped (String.get st max_index))^
				(String.sub st (min_index + 1) (max_index - 1 - min_index))^
				(Char.escaped (String.get st min_index))^
				(String.sub st (max_index + 1) ((String.length st) - 1 - max_index))
			in
			match action with
			| Droite -> E (replace s index (index + 1))
			| Gauche -> E (replace s index (index - 1))
			| Bas -> E (replace s index (index + 3))
			| Haut -> E (replace s index (index - 3));;

let fn_successeur etat =
	let ac = action_possible etat in
	let rec ss_f l s = match l with
		| [] -> s
		| t:: q -> ss_f q ((t, (successeur etat t)):: s)
	in ss_f ac [];;

Random.self_init ();;

let rec melange etat i =
	if(i = 0 ) then etat
	else
		let l = fn_successeur etat in
		let index_ac = (Random.int (List.length l)) in
		match (List.nth l index_ac) with
		| (a, e) -> melange e (i - 1);;

type probleme = Pb of etat * (etat -> (action * etat) list ) * etat * int;;

let taquinPb = Pb ( etat_initial, fn_successeur, etat_final, 1) ;;

let distanceDe s i =
	match etat_final with
	| E(f) ->
			let char = String.get s i in
			let diff = String.index f char in
			let indexD = (i mod 3, i / 3) in
			let indexF = (diff mod 3, diff / 3) in
			(abs (fst indexF - fst indexD ) ) + (abs (snd indexF - snd indexD) );;

let h2 etat = match etat with
	| E(s) -> let rec ss_f i = if (i = String.length s) then
					0
				else distanceDe s i + (ss_f (i+1))
			in ss_f 0;;




