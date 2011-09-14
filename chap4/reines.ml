
type etat = E of ((int * int) list);;

type action = H | B | D | G | HD | BD | HG | BG ;;

let size = (8, 8);;

let trans action x y =
	match action with
	| H -> (x, y + 1)
	| B -> (x, y - 1)
	| D -> (x + 1, y)
	| G -> (x - 1, y)
	| HD -> (x + 1, y + 1)
	| HG -> (x - 1, y + 1)
	| BG -> (x - 1, y - 1)
	| BD -> (x + 1, y - 1);;

let transformation etat action index =
	match etat with E (l) ->
			if( index >= List.length l ) then
				failwith "Out of bounds : tranformation"
			else
				let (x, y) = List.nth l index in
				let n_l = List.filter (fun z -> let (z1, z2) = z in (x != z1 || y != z2)) l in
				E((trans action x y):: n_l);;

let possible etat index =
	match etat with E (l) ->
			if( index >= List.length l ) then
				failwith "Out of bounds : possible"
			else
				let (x, y) = List.nth l index in
				let (max_x, max_y) = size in
				let rec ss_f ll s = match ll with
					| [] -> s
					| t:: q -> let pos_n = (trans t x y ) in
							let (n_x, n_y) = pos_n in
							if(n_x >= 0 && n_y >= 0 && n_x < max_x && n_y < max_y &&
								not (List.mem pos_n l) ) then
								ss_f q (t:: s)
							else ss_f q s
				in ss_f [H; B; D; G; HD; BD; HG; BG] [];;

let rec sss_f l s index etat = match l with
	| [] -> s
	| t:: q -> sss_f q ((t, index, (transformation etat t index))::s) index etat;;

let fn_successeurs etat = match
	etat with E(l) -> let rec ss_f ll index s =
				match ll with | [] -> s
				| t:: q -> let ac_pos = (possible etat index) in
						ss_f q (index + 1) (sss_f ac_pos [] index etat)@s
			in ss_f l 0 [];;

let etat_initial = (E([(0,3);(1,2);(2,1);(3,4);(4,3);(5,2);(6,1);(7,2)]));;

let etat_initial = E ([(0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7)]);;

List.length (fn_successeurs etat_initial);;

let appartient = List.mem;;
let supprimer_doublon l = 
			let rec g l s = match l with
				| [] -> s
				| t::q when ( appartient t s ) -> g q s
				| t::q -> g q (s@[t])
			in g l [];;

let supprimer_doublon_2 l = 
			let rec g l s = match l with
				| [] -> s
				| (a,b)::q when ( appartient (a,b) s ) -> g q s
				| (a,b)::q when ( appartient (b,a) s ) -> g q s
				| (a,b)::q -> g q (s@[(a,b)])
			in g l [];;

let s_croisement etat index = match etat with
	| E(l) -> let (x,y) = List.nth l index in
		 	let n_l = List.filter (fun (z1,z2) -> (x != z1 || y != z2)) l in
		let ligne = List.filter (fun (x2,y2) -> (x2 = x || y2 = y)) n_l in
		let diag = List.filter (fun (x2,y2) -> abs (x2-x) = abs (y2-y)) n_l in
		List.map (fun c -> (c,(x,y))) (supprimer_doublon ligne@diag);;

s_croisement (E([(0,3);(1,2);(2,1);(3,4);(4,3);(5,2);(6,1);(7,2)])) ;;

let croisement etat = match etat with 
	| E(l) -> let rec ss_f ll index = 
			if (index >= List.length l) then []
			else (s_croisement etat index)@(ss_f ll (index+1))
		in List.length( supprimer_doublon_2 (ss_f l 0));;

croisement (E([(0,3);(1,2);(2,1);(3,4);(4,3);(5,2);(6,1);(7,2)]));;

let etat_final et =( croisement et ) = 0;;


type probleme = Pb of etat * (etat -> (action * int * etat) list ) * (etat->bool) * (etat->int);;

let reinesPb = Pb ( etat_initial, fn_successeurs, etat_final, croisement) ;;

