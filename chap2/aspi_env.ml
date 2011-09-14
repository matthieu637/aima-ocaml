
type envAspi = E of grille * perf
and grille = G of dimension * etatGrille
and dimension = D of int * int
and etatGrille = EG of (etat * int) list
and etat = Propre | Sale
and perf = int;;

let creerGrille x y =
	G (D(x, y), EG []);;

let creerEnvAspi x y = E((creerGrille x y), 0);;

let point_perdu = - 1;;
let point_gagne = 10;;

let modif x y env et = match env with
	| E(g, p) -> match g with
			| G(d, eg) -> match d with
					| D(x2, y2) -> if( x >= x2 || y >= y2 || x < 0 || y < 0)
							then failwith "modif : out of bounds"
							else match eg with
								| EG l -> let rec ss_f l s perf = match l with
											| [] -> E(G(D(x2, y2), EG s), p + perf)
											| t:: q -> match t with
													| (etat, i) -> if ( i == y * y2 + x )
															then match (etat, et) with
																| (Propre, Propre) -> ss_f q ((et, i):: s) point_perdu
																| (Propre, Sale) -> ss_f q ((et, i):: s) perf
																| (Sale, Propre) -> ss_f q ((et, i):: s) point_gagne
																| (Sale, Sale ) -> ss_f q ((et, i):: s) perf
															else	ss_f q ((etat, i):: s) perf
										in ss_f l [] 0;;

let get x y env = match env with
	| E(g, p) -> match g with
			| G(d, eg) -> match d with
					| D(x2, y2) -> if( x >= x2 || y >= y2 || x < 0 || y < 0)
							then (failwith ("get : out of bounds (x="^(string_of_int x)^", y="^(string_of_int y)^")") )
							else match eg with
								| EG l -> let rec ss_f l = match l with
											| [] -> Propre
											| (e, i):: q -> if( i == y * y2 + x) then e
													else ss_f q
										in ss_f l;;

let i_to_xy i env = match env with
	| E(g, p) -> match g with
			| G(d, eg) -> match d with
					| D(x2, y2) -> (i mod x2, i / x2);;

let addPerf env p = match env with
	| E(g, pe) -> E(g, pe + p);;

let perfOf env = match env with
	| E(g, p) -> p;;

let salir x y env = modif x y env Sale;;
let nettoyer x y env = modif x y env Propre;;

let salirEnv env = match (env,Random.self_init ()) with
	| E(g, p),e -> match g with
			| G(d, eg) -> match d with
					| D(x, y) -> let rec ss_f i s = match i with
								| - 1 -> s
								| _ -> if( Random.bool ()) then
											ss_f (i - 1) ((Sale, i):: s)
										else ss_f (i - 1) ((Propre, i):: s)
							in E(G(D(x, y), EG(ss_f ((x * y) - 1) [])), p);;

let afficher env = match env with
	| E(g, p) -> match g with
			| G(d, eg) -> match d with
					| D(x, y) -> match eg with
							| EG(l) -> let rec ss_f i s = match i with
										| - 1 -> s
										| _ -> match (i_to_xy i env) with
												| (x2, y2) -> match (get x2 y2 env) with
														| Sale -> ss_f (i - 1) (" Sale "^(string_of_int i)^"\n")^s
														| Propre -> ss_f (i - 1) (" Propre "^(string_of_int i)^"\n")^s
									in print_string (ss_f ((x * y) - 1) "");;

