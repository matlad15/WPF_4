(* typ reprezuntujacy pojedynczy punkt *)
type point = float * float

(* typ reprezuntujacy kartke jako funkcje mowiaca ile 
razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(* funkcja liczaca, po ktorej stronie prostej wyznaczonej 
przez punkty p1 i p2 znajduje sie punkt p *)
(* jezeli zwroci liczbe > 0 to p znajduje sie po prawej stronie prostej;
jezeli zwroci liczbe = 0 to p znajduje sie na prostej;
jezeli zwroci liczbe < 0 to p znajduje sie po lewej stronie prostej *)
let det ((p1 : point), (p2 : point)) (p : point) =
	let (x1, y1) = p1 and (x2, y2) = p2 and (x, y) = p in
	let nx1 = x2 -. x1 and ny1 = y2 -. y1 and nx = x -. x1 and ny = y -. y1 in
	nx *. ny1 -. ny *. nx1

(* blad pomiarowy przy liczeniu funkcji det
i odleglosci miedzy punktami *)
(* wykorzystywany w funkcji zloz i kolko *)
let eps = 1e-12

(* znajdowanie punktu p', ktory jest odbiciem symetrycznym p wzgledem 
prostej wyznaczonej przez punkty p1 i p2 *)
(* af1 i bf1 to wspolczynniki prostej przechodzacej przez punkty p1 i p2 (K) *)
(* af i bf to wpolczynniki prostej (L) przechodzacej przez 
punkt p i prostopadlej  do prostej K *)
(* pomx i pomy opisuja punkt lezacy na przecieciu prostych K i L (p3) *)
(* rozx i rozy reprezentuja wektor laczacy punkty p i p3 *)
let symetria ((p1 : point), (p2 : point)) (p : point) =
	let (x1, y1) = p1 and (x2, y2) = p2 and (x, y) = p in
	if x1 = x2 then
		let rozx = x1 -. x in ((x +. 2. *. rozx, y) : point)
	else if y1 = y2 then
		let rozy = y1 -. y in ((x, y +. 2. *. rozy) : point)
	else
		let af1 = (y2 -. y1) /. (x2 -. x1) in
		let bf1 = y1 -. af1 *. x1 in
		let af = -1. /. af1 in
		let bf = y -. af *. x in
		let pomx = (bf1 -. bf) /. (af -. af1) in
		let pomy = af *. pomx +. bf in
		let rozx = pomx -. x and rozy = pomy -. y in
		((x +. 2. *. rozx, y +. 2. *. rozy) : point)

(* funkcja liczaca odleglosc miedzy punktami p1 i p2 *)
let odl (p1 : point) (p2 : point) =
	let (x1, y1) = p1 and (x2, y2) = p2 in
	sqrt ((x1 -. x2) *. (x1 -. x2) +. (y1 -. y2) *. (y1 -. y2))

(* funckja tworzaca kartke o ksztalcie prostokata *)
(* lewy dolny wierzcholek prostokata to p1, a prawy gorny to p2 *)
let prostokat (p1 : point) (p2 : point) =
	let (x1, y1) = p1 and (x2, y2) = p2 in
	let przebicia p =
		let (x, y) = p in
		if x < x1 || y < y1 || x > x2 || y > y2 then 0
		else 1
	in (przebicia : kartka)

(* funkcja tworzaca kartke w ksztalcie kola o srodku o i promieniu r *)
let kolko o r =
	let przebicia p =
		if (odl o p <= r +. eps) then 1
		else 0
	in (przebicia : kartka)

(* funkcja skladajaca kartke wzdluz prostej 
wyznaczonej przez punkty p1 i p2 (K) *)
(* czesc kartki k lezaca po prawej stronie prostej K naklada sie na 
czesc kartki k lezaca po lewej stronie prostej K *)
let zloz p1 p2 (k : kartka) =
	let przebicia p =
		let znak = det (p1, p2) p in
		if abs_float znak < eps then k p
		else if znak > 0. then 0
		else  k p + k (symetria (p1, p2) p)
	in (przebicia : kartka)

(* funkja wykonujaca wiele operacji skladania 
kartki k wzdluz prostych z listy lista *)
let skladaj lista k =
	List.fold_left (fun k1 (p1, p2) -> zloz p1 p2 k1) k lista

(* TESTY *)
(*
let a = prostokat (0., 0.) (10., 10.);;
assert(a (0., 0.) = 1);;
assert(a (5., 5.) = 1);;
assert(a (10., 10.) = 1);;
assert(a (10., 0.) = 1);;
assert(a (0., 10.) = 1);;
assert(a (10.1, 0.) = 0);;
assert(a (0., 10.1) = 0);;
assert(a (10.1, 10.1) = 0);;
let a = zloz (5., 0.) (5., 377.) a;;
assert(a (0., 0.) = 2);;
assert(a (-377., 0.) = 0);;
assert(a (5., 2.5) = 1);;
assert(a (2.5, 3.5) = 2);;
assert(a (5., 5.) = 1);;
assert(a (5.1, 5.) = 0);;
assert(a (5.1, 5.1) = 0);;
let a = zloz (5., 0.) (5., 1.) a;;
assert(a (0., 0.) = 2);;
assert(a (-377., 0.) = 0);;
assert(a (5., 2.5) = 1);;
assert(a (2.5, 3.5) = 2);;
assert(a (5., 5.) = 1);;
assert(a (5.1, 5.) = 0);;
assert(a (5.1, 5.1) = 0);;
let c = zloz (-6., -6.) (-6.1, -6.1) a;;
assert(c (0., 0.) = 2);;
assert(c (0., 5.) = 0);;
assert(c (2.5, 2.5) = 2);;
assert(c (1., 2.) = 0);;
assert(c (2.5, 5.) = 0);;
assert(c (2.5, 6.) = 0);;
assert(c (2.5, 2.) = 4);;
assert(c (5., 5.) = 1);;
assert(c (5., 0.) = 3);;
assert(c (4., 2.) = 4);;
assert(c (7., 9.) = 0);;
assert(c (7., 2.) = 2);;
assert(c (7., 3.8) = 2);;
assert(c (5., 2.5) = 3);;
assert(c (10., 0.) = 2);;
assert(c (10., 10.) = 0);;
assert(c (10., 2.5) = 2);;
let a = kolko (3., 3.) 7.;;
assert(a (0., 0.) = 1);;
assert(a (3., 3.) = 1);;
assert(a (8., 7.5) = 1);;
assert(a (10., 3.) = 1);;
assert(a (3., 10.) = 1);;
assert(a (-4., 3.) = 1);;
assert(a (3., -4.) = 1);;
assert(a (10.1, 3.) = 0);;
assert(a (10., 3.1) = 0);;
assert(a (-4.1, 3.) = 0);;
assert(a (-3.9, 3.) = 1);;
let a = zloz (5., -10.) (5., 100.) a;;
assert(a (0., 0.) = 1);;
assert(a (0.67, 0.) = 1);;
assert(a (0.68, 0.) = 2);;
assert(a (0.69, 0.69) = 2);;
assert(a (1., 0.) = 2);;
assert(a (2., 2.) = 2);;
assert(a (3., 0.) = 2);;
assert(a (5., 0.) = 1);;
assert(a (5.1, 0.) = 0);;
assert(a (3., 3.) = 2);;
assert(a (3., 10.) = 1);;
assert(a (-1., -1.) = 1);;
assert(a (7., 7.) = 0);;
assert(a (10., 0.) = 0);;
let a = zloz (5., 0.) (5., 0.01) a;;
assert(a (0., 0.) = 1);;
assert(a (0.67, 0.) = 1);;
assert(a (0.68, 0.) = 2);;
assert(a (0.69, 0.69) = 2);;
assert(a (1., 0.) = 2);;
assert(a (2., 2.) = 2);;
assert(a (3., 0.) = 2);;
assert(a (5., 0.) = 1);;
assert(a (5.1, 0.) = 0);;
assert(a (3., 3.) = 2);;
assert(a (3., 10.) = 1);;
assert(a (-1., -1.) = 1);;
assert(a (7., 7.) = 0);;
assert(a (10., 0.) = 0);;
let l = [((5., -10.), (5., 100.)); ((5., 0.), (5., 0.01));
	((1., 0.), (1., -1.)); ((5., 10.), (1., 0.));
	((1., 0.), (5., 10.))];;
let a = kolko (3., 3.) 7.;;
let a = skladaj l a;;
assert(a (0., 0.) = 3);;
assert(a (1., 0.) = 2);;
assert(a (2., 0.) = 0);;
assert(a (5., 0.) = 0);;
assert(a (6., 0.) = 0);;
assert(a (2., 2.) = 0);;
assert(a (3., 3.) = 0);;
assert(a (5., 5.) = 0);;
assert(a (6., 6.) = 0);;
assert(a (8., 8.) = 0);;
assert(a (4., 3.) = 0);;
assert(a (5., 3.) = 0);;
assert(a (6., 3.) = 0);;
assert(a (7., 3.) = 0);;
assert(a (1., -1.) = 0);;
assert(a (1., -3.) = 0);;
assert(a (1., -4.) = 0);;
assert(a (3., -1.) = 0);;
assert(a (3., -2.) = 0);;
assert(a (3., -3.) = 0);;
assert(a (3., -4.) = 0);;
assert(a (3., -5.) = 0);;
assert(a (0., 4.) = 3);;
assert(a (0., 5.) = 1);;
assert(a (0., 6.) = 1);;
assert(a (0., 7.) = 0);;
assert(a (0., -1.) = 2);;
assert(a (0., -2.) = 0);;
assert(a (2., 3.) = 7);;
assert(a (1., 3.) = 5);;
assert(a (0., 3.) = 3);;
assert(a (-1., 3.) = 3);;
assert(a (-2., 3.) = 1);;
assert(a (-3., 3.) = 0);;
assert(a (1., 5.) = 5);;
assert(a (2., 5.) = 6);;
assert(a (3., 5.) = 3);;
assert(a (4., 5.) = 0);;
assert(a (3., 6.) = 6);;
assert(a (3., 4.) = 0);;
assert(a (3., 7.) = 6);;
assert(a (3., 8.) = 3);;
assert(a (3., 9.) = 1);;
assert(a (3., 10.) = 1);;
assert(a (3., 10.1) = 0);;
*)