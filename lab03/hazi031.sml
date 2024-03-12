(* Keressük meg a k-adik maximumát *)


(* Prefixum, suffixum, sublist *)
fun sum([]) = 0
  | sum([a]) = a
  | sum(a::x) = a + sum(x);
sum [1,2];
fun prefixum([]) = []
  | prefixum(a::b::x) = sum(a) :: prefixum(b::x);

(* Keressük meg egy lista maximális hosszúságú, monoton növekvő részlistáját. *)
fun subl([]) = []
  | subl(a::b::x) =
      if a <= b then a :: subl(b::x)
      else [a];
subl [1, 2, 3, 4, 1, 0];

(* Keressük meg egy lista azon részlistáit, ahol az elemek összege négyzetszám. *)
fun isSquare(numb) =
    if numb <= 0 then false
    else
      let 
        val a = Math.sqrt(Real.fromInt(numb))
      in
        a-Real.fromInt(floor(a)) < 0.00000001
      end;

(*fun sublSq([]) = []
  | sublSq(a::b::x) =
      if isSquare(sum([a]::b)) then a :: sublSq(b::x)
      else [];
sublSq [4, 4, 1, 2];*) 

(*keressük meg egy lista azon részlistáit, ahol az elemek összege egy adott S érték*)
fun sublSum([], _) = []
  | sublSum(a::b::x, s) =
      if sum(a::b) = s then a :: sublSum(b::x, s)
      else sublSum(b::x, s);
sublSum([1, 2, 3, 4, 1, 0], 4);

(*keressük meg egy lista azon elemeit, melyek összege egy adott S érték*)
fun sum([]) = 0
  | sum([a]) = a
  | sum(a::x) = a + sum(x);
sum [1, 2, 3, 4];


(*fun sublSum([],_) = []
  | sublSum(a::b::x, s) =
      if sum(a::b) = s then a :: sublSum(b::x, s)
      else sublSum(b::x, s);
sublSum([1, 2, 3, 4, 1, 0], 4);*)


(*adjuk meg egy szám valódi osztói listáját (nem egy és nem önmaga)*)
fun genSub(0) = []
  | genSub(x) = 
    if x-1 > 1 then x-1 :: genSub(x-1)
    else [];
fun contains([],_) = false
  | contains(x::xs, elem) =
    if x = elem then true
    else contains(xs, elem);
fun divisible(x,y) = x mod y = 0;
fun getDiv([],_) = []
  | getDiv(x::xs, num) =
      if divisible(num, x) then x :: getDiv(xs, num)
      else getDiv(xs, num);
val a = 10;
getDiv(genSub(a), a);    