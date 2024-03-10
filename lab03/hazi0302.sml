(*adjuk meg a prímtényezős felbontását egy számnak*)
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
fun genSub'(0) = []
  | genSub'(x) = x :: genSub'(x-1);
fun isPrime(num) = 
  if num < 2 then false
  else null(getDiv(genSub(num), num));
fun genPrimeTo(_,[]) = []
  | genPrimeTo(num, x::xs) = 
    if isPrime(x) then x :: genPrimeTo(num, xs)
    else genPrimeTo(num, xs);
genPrimeTo(75, genSub'(75));
fun primeDiv(_,[]) = []
  | primeDiv(num, x::xs) =
      if divisible(num, x) then x :: primeDiv(num div x, x::xs)
      else primeDiv(num, xs);
fun rev [] = []
  | rev (h::tl) = rev tl @ [h];
val primePre = genPrimeTo(126, genSub'(126)); 
val revPrimePre = rev primePre;
primeDiv(126, revPrimePre);

(*számoljuk ki lkkt (n,m)-et, ahol lkkt a legkisebb közös többszörös*)
(*számoljuk ki log2 n egészrészt*)
(*számoljuk ki rekurzívan power (2, n)*)
(*oldjuk meg a másodfokú egyenletet*)
fun solve(a, b, c) =
    let
      val delta = Real.fromInt(b*b-(4*a*c))
    in
      [(Real.fromInt(~b) + Math.sqrt(delta))/(Real.fromInt(2*a)), (Real.fromInt(~b) - Math.sqrt(delta))/(Real.fromInt(2*a))]
    end;
solve(2, ~12, 10);

(*definiáljunk egy relprime relációt, mely igaz akkor és csak akkor, ha n és m relatív prímek*)
fun removeDuplicates([]) = []
  | removeDuplicates([x]) = [x]
  | removeDuplicates(x::y::z) =
      if x = y then removeDuplicates(y::z)
      else x :: removeDuplicates(y::z);
removeDuplicates [1, 1, 2, 2, 3, 3, 3, 4];
fun genSub_w1(0) = []
  | genSub_w1(x) = 
    if x-1 >= 0 then x :: genSub_w1(x-1)
    else [];
fun at(_, [], _) = ~1
  | at(index, x::xs, init) =
    if init = index then x
    else at(index, xs, init+1);
fun len([]) = 0
  | len(x::xs) = 1 + len(xs);

fun relPrime(a, b) =
let
   val div1 = getDiv(genSub(a), a) and
       div2 = getDiv(genSub(b), b) and
       i = ref 0 and j = ref 0 and
       isRelativePrime = ref true
in
  while (!i) < len(div1) do (
    while (!j) < len(div2) do (
      if at(!i, div1, 0) = at(!j, div2, 0) then isRelativePrime := false
      else (
        print(Int.toString(at(!i, div1, 0)) ^ " " ^ Int.toString(at(!j, div2, 0)) ^ "\n")
      );
      j := (!j) + 1
    );
    i := (!i) + 1
  );
  !isRelativePrime
end;
relPrime(10, 16);

(*Adjuk meg 1-től 1000-ig azokat a számokat, melyek nem oszthatók sem 2-vel, sem 3-mal, sem 5-tel*)
fun genSub'(0) = []
  | genSub'(x) = 
    if x-1 > 1
      andalso not(divisible(x-1, 2))
      andalso not(divisible(x-1, 3))
      andalso not(divisible(x-1, 5))
    then x-1 :: genSub'(x-1)
    else genSub'(x-1);
genSub' 100;
  
  
(*Adjuk meg 1-től 1000-ig azokat a számokat, melyek oszthatók vagy 2-vel, vagy 3-mal, vagy 5-tel, mindegyik számot egyszer soroljunk fel*)
fun genSub''(0) = []
  | genSub''(x) = 
    if x-1 > 1
      orelse not(divisible(x-1, 2))
      orelse not(divisible(x-1, 3))
      orelse not(divisible(x-1, 5))
    then x-1 :: genSub''(x-1)
    else genSub''(x-1);
genSub'' 100;

(*Állítsuk elő a 100-at 2, 3, 5 vagy 7 értékek összegeként.*)
val values = [7, 5, 3, 2];
fun make100(_, []) = []
  | make100(init, x::xs) =
      if init + x <= 100 then x :: make100(init+x, x::xs)
      else make100(init, xs);
make100(0, values);
  
(*Állítsuk elő a 100-at legfeljebb 5 szám összegeként.*)