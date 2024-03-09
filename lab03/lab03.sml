List.hd [1,2,3];
List.tl [1,2,3];

fun myhd (x::_) = x; 
fun mytl (_::tl) = tl;

infix 6 ++;

fun ([] ++ xs) = xs
  | (x::xs) ++ ys = x :: (xs ++ ys);

fun revl [] = []
  | revl (x::xs) = (revl xs) ++ [x];

revl [1,2,3];

(*Írjuk meg a listamegfordítás iteratív verzóját.*)

fun ip ([], []) = 0
  | ip ((x::xs), (y::ys)) = x*y + ip (xs, ys);

ip ([1, 2, 3], [~1, 2, 3]);

fun itip (n, [], []) = n
  | itip (n, (x::xs), (y::ys))= itip (n+x*y, xs, ys);

itip (0, [1,2,3], [~1, 2, 3]);

(*Keressük meg azon (x,y,z) pythagoraszi számhármasokat, melyekre 1 <= x,y,z <= 100.
x^2+y^2=z^2 kell, hogy teljesüljön.*)
(* listaelemek összege, szorzata, iteratívan is*)
(*erase5 törölje az 5 első előfordulását a listából*)

(*erase5 [1,5,3,5,7] = [1,3,5,7]*)

(*erase subsequent repeating elements
repeat [1,1,2,2,2,3] = [1,2,3]*)

(*find the largest monotone increasing sublist of a list
moninc [1,5,9,3,4,5,6] = [3,4,5,6]*)

(*delete every second occurrence of 5
delsec5 [1,5,3,5,7] = [1,5,3,7]*)

(*find the maximum of a list, find the k th maximum*)

fun erase5 [] = []
  |erase5 (5::xs) = xs
  |erase5 (x::xs) = x :: erase5 xs;

erase5 [1,5,3,5,7];

fun erase' l = let fun erase (n, l::ls) = 
  if n=l then ls
  else l :: (erase (n, ls))
in
  erase (5, l)
end;

erase' [1,5,3,5,7];

fun maxl [m] = m
  | maxl (m::n::ms) = if m <= n then maxl (n::ms)
else maxl (m::ms);

maxl [5, 10, 12, 3, 7];

fun maxl' [m] = m
  |maxl' (m::ms) = if maxl' ms <= m then m
else maxl' ms;

maxl' [1,5,12,10,8];
