fun power (x, k) =
if k=1 then x
else if k mod 2 = 0 then power(x*x, k div 2)
else x * power(x*x, k div 2);
power(2,3);

fun nextfib (prev, curr: int) = (curr, prev + curr);
nextfib(0,1);
nextfib it;
nextfib it;
nextfib it;
nextfib it;


fun suml ([]) = 0
  | suml (x::xs) = x + suml xs;
suml [];

fun ip ([], []) = 0 
| ip ((x::xs), (y::ys)) = x*y + ip (xs,ys);
ip ([1,2,3], [1,2,3]);

List.take([1,2,3,4,5],3);
List.drop([1,2,3,4,5],3);
