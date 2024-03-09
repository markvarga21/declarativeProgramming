fun lengthvec (x,y) = Math.sqrt(x*x+y*y);
fun negvec (x,y) : real*real = (~x,~y);
fun addvec((x1,y1), (x2,y2)) : real*real = (x1+x2, y1+y2);
fun subvec(v1,v2) = addvec(v1, negvec v2);
fun distance (v1, v2) = lengthvec (subvec (v1,v2));

distance((1.0,1.0), (0.0,0.0));
val henryV = {
  name = "Henry V",
  born = 1387,
  died = 1422,
  crowned = 1413
};

val richardIII = {
  name = "Richard III",
  born = 1452,
  died = 1485,
  crowned = 1483
};

type king = {
  name: string,
  born: int,
  died: int,
  crowned: int
};

fun lifetime (k: king) = #died k - #born k;
fun lifetime' ({born,died, ...}: king) = died - born;

lifetime henryV;
lifetime' richardIII;

fun fact (n: int) = if n = 0 then 1
                    else n * fact(n-1);
fact 3;

(* ez itt egy megjegyzes *)

fun facti (n,p) = if n=0 then p else facti(n-1, n*p);
facti(10, 1);

(* GCD = greatest common diviser *)
fun gcd (m,n) = if m=0 then n else gcd (n mod m, m);
gcd(12, 18);

fun add(n, m) = if m=0 then n else add(add(n,m-1), 1);
add(1,2);

fun power (x, k) : real =
if k=1 then x
else if k mod 2 = 0 then power(x*x, k div 2)
else x* power (x*x, k div 2);





