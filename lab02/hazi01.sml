(* Írjuk meg a mylength függvényt *)
fun mylength(ls : 'a list) = if null(ls) then 0
                        else 1 + mylength(tl(ls));
mylength [1,2];
List.length [1,2];

(* IP = inner product *)
fun ip ([], []) = 0 
| ip ((x::xs), (y::ys)) = x*y + ip (xs,ys);
ip ([1,2,3], [1,2,3]);

(* Írjuk meg iteratívan ip-t *)
fun ip(ls1, ls2) =
    if null(ls1) orelse null(ls2) then 0
    else if mylength(ls1) <> mylength(ls2) then raise Fail ("Not equal list size(s).")
    else hd(ls1)*hd(ls2) + ip(tl(ls1), tl(ls2));
ip([1, 2, 3], [1, 2, 3]);

List.take([1,2,3,4,5],3);
List.drop([1,2,3,4,5],3);

(*Írjuk meg a saját mytake és mydrop függvényeket. Írjuk meg iteratívan*)
fun mytake(ls, k) =
  if null(ls) orelse k <= 0 then []
  else if k > 0 then
    hd(ls) :: mytake(tl(ls), k-1)
  else [];
mytake([1,2,3,4,5], 3);

fun mydrop(ls, k) =
  if null(ls) then []
  else let
    val temp_ls = ref ls and i = ref k
  in
    while (!i) > 0 do (
      temp_ls := tl(!temp_ls);
      i := (!i) - 1
  );
  !temp_ls
  end;
mydrop([1, 2, 3, 4, 5], 3);

(*Keressük meg egy lista maximumát. Keressük meg a k-adik maximumát*)

fun selectMax(x, y) =
  if x >= y then x
  else y;

fun max(ls) =
  if null(ls) then ~1
  else let
    val max_val = ref ~99999 and ls_ref = ref ls
  in
    while not(null(!ls_ref)) do (
      max_val := selectMax(!max_val, hd(!ls_ref));
      ls_ref := tl(!ls_ref)
  );
  !max_val
  end;
max([10, 2, 3, 4]);


  