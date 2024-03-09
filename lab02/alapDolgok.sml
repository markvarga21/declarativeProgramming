print("List initialization:");
val ls = [1,2,3];
val head:int = hd(ls);
val tail : 'a list = tl(ls);
print("Functions:");
fun plus (x:int, y:int) = x + y;
plus(2,2);
fun len(ls : 'a list) = if null(ls) then 0
             else 1 + len(tl(ls));
val ls2 = 1::[2,3];
len [1,2,3];
len([1,2,3]);
len ls2;
print("List operations:");
val temp = [1] @ [2,3];
1 :: 2 :: [3,4,5];
rev(temp);
fun first(x,y) = y;
fun first(x,_) = x;
first(1,2);


