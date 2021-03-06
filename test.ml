open Regexp

(* TESTS PARTIE 1 *)

let string_of_regexp e = 
  let rec f = function
    | Eps -> "."
    | Sym x -> String.make 1 x
    | Alt (e1,e2) -> Format.sprintf "(%s+%s)" (f e1) (f e2)
    | Seq (e1,e2) -> Format.sprintf "%s%s" (f e1) (f e2)
    | Rep e -> Format.sprintf "(%s)*" (f e) 
  in f e

let string_of_word w = 
  let rec f = function
    | [] -> ""
    | a::w -> (String.make 1 a) ^ (f w)
  in f w

let tests1 = [
  Eps , [] , true;
  Eps , ['a'] , false;
  Sym 'a' , ['a'] , true;
  Sym 'a' , ['b'] , false;
  Sym 'a' , ['a';'b'] , false;
  Sym 'a' , [] , false;
  Alt (Sym 'a',Sym 'b') , ['a'] , true;
  Alt (Sym 'a',Sym 'b') , ['b'] , true;
  Alt (Sym 'a',Sym 'b') , ['c'] , false;
  Seq (Sym 'a',Sym 'b') , ['a';'b'] , true;
  Seq (Sym 'a',Sym 'b') , ['a'] , false;
  Seq (Sym 'a',Sym 'b') , ['b'] , false;
  Seq (Sym 'a',Sym 'b') , [] , false;
  Seq (Sym 'a',Eps) , ['a'] , true;
  Seq (Eps,Sym 'a') , ['a'] , true;
  Seq (Eps,Eps) , [] , true;
  Rep (Sym 'a') , [] , true;
  Rep (Sym 'a') , ['a'] , true;
  Rep (Sym 'a') , ['a';'a'] , true;
  Rep (Sym 'a') , ['a';'a';'a'] , true;
  Rep (Sym 'a') , ['a';'b';'a'] , false;
  Rep (Seq(Sym 'a', Sym 'a')), ['a';'a';'a';'a';'a';'a'] , true;
  Rep (Seq(Sym 'a', Sym 'a')), ['a';'a';'a';'a';'a';'a';'a'] , false;
  Seq(Rep( Alt(Sym 'a', Sym 'b')), Rep(Sym 'c')), ['a';'a';'b';'c';'c';'c';'c'], true;
  Seq(Rep( Alt(Sym 'a', Sym 'b')), Rep(Sym 'c')), ['a';'a';'b';'c';'c';'c'; 'b';'c'], false;
  Alt(Rep(Sym 'a'),Rep( Sym 'b')), [], true;
  Alt(Rep(Sym 'a'),Rep( Sym 'b')), ['a';'a';'a';'a'], true;
  Alt(Rep(Sym 'a'),Rep( Sym 'b')), ['b';'b';'b'], true;
  Alt(Rep(Sym 'a'),Rep( Sym 'b')), ['b';'b';'a';'b'], false;
  Alt(Rep(Sym 'a'),Rep( Sym 'b')), ['b';'a';'b';'b'], false;
  Alt(Rep(Sym 'a'),Rep( Sym 'b')), ['a';'b';'b';'b'], false;
  Seq(Seq(Seq(Sym 'a', Sym 'b'), Sym 'c'),Eps), ['a'; 'b'; 'c'], true;
  Rep(Seq(Seq(Seq(Sym 'a', Sym 'b'),Sym 'c'),Eps)), ['a'; 'b'; 'c'; 'a'; 'b'; 'c' ], true;
  Seq(Seq(Seq(Sym 'a', Sym 'b'), Sym 'c'),Eps), ['a'; 'b'; 'c'; 'a'], false;  
  Rep(Seq(Sym 'a',Seq(Sym 'b',Seq(Sym 'c',Eps)))), ['a';'b';'c'], true;
  Rep(Seq(Sym 'a',Seq(Sym 'b',Seq(Sym 'c',Eps)))), ['a';'b';'c';'a';'b';'c'], true;
  Rep(Seq(Sym 'a',Seq(Sym 'b',Seq(Sym 'c',Eps)))), ['a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a'], false

]

      
let treat accept (e,u,b) = 
  Format.printf "%s@.%s@."
    (string_of_regexp e) 
    (string_of_word u);
  let t1 = Printf.sprintf "%.20f" (Sys.time ()) in
  let _ = accept e u in
  let t2 = Printf.sprintf "%.20f" (Sys.time ()) in
  Format.printf "%s@.%s@." t1 t2


let test1 accept = 
  List.iter (treat accept) tests1



(* TESTS PARTIE 2 *)

let tests2 = [
 Eps , [] , true;
  Eps , ['a'] , false;
  Sym 'a' , ['a'] , true;
  Sym 'a' , ['b'] , false;
  Sym 'a' , ['a';'b'] , false;
  Sym 'a' , [] , false;
  Alt (Sym 'a',Sym 'b') , ['a'] , true;
  Alt (Sym 'a',Sym 'b') , ['b'] , true;
  Alt (Sym 'a',Sym 'b') , ['c'] , false;
  Seq (Sym 'a',Sym 'b') , ['a';'b'] , true;
  Seq (Sym 'a',Sym 'b') , ['a'] , false;
  Seq (Sym 'a',Sym 'b') , ['b'] , false;
  Seq (Sym 'a',Sym 'b') , [] , false;
  Seq (Sym 'a',Eps) , ['a'] , true;
  Seq (Eps,Sym 'a') , ['a'] , true;
  Seq (Eps,Eps) , [] , true;
  Rep (Sym 'a') , [] , true;
  Rep (Sym 'a') , ['a'] , true;
  Rep (Sym 'a') , ['a';'a'] , true;
  Rep (Sym 'a') , ['a';'a';'a'] , true;
  Rep (Sym 'a') , ['a';'b';'a'] , false;
  Rep (Seq(Sym 'a', Sym 'a')), ['a';'a';'a';'a';'a';'a'] , true;
  Rep (Seq(Sym 'a', Sym 'a')), ['a';'a';'a';'a';'a';'a';'a'] , false;
  Seq(Rep( Alt(Sym 'a', Sym 'b')), Rep(Sym 'c')), ['a';'a';'b';'c';'c';'c';'c'], true;
  Seq(Rep( Alt(Sym 'a', Sym 'b')), Rep(Sym 'c')), ['a';'a';'b';'c';'c';'c'; 'b';'c'], false;
  Alt(Rep(Sym 'a'),Rep( Sym 'b')), [], true;
  Alt(Rep(Sym 'a'),Rep( Sym 'b')), ['a';'a';'a';'a'], true;
  Alt(Rep(Sym 'a'),Rep( Sym 'b')), ['b';'b';'b'], true;
  Alt(Rep(Sym 'a'),Rep( Sym 'b')), ['b';'b';'a';'b'], false;
  Alt(Rep(Sym 'a'),Rep( Sym 'b')), ['b';'a';'b';'b'], false;
  Alt(Rep(Sym 'a'),Rep( Sym 'b')), ['a';'b';'b';'b'], false;
  Seq(Seq(Seq(Sym 'a', Sym 'b'), Sym 'c'),Eps), ['a'; 'b'; 'c'], true;
  Rep(Seq(Seq(Seq(Sym 'a', Sym 'b'),Sym 'c'),Eps)), ['a'; 'b'; 'c'; 'a'; 'b'; 'c' ], true;
  Seq(Seq(Seq(Sym 'a', Sym 'b'), Sym 'c'),Eps), ['a'; 'b'; 'c'; 'a'], false;  
  Rep(Seq(Sym 'a',Seq(Sym 'b',Seq(Sym 'c',Eps)))), ['a';'b';'c'], true;
  Rep(Seq(Sym 'a',Seq(Sym 'b',Seq(Sym 'c',Eps)))), ['a';'b';'c';'a';'b';'c'], true;
  Rep(Seq(Sym 'a',Seq(Sym 'b',Seq(Sym 'c',Eps)))), ['a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a';'b';'c';'a'], false

  ]

let treat2  accept (e,u,b) = 
  Format.printf "%s@.%s@."
    (string_of_regexp e) 
    (string_of_word u);
  let t1 = Printf.sprintf "%.20f" (Sys.time ()) in
  let _ = accept e u in
  let t2 = Printf.sprintf "%.20f" (Sys.time ()) in
  Format.printf "%s@.%s@." t1 t2


let test2 accept = 
  List.iter (treat2 accept) tests2



(* TESTS PARTIE 3 *)

let fa = function
  |'a' -> true
  | _ -> false

let fb = function
  | 'b' -> true
  | _ -> false

let fc = function
  | 'c' -> true
  | _ -> false

let falp alp = function
  | a when a == alp -> true
  | _ -> false 

let tests3 = [
 Eps , [] , true;
  Eps , ['a'] , false;
  Sym fa , ['a'] , true;
  Sym fa , ['b'] , false;
  Sym fa , ['a';'b'] , false;
  Sym fa , [] , false;
  Alt (Sym fa,Sym fb) , ['a'] , true;
  Alt (Sym fa,Sym fb) , ['b'] , true;
  Alt (Sym fa,Sym fb) , ['c'] , false;
  Seq (Sym fa,Sym fb) , ['a';'b'] , true;
  Seq (Sym fa,Sym fb) , ['a'] , false;
  Seq (Sym fa,Sym fb) , ['b'] , false;
  Seq (Sym fa,Sym fb) , [] , false;
  Seq (Sym fa,Eps) , ['a'] , true;
  Seq (Eps,Sym fa) , ['a'] , true;
  Seq (Eps,Eps) , [] , true;
  Rep (Sym fa) , [] , true;
  Rep (Sym fa) , ['a'] , true;
  Rep (Sym fa) , ['a';'a'] , true;
  Rep (Sym fa) , ['a';'a';'a'] , true;
  Rep (Sym fa) , ['a';'b';'a'] , false;
  Rep (Seq(Sym fa, Sym fa)), ['a';'a';'a';'a';'a';'a'] , true;
  Rep (Seq(Sym fa, Sym fa)), ['a';'a';'a';'a';'a';'a';'a'] , false;
  Seq(Rep( Alt(Sym fa, Sym fb)), Rep(Sym fc)), ['a';'a';'b';'c';'c';'c';'c'], true;
  Seq(Rep( Alt(Sym fa, Sym fb)), Rep(Sym fc)), ['a';'a';'b';'c';'c';'c'; 'b';'c'], false;
  Alt(Rep(Sym fa),Rep( Sym fb)), [], true;
  Alt(Rep(Sym fa),Rep( Sym fb)), ['a';'a';'a';'a'], true;
  Alt(Rep(Sym fa),Rep( Sym fb)), ['b';'b';'b'], true;
  Alt(Rep(Sym fa),Rep( Sym fb)), ['b';'b';'a';'b'], false;
  Alt(Rep(Sym fa),Rep( Sym fb)), ['b';'a';'b';'b'], false;
  Alt(Rep(Sym fa),Rep( Sym fb)), ['a';'b';'b';'b'], false;
  Seq(Seq(Seq(Sym fa, Sym fb), Sym fc),Eps), ['a'; 'b'; 'c'], true;
  Rep(Seq(Seq(Seq(Sym fa, Sym fb),Sym fc),Eps)), ['a'; 'b'; 'c'; 'a'; 'b'; 'c' ], true;
  Seq(Seq(Seq(Sym fa, Sym fb), Sym fc),Eps), ['a'; 'b'; 'c'; 'a'], false;  
  Rep(Seq(Sym fa,Seq(Sym fb,Seq(Sym fc,Eps)))), ['a';'b';'c'], true;
  Rep(Seq(Sym fa,Seq(Sym fb,Seq(Sym fc,Eps)))), ['a';'b';'c';'a';'b';'c'], true;
  Rep(Seq(Sym fa,Seq(Sym fb,Seq(Sym fc,Eps)))), ['a';'b';'c';'a'], false

  ]

let string_of_wregexp bwe = 
  let rec f = function
    | Eps -> "."
    | Sym g -> if g 'a' then String.make 1 'a' else( if g 'b' then String.make 1 'b' else String.make 1 'c' )
    | Alt (e1,e2) -> Format.sprintf "(%s+%s)" (f e1) (f e2)
    | Seq (e1,e2) -> Format.sprintf "%s%s" (f e1) (f e2)
    | Rep e -> Format.sprintf "(%s)*" (f e) 
  in f bwe

let treat2 accept (e,u,b) = 
  Format.printf "%s@.%s@."
    (string_of_wregexp e) 
    (string_of_word u);
  Format.printf "%s@.@." (if accept e u=b then "OK" else "FAILED")


let test3 accept =
  List.iter (treat2 accept) tests3
