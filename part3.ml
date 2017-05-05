open Wregexp
open Regexp
open Test
open List

(* PONDERATEUR CONSTRUIT EN SE BASANT SUR LA DEFINITION INDUCTIVE *)

module WeighterSpec : WEIGHTER = functor (W:SEMIRING) -> struct

    let rec eval wreg w =
      match wreg with
      |Eps -> begin match w with 
              | [] -> W.un
              | _ -> W.zero
              end
      |Sym f ->  begin match w with 
                 | [l] -> f l
                 | _ -> W.zero
                 end
      |Alt(wr1,wr2) -> W.sum [eval wr1 w; eval wr2 w]
      |Seq(wr1,wr2) -> 
        let rec aux w1 = function
          | [] -> [W.prod [eval wr1 w1; eval wr2 []]]
          | h :: t -> (W.prod [eval wr1 w1; eval wr2 (h::t)]) :: (aux (w1@[h]) t)
        in
        W.sum (aux [] w)
      |Rep(wr1) ->      
                   let rec aux w1 = function
                     | [] -> eval wr1 w
                     | h :: t -> W.sum [aux (w1@[h]) t; W.prod [eval wr1 (w1@[h]); eval (Rep(wr1)) (t)]]
                   in
                   match w with
                   | [] -> W.un
                   | _ -> aux [] w

end



(* PONDERATEUR CONSTRUIT EN GENERALISANT LA PARTIE 2 *)

module Weighter : WEIGHTER = functor (W:SEMIRING) -> struct



end
*)

(* TESTS *)


module BoolRing : SEMIRING  with type t = bool = struct
  type t = bool
  let un = true
  let zero = false
  let sum l = (List.fold_left (||) false l)
  let prod l = List.fold_left (&&) true l
end

module M = WeighterSpec (BoolRing)

let accept = M.eval

let () = test3 accept
