open Regexp
open Dfa
open Test

module Part2 : DFA = struct

  type 'a state =  bool * ('a * bool) regexp

  let rec cont_eps (reg : 'a regexp) =
    match reg with
    | Eps -> true
    | Sym l -> false
    | Alt(r1,r2) -> (cont_eps r1) || (cont_eps r2)
    | Seq(r1,r2) -> (cont_eps r1) && (cont_eps r2)
    | Rep(r1) -> true

  let init reg =
    let rec aux_init = function
      | Eps -> Eps
      | Sym a -> Sym (a,false)
      | Alt(r1,r2) -> Alt(aux_init r1, aux_init r2)
      | Seq(r1,r2) -> Seq(aux_init r1,aux_init r2)
      | Rep(r1) -> Rep(aux_init r1)
    in
    (true,aux_init reg)

  let final reg_st = 
    let rec aux = function
      | Eps -> false
      | Sym (l,b) -> b
      | Alt(r1,r2) -> (aux r1) || (aux r2)
      | Seq(r1,r2) -> (aux r2 ) || (cont_eps r2 && aux r1)
      | Rep(r1) -> (aux r1)
    in
    ((fst reg_st) && (cont_eps (snd reg_st))) || (aux (snd reg_st))

  let next reg_st l =
    let rec aux_next b = function
      | Eps -> (Eps,b)
      | Sym (lp,bp) ->
         if b && lp == l then
           (Sym (lp, true), bp)
         else 
           (Sym (lp, false), bp)
      | Alt(r1,r2) -> let (r12,b1) = aux_next b r1 in
                      let (r22,b2) = aux_next b r2 in
                      (Alt(r12,r22), b1 || b2)
      | Seq(r1,r2) -> let (r12,b1) = aux_next b r1 in
                      let (r22,b2) = aux_next b1 r2 in
                      (Seq(r12,r22),b2)
      | Rep(r1) -> let (r12,b1) = aux_next (final (b,Rep(r1))) r1 in
                   (Rep(r12),b1)
    in         
    let (r,b) = aux_next (fst reg_st) (snd reg_st) in 
    (false, r)

end


(* UTILISATION DU MODULE ET DES FONCTEURS *)

module M2 = Acceptor.Make (Part2)

let accept2 e u = M2.accept e u




(* LANCEMENT DES TESTS -- NE PAS MODIFIER SOUS CETTE LIGNE *)

let () = test2 accept2
