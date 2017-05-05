open Regexp
open Dfa
open Test

module Part1 : DFA = struct

  type 'a state = 'a regexp option

  let init reg = Some(reg)

  let final reg_opt = 
    let rec aux = function 
      | Eps -> true
      | Sym(l) -> false
      | Alt(r1,r2) -> (aux r1) || (aux r2)
      | Seq(r1,r2) -> (aux r1) && (aux r2)
      | Rep(r1) -> true
    in
    match reg_opt with
    | None -> false
    | Some(reg) -> aux reg
                       
  let rec next reg_opt l =
    match reg_opt with
    | None -> None
    | Some(reg) -> match reg with
                   | Eps -> None
                   | Sym(lp) -> if lp == l then Some(Eps) else None
                   | Alt(r1,r2) -> let r1_opt = next (Some(r1)) l in
                                   let r2_opt = next (Some(r2)) l in
                                   begin
                                     match r1_opt, r2_opt with
                                     | None, None -> None
                                     | None, _ -> r2_opt
                                     | (Some(r12)), None -> r1_opt
                                     | (Some(r12)), (Some(r22)) -> Some(Alt(r12,r22))
                                   end
                   | Seq(r1,r2) -> let r1_opt = next (Some(r1)) l in
                                   if final (Some(r1)) then
                                     begin
                                       match r1_opt, (next (Some(r2)) l) with
                                       | None, r2_opt -> r2_opt
                                       | (Some(r12)), None -> Some(Seq(r12,r2))
                                       | (Some(r12)), (Some(r22)) -> Some(Alt(r22,Seq(r12,r2))) 
                                     end
                                   else
                                     begin
                                       match r1_opt with
                                       | None -> None 
                                       | Some(Eps) -> Some(r2)
                                       | Some(r12) -> Some(Seq(r12,r2))
                                     end
                   | Rep(r1) -> let r1_opt = (next (Some r1) l) in
                                match r1_opt with
                                | None -> None
                                | Some(r12) -> Some(Seq(r12, Rep(r1)))
            

end


(* UTILISATION DU MODULE ET DES FONCTEURS *)
module M = Acceptor.Make(Part1)

let accept e u = M.accept e u




(* LANCEMENT DES TESTS -- NE PAS MODIFIER SOUS CETTE LIGNE *)

let () = test1 accept
