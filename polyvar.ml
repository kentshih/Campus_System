(* An illustration of how OCaml's "polymorphic variants" feature provides a
   simple solution to the expression problem. *)

(* Integer addition language. *)
module Core = struct

  (* Smart constructors for building tagged values. *)
  let lit i   = `Lit i
  let neg e   = `Neg e
  let add l r = `Add (l, r)

  (* Some example expressions. *)
  let ex1 = add (lit 2) (lit 3)
  let ex2 = add (lit 4) ex1
  let ex3 = neg ex2
  let bad = `Foo ex3
  
  (* Non-extensible evaluation.
  let rec eval = function
    | `Lit i      -> i
    | `Neg e      -> - eval e
    | `Add (l, r) -> eval l + eval r
  (*  | `Bar e      -> 3 *)
  *)
  
  (* Extensible evaluation function. *)
  let ext_eval f = function
    | `Lit i      -> i
    | `Neg e      -> - f e
    | `Add (l, r) -> f l + f r

  (* Tie the knot. *)
  let rec eval e = ext_eval eval e
  
end

(* Add pretty printing. *)
module Pretty = struct

  (* This imports Core and adds all of its bindings to Pretty. *)
  include Core

  (* Extensible pretty printing. *)
  let ext_pretty f = function
    | `Lit i      -> string_of_int i
    | `Neg e      -> "-" ^ f e
    | `Add (l, r) -> "(" ^ f l ^ " + " ^ f r ^ ")"

  (* Tie the knot. *)
  let rec pretty e = ext_pretty pretty e

end


(* Add multiplication. *)
module Mult = struct

  (* Smart constructors for building tagged values. *)
  let lit = Core.lit
  let neg = Core.neg
  let add = Core.add
  let mul l r = `Mul (l, r)

  (* Example expressions. *)
  let ex4 = mul Core.ex1 Core.ex2
  let ex5 = add ex4 Core.ex3

  (* Non-extensible evaluation (doesn't work).
  let rec eval = function
    | `Mul (l, r) -> eval l * eval r
    | (`Lit _ | `Neg _ | `Add _) as e -> Core.eval e
  *)
  
  (* Extensible evaluation. *)
  let ext_eval f = function
    | `Mul (l, r) -> f l * f r
    | (`Lit _ | `Neg _ | `Add _) as e -> Core.ext_eval f e

  (* Tie the knot. *)
  let rec eval e = ext_eval eval e

  (* Pretty printing. *)
  let ext_pretty f = function
    | `Mul (l, r) -> "(" ^ f l ^ " * " ^ f r ^ ")"
    | (`Lit _ | `Neg _ | `Add _) as e -> Pretty.ext_pretty f e

  let rec pretty e = ext_pretty pretty e

end
