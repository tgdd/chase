(** Compose operator. See Haskell's . *)
let ( <.> ) f g = fun x -> f (g x)

(** Application operator. Used to change the precedence of function application.
    See Haskell's $ *)
let ( & ) a b = a b
