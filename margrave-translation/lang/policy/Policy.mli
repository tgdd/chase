(** Packaged up types and function definitions for policies. *)

(* Syntax *)

(** Policy free variable declaration. *)
type variable = { variable_name : string
                ; variable_type : string
                }

(** Policy rule override declartion. *)
type rule_override = { low_rule : string
                     ; high_rules : string list
                     }

(** Policy rule combination declaration. *)
type rule_comb = { rule_fas : string list list
                 ; rule_overrides : rule_override list
                 }

(** Policy rule definition. *)
type rule = { rule_name : string
            ; rule_decision : string
            ; rule_parameters : string list
            ; rule_body : SortedFol.formula
            }
              
(** Policy definition. *)
type policy = { uses : string
              ; target : SortedFol.formula
              ; vars : variable list
              ; rules : rule list
              ; rule_combs : rule_comb
              }

(** Reads a policy from the given channel. *)
val read_policy : in_channel -> policy

(** Creates a string representation of a policy. *)
val show_policy : policy -> string

(** Writes the string representation of the policy to the specified channel. *)
val output_policy : out_channel -> policy -> unit

(** Prints the string representation of the policy to standard out. *)
val print_policy : policy -> unit
