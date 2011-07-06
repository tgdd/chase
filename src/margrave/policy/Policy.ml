(** Packaged up types and function definitions for policies.
    
    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

module Syntax = PolicySyntax
module Checker = PolicyChecker
module Compiler = PolicyCompiler

(******************************************************************************)
(* READING AND WRITING POLICY HELPERS *)
(** Creates a string representation of a policy. *)
let show_policy = Syntax.show_policy

(** Reads a policy from the given input channel. *)
let read_policy ic =
  let lexbuf = Lexing.from_channel ic in
  PolicyParser.start PolicyLexer.token lexbuf

(** Ouputs a policy to the given output channel. *)
let output_policy oc p =
  output_string oc (show_policy p);
  output_string oc "\n";
  flush oc

(** Prints a policy to standard out. *)
let print_policy p =
  output_policy stdout p
(* END READING AND WRITING POLICY HELPERS *)
(******************************************************************************)
