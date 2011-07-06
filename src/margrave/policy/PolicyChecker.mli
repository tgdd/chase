(** For checking policies for consistency with respect to a signature
    and for internal consistency. For example, for checking the formulas for
    well-sortedness.
    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

(** Indicates that a check has failed. *)
exception Policy_check_failure of string

(** Ensures a policy is well-formed w.r.t. a signature. Raises
    Policy_check_failure if there is an error. *)
val check_policy : SortedFol.Syntax.sig_t -> PolicySyntax.policy -> unit
