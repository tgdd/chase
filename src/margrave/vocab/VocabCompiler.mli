(** Type and functions for compiling a vocab into useful metadata.

    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

(** Converts a vocab to a Sorted FOL siganture. *)
val vocab2signature : VocabSyntax.vocab -> SortedFol.Syntax.sig_t
