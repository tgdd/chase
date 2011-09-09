(** Entry point for the Margrave to FOL compiler. The main function handles
    parsing out the arguments and passes them to run which does the actual work.
    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

open IOUtil

(** The entry point for compiling policies to FOL theories with signature
    information. The input file contains the policy.
    
    The general idea is to
    1. Parse the policy file.
    2. Use the policy to determine the vocabulary file to use.
       (The vocabulary file is relative to the location of the policy file.)
    3. Parse the vocabulary file.
    4. Compile the vocabulary file to metadata, including
       - a list of sorts 
       - a list of sub-sort rules
       - a list of function names
       - a list of free variable names
       - a function from relation names to types
       - a function from function names to types
       - a function from free variables to types
       - a function to determine if two types intersect
    5. Do a well-sortedness check on the formulas in the policy.
    6. Compile the policy into a sorted FOL theory and augmented signature.
    7. Return the augmented signature and theory. *)
let run :  string -> SortedFol.Signature.sig_t * SortedFol.Syntax.theory =
  fun policyfile ->
    (* Read the policy *)
    let policy = call_with_in_channel policyfile Policy.read_policy in
    (* Get the vocab file location *)
    let uses = policy.Policy.Syntax.uses in
    (* Determine the actual location--concat the dir of the policy if
       necessary. *)
    let vocabfile = if not (Filename.is_relative uses) then uses else 
        Filename.concat (Filename.dirname policyfile) uses in
    (* Read the vocab *)
    let vocab = call_with_in_channel vocabfile Vocab.read_vocab in
    (* Convert the vocab to a sorted FOL signature *)
    let sgn = Vocab.Compiler.vocab2signature vocab in
    (* Compile the policy into an augmented signature and a theory. *)
    Policy.Compiler.compile_policy sgn policy

(*    snd (Policy.Compiler.policy2theory sgn policy) *)

(** Utility for handling maybe. *)
let maybe : 'a option -> 'a -> 'a =
  fun opt def -> match opt with
    | None -> def
    | Some x -> x

(** Determines the name of the output file based on the name of the input
    file. *)
let determine_outfile : string -> string =
  fun infile ->
    if StringUtil.ends_with infile ".po" then
        infile ^ ".po"
    else try
      let dotpos = String.rindex infile '.' in
      let prefix = String.sub infile 0 dotpos in
      prefix ^ ".po"
    with Not_found -> infile ^ ".po"

(* Refs for parameter passing *)
let infile = ref None
let outfile = ref None

let usage_msg = "Usage: mc policy <options>\nOptions are:"

(** Sets the output file or raises an exception if it has already been set. *)
let set_outfile : string -> unit =
 fun name -> match !outfile with
   | None -> outfile := Some name
   | Some _ -> 
     raise (Arg.Bad "Expected at most one output file, but received multiple")

(** Sets the input file or raises an exception if it has already been set. *)
let set_infile : string -> unit =
  fun name -> match !infile with
    | None -> infile := Some name
    | Some _ ->
      raise (Arg.Bad "Expected one policy file, but recieved multiple")

(* For use with the Arg module's parameter parsing. *)
let speclist = [("-o", Arg.String set_outfile, "<output> the outputfile")]

(** For handling error messages in parameter passing after the Arg module
    has done it's job *)
let parse_arguments_error msg =
  begin
    print_endline ((Array.get Sys.argv 0) ^ ": " ^ msg);
    Arg.usage speclist usage_msg;
    exit 0
  end

(** Parses the arguments and returns (input file, output file) or prints usage
    and terminates the program if the arguments couldn't be parsed. *)
let parse_arguments : unit -> (string * string) =
  fun () -> begin
    Arg.parse speclist set_infile usage_msg;
    match !infile with
    | None -> parse_arguments_error "policy file required"
    | Some(infile) -> 
      if not (Sys.file_exists infile) then
        raise (Failure "input file does not exist") else
      let out = maybe !outfile (determine_outfile infile) in
      (infile, out)
  end

let main () =
  let (infile, outfile) = parse_arguments() in
  let sgn, thy = run infile in
  ( call_with_out_channel
      outfile
      ( fun oc ->
          ( output_string oc (SortedFol.Signature.Signature.show sgn)
          ; output_string oc "\n\n"
          ; SortedFol.Syntax.output_latex_formulas oc thy
          )
      )
  )

let _ = Printexc.print main ()
