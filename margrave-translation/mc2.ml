(** Entry point for the Margrave to FOL compiler. The main function handles
    parsing out the arguments and passes them to run which does the actual work.
    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

open IOUtil
open SortedFol

(** The entry point for compiling policies to FOL theories with signature
    information. The input file contains the policy and the output file
    is where the signature and FOL theory should be written.
    
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
    6. Compile the policy into a sorted FOL theory.
    7. Create models for the theory.
    8. Output the models to the file. *)
let run : string -> string -> unit =
  fun policyfile outfile ->
    let policy = call_with_in_channel policyfile Policy.read_policy in
    let uses = policy.Policy.uses in
    let vocabfile = if not (Filename.is_relative uses) then uses else 
        Filename.concat (Filename.dirname policyfile) uses in
    let vocab = call_with_in_channel vocabfile Vocab.read_vocab in
    Vocab.print_vocab vocab

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
  run infile outfile

let _ = Printexc.print main ()
