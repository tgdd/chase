
(** Opens an input channel for the filename specified and calls the function
    with that channel. Closes the channel after the function returns. *)
let call_with_in_channel : string -> (in_channel -> 'a) -> 'a =
  fun name func ->
		  let ic = open_in name in
		  let res =
		    try func ic
		    with exn -> close_in ic; raise exn in
		  close_in ic;
		  res

(** Opens an output channel for the filename specified and calls the function
    with that channel. Closes the channel after the function returns. *)
let call_with_out_channel : string -> (out_channel -> 'a) -> 'a =
  fun name func ->
		  let oc = open_out name in
		  let res =
		    try func oc
		    with exn -> close_out oc; raise exn in
		  close_out oc;
		  res