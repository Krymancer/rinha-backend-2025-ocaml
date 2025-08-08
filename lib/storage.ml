(* Bit-packed payment storage for memory efficiency *)

type storage_entry = {
  amount : int;
  requested_at : int64;
} [@@deriving show]

module BitPackingStorage = struct
  type t = {
    mutable start_timestamp : int64;
    mutable data : int array;
    mutable length : int; (* number of entries *)
    mutable capacity : int; (* size of [data] array in ints *)
  }

  let create () =
    {
      start_timestamp = Int64.of_float (Unix.time () *. 1000.0);
      data = Array.make 1000 0;
      length = 0;
      capacity = 1000;
    }

  let reset storage =
    storage.start_timestamp <- Int64.of_float (Unix.time () *. 1000.0);
    storage.length <- 0

  let resize storage =
    let new_capacity = storage.capacity * 2 in
    let new_data = Array.make new_capacity 0 in
    (* copy existing pairs: 2 ints per entry *)
    Array.blit storage.data 0 new_data 0 (storage.length * 2);
    storage.data <- new_data;
    storage.capacity <- new_capacity

  let push storage amount current_timestamp =
    let delta = Int64.sub current_timestamp storage.start_timestamp in
    let delta_int = Int64.to_int delta in
    
    if delta_int < 0 || delta_int > 86_400_000 then
      failwith "Timestamp fora da janela permitida (0-86400000 ms)";
    
    if amount < 0 then
      failwith "Amount deve ser positivo";
    
    (* Ensure capacity for two ints per entry *)
    let idx = storage.length * 2 in
    if idx + 1 >= storage.capacity then (
      resize storage
    );
    
    storage.data.(idx) <- delta_int;
    storage.data.(idx + 1) <- amount;
    storage.length <- storage.length + 1

  let list storage =
    let result = ref [] in
    for i = storage.length - 1 downto 0 do
      let idx = i * 2 in
      let delta = storage.data.(idx) in
      let amount = storage.data.(idx + 1) in
      let requested_at = Int64.add storage.start_timestamp (Int64.of_int delta) in
      result := { amount; requested_at } :: !result
    done;
    !result

  (* Iterate over entries within [from_ts, to_ts] bounds without allocating *)
  let fold_in_range storage ~(from_ts:int64 option) ~(to_ts:int64 option) ~init ~f =
    let acc = ref init in
    let within ts =
      let ge_from = match from_ts with None -> true | Some a -> Int64.compare ts a >= 0 in
      let le_to = match to_ts with None -> true | Some b -> Int64.compare ts b <= 0 in
      ge_from && le_to
    in
    for i = 0 to storage.length - 1 do
      let idx = i * 2 in
      let delta = storage.data.(idx) in
      let amount = storage.data.(idx + 1) in
      let ts = Int64.add storage.start_timestamp (Int64.of_int delta) in
      if within ts then
        acc := f !acc amount ts
    done;
    !acc
end

type state = {
  default : BitPackingStorage.t;
  fallback : BitPackingStorage.t;
}

let create_state () = {
  default = BitPackingStorage.create ();
  fallback = BitPackingStorage.create ();
}
