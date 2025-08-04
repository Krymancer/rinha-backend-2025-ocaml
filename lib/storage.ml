(* Bit-packed payment storage for memory efficiency *)

type storage_entry = {
  amount : int;
  requested_at : int64;
} [@@deriving show]

module BitPackingStorage = struct
  type t = {
    mutable start_timestamp : int64;
    mutable data : int array;
    mutable length : int;
    mutable capacity : int;
  }

  let create () =
    {
      start_timestamp = Int64.of_float (Sys.time () *. 1000.0);
      data = Array.make 1000 0;
      length = 0;
      capacity = 1000;
    }

  let reset storage =
    storage.start_timestamp <- Int64.of_float (Sys.time () *. 1000.0);
    storage.length <- 0

  let resize storage =
    let new_capacity = storage.capacity * 2 in
    let new_data = Array.make new_capacity 0 in
    Array.blit storage.data 0 new_data 0 storage.length;
    storage.data <- new_data;
    storage.capacity <- new_capacity

  let push storage amount current_timestamp =
    let delta = Int64.sub current_timestamp storage.start_timestamp in
    let delta_int = Int64.to_int delta in
    
    if delta_int < 0 || delta_int > 86_400_000 then
      failwith "Timestamp fora da janela permitida (0-86400000 ms)";
    
    if amount < 0 || amount > 4_095 then
      failwith "Amount fora do intervalo suportado (0-4095)";
    
    if storage.length >= storage.capacity then
      resize storage;
    
    let packed = (delta_int lsl 12) lor amount in
    storage.data.(storage.length) <- packed;
    storage.length <- storage.length + 1

  let list storage =
    let result = ref [] in
    for i = storage.length - 1 downto 0 do
      let entry = storage.data.(i) in
      let delta = entry lsr 12 in
      let amount = entry land 0xfff in
      let requested_at = Int64.add storage.start_timestamp (Int64.of_int delta) in
      result := { amount; requested_at } :: !result
    done;
    !result
end

type state = {
  default : BitPackingStorage.t;
  fallback : BitPackingStorage.t;
}

let create_state () = {
  default = BitPackingStorage.create ();
  fallback = BitPackingStorage.create ();
}
