(* JSON types for the API *)

type queue_message = {
  amount : float;
  correlation_id : string;
}

type payment_data = {
  requested_at : string;
  amount : float;
  correlation_id : string;
}

type payment_processor = 
  | Default
  | Fallback

type payment_data_response = {
  requested_at : string;
  amount : float;
  correlation_id : string;
  payment_processor : payment_processor;
}

type payment_summary = {
  total_requests : int;
  total_amount : float;
}

type payment_summary_response = {
  default : payment_summary;
  fallback : payment_summary;
}

type payment_summary_query = {
  from_time : string option;
  to_time : string option;
  local_only : bool;
}
