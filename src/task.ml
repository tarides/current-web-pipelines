type ('current, 'state) t = {
  current : 'current Current.t;
  state : 'state Current.t;
}
(** A pipeline with current 'current and metadata 'state. *)

type 'state simple = (unit, 'state) t

let current t = t.current
let state t = t.state
let v ~current ~state = { current; state }

let list_iter (type a) ~collapse_key
    (module S : Current_term.S.ORDERED with type t = a) fn values =
  let fn_take_current v =
    let v = fn v in
    v.current
  in
  let fn_take_status v =
    let v = fn v in
    v.state
  in

  let current =
    Current.list_iter ~collapse_key (module S) fn_take_current values
  in
  let state = Current.list_map ~collapse_key (module S) fn_take_status values in
  { current; state }

let all tasks =
  let current = List.map (fun t -> t.current) tasks |> Current.all in
  let state = List.map (fun t -> t.state) tasks |> Current.list_seq in
  { current; state }

let list_seq tasks =
  let current = List.map (fun t -> t.current) tasks |> Current.list_seq in
  let state = List.map (fun t -> t.state) tasks |> Current.list_seq in
  { current; state }

let map_current fn t = { t with current = Current.map fn t.current }
let map_state fn t = { t with state = Current.map fn t.state }

let status_of_state_and_metadata state metadata =
  match (state, metadata) with
  | Ok v, _ -> Ok v
  | (Error (`Skipped _ | `Skipped_failure) as e), _ -> e
  | Error _, Some { Current.Metadata.job_id = None; _ } -> Error `Blocked
  | Error _, None -> Error `Blocked
  | (Error (`Active _) as e), _ -> e
  | Error (`Msg "Cancelled"), _ -> Error `Cancelled
  | (Error (`Msg _) as e), _ -> e

let single_c node_metadata current =
  let open Current.Syntax in
  let state =
    let+ state = Current.state ~hidden:true current
    and+ metadata = Current.Analysis.metadata current
    and+ node_metadata = node_metadata in
    let job_result = status_of_state_and_metadata state metadata in
    State.job_tree_item node_metadata ?metadata job_result
  in
  { current; state }

let single node_metadata current =
  let open Current.Syntax in
  let state =
    let+ state = Current.state ~hidden:false current
    and+ metadata = Current.Analysis.metadata current in
    let job_result = status_of_state_and_metadata state metadata in
    State.job_tree_item node_metadata ?metadata job_result
  in
  { current; state }
