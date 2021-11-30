type 'output job_result =
  ( 'output,
    [ `Active of [ `Running | `Ready ]
    | `Msg of string
    | `Cancelled
    | `Blocked
    | `Skipped_failure
    | `Skipped of string ] )
  result

let to_int : _ job_result -> int = function
  | Error (`Skipped _) -> 0
  | Error `Skipped_failure -> 0
  | Ok _ -> 1
  | Error `Blocked -> 2
  | Error (`Active `Ready) -> 3
  | Error (`Active `Running) -> 4
  | Error `Cancelled -> 5
  | Error (`Msg _) -> 6

type 'output job = {
  result : 'output job_result;
  metadata : Current.Metadata.t option;
}

type ('output, 'node_metadata) job_tree_node =
  | Item of 'output job
  | Group of ('output, 'node_metadata) job_tree list

and ('output, 'node_metadata) job_tree = {
  node : ('output, 'node_metadata) job_tree_node;
  metadata : 'node_metadata;
}

let job_tree_group metadata jobs = { metadata; node = Group jobs }

let job_tree_item node_metadata ?metadata result =
  { metadata = node_metadata; node = Item { result; metadata } }

type ('output, 'node_metadata, 'stage_metadata) stage = {
  jobs : ('output, 'node_metadata) job_tree list;
  metadata : 'stage_metadata;
}

let stage metadata jobs = { metadata; jobs }

type ('output, 'node_metadata, 'stage_metadata, 'pipeline_metadata) pipeline = {
  stages : ('output, 'node_metadata, 'stage_metadata) stage list;
  metadata : 'pipeline_metadata;
}

let pipeline metadata stages = { metadata; stages }

let status_of_list l =
  List.fold_left
    (fun v new_v -> if to_int new_v >= to_int v then new_v else v)
    (Error (`Skipped "no task to do"))
    l

let rec job_tree_status job_tree =
  match job_tree.node with
  | Item { result; _ } -> result
  | Group nodes -> nodes |> List.rev_map job_tree_status |> status_of_list

let stage_status stage =
  stage.jobs |> List.rev_map job_tree_status |> status_of_list

let pipeline_status pipeline =
  pipeline.stages |> List.rev_map stage_status |> status_of_list

let node_metadata { metadata; node = _ } = metadata

let stage_metadata { metadata; jobs = _ } = metadata

let map fn_node fn_stage fn_pipeline { stages; metadata } =
  let rec map_node { node; metadata } =
    match node with
    | Group nodes ->
        let nodes = List.map map_node nodes in
        {
          node = Group nodes;
          metadata = fn_node (`Children (List.map node_metadata nodes)) metadata;
        }
    | Item item ->
        { node = Item item; metadata = fn_node (`Leaf item) metadata }
  in
  let map_stage { jobs; metadata } =
    let jobs = List.map map_node jobs in
    { jobs; metadata = fn_stage (List.map node_metadata jobs) metadata }
  in
  let stages = List.map map_stage stages in
  { stages; metadata = fn_pipeline (List.map stage_metadata stages) metadata }

module Marshalling = struct
  let error_to_json = function
    | `Skipped msg ->
        `Assoc [ ("type", `String "skipped"); ("msg", `String msg) ]
    | `Skipped_failure -> `Assoc [ ("type", `String "skipped-failure") ]
    | `Blocked -> `Assoc [ ("type", `String "blocked") ]
    | `Active `Ready -> `Assoc [ ("type", `String "active-ready") ]
    | `Active `Running -> `Assoc [ ("type", `String "active-running") ]
    | `Cancelled -> `Assoc [ ("type", `String "cancelled") ]
    | `Msg msg -> `Assoc [ ("type", `String "msg"); ("msg", `String msg) ]

  let error_of_json json =
    let open Yojson.Safe.Util in
    match member "type" json |> to_string with
    | "skipped" -> `Skipped (member "msg" json |> to_string)
    | "msg" -> `Msg (member "msg" json |> to_string)
    | "skipped-failure" -> `Skipped_failure
    | "blocked" -> `Blocked
    | "active-ready" -> `Active `Ready
    | "active-running" -> `Active `Running
    | "cancelled" -> `Cancelled
    | _ -> raise (Type_error ("type", json))

  let job_result_to_json marshal_output = function
    | Ok v ->
        `Assoc [ ("type", `String "ok"); ("value", `String (marshal_output v)) ]
    | Error e -> `Assoc [ ("type", `String "err"); ("value", error_to_json e) ]

  let job_result_of_json unmarshal_output json =
    let open Yojson.Safe.Util in
    match member "type" json |> to_string with
    | "ok" -> Ok (unmarshal_output (member "value" json |> to_string))
    | "err" -> Error (error_of_json (member "value" json))
    | _ -> raise (Type_error ("type", json))

  let option fn x = Option.map fn x |> Option.value ~default:`Null

  let current_metadata_to_json { Current.Metadata.job_id; update } =
    `Assoc
      [
        ("job_id", option (fun x -> `String x) job_id);
        ( "update",
          option
            (function
              | `Ready -> `String "ready" | `Running -> `String "running")
            update );
      ]

  let current_metadata_of_json json =
    let open Yojson.Safe.Util in
    let job_id = member "job_id" json |> to_string_option in
    let update =
      member "update" json |> to_string_option
      |> Option.map (function
           | "ready" -> `Ready
           | "running" -> `Running
           | _ -> raise (Type_error ("update.parse", json)))
    in
    { Current.Metadata.job_id; update }

  let rec job_tree_node_to_json marshal_output marshal_node = function
    | Item { result; metadata } ->
        `Assoc
          [
            ("metadata", option current_metadata_to_json metadata);
            ("result", job_result_to_json marshal_output result);
          ]
    | Group g ->
        `List (List.map (job_tree_to_json marshal_output marshal_node) g)

  and job_tree_to_json marshal_output (marshal_node : _ -> string)
      { node; metadata } : Yojson.Safe.t =
    `Assoc
      [
        ("metadata", `String (marshal_node metadata));
        ("node", job_tree_node_to_json marshal_output marshal_node node);
      ]

  let rec job_tree_node_of_json unmarshal_output unmarshal_node =
    let open Yojson.Safe.Util in
    function
    | `Assoc _ as v ->
        let metadata =
          member "metadata" v |> to_option current_metadata_of_json
        in
        let result = member "result" v |> job_result_of_json unmarshal_output in
        Item { metadata; result }
    | `List lst ->
        Group (List.map (job_tree_of_json unmarshal_output unmarshal_node) lst)
    | v -> raise (Type_error ("uh", v))

  and job_tree_of_json unmarshal_output unmarshal_node json =
    let open Yojson.Safe.Util in
    let metadata = member "metadata" json |> to_string |> unmarshal_node in
    let node =
      member "node" json
      |> job_tree_node_of_json unmarshal_output unmarshal_node
    in
    { metadata; node }

  let stage_to_json marshal_output marshal_node (marshal_stage : _ -> string)
      { metadata; jobs } : Yojson.Safe.t =
    `Assoc
      [
        ("metadata", `String (marshal_stage metadata));
        ( "jobs",
          `List (List.map (job_tree_to_json marshal_output marshal_node) jobs)
        );
      ]

  let stage_of_json unmarshal_output unmarshal_node unmarshal_stage json =
    let open Yojson.Safe.Util in
    let metadata = member "metadata" json |> to_string |> unmarshal_stage in
    let jobs =
      member "jobs" json |> to_list
      |> List.map (job_tree_of_json unmarshal_output unmarshal_node)
    in
    { metadata; jobs }

  let pipeline_to_json marshal_output marshal_node marshal_stage
      (marshal_pipeline : _ -> string) { metadata; stages } : Yojson.Safe.t =
    `Assoc
      [
        ("metadata", `String (marshal_pipeline metadata));
        ( "stages",
          `List
            (List.map
               (stage_to_json marshal_output marshal_node marshal_stage)
               stages) );
      ]

  let pipeline_of_json unmarshal_output unmarshal_node unmarshal_stage
      unmarshal_pipeline json =
    let open Yojson.Safe.Util in
    let metadata = member "metadata" json |> to_string |> unmarshal_pipeline in
    let stages =
      member "stages" json |> to_list
      |> List.map
           (stage_of_json unmarshal_output unmarshal_node unmarshal_stage)
    in
    { metadata; stages }
end

let marshal marshal_output marshal_node marshal_stage marshal_pipeline pipeline
    =
  Marshalling.pipeline_to_json marshal_output marshal_node marshal_stage
    marshal_pipeline pipeline
  |> Yojson.Safe.to_string

let unmarshal unmarshal_output unmarshal_node unmarshal_stage unmarshal_pipeline
    str =
  Yojson.Safe.from_string str
  |> Marshalling.pipeline_of_json unmarshal_output unmarshal_node
       unmarshal_stage unmarshal_pipeline
