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

type ('output, 'node_metadata, 'stage_metadata) stage = {
  jobs : ('output, 'node_metadata) job_tree list;
  metadata : 'stage_metadata;
}

type ('output, 'node_metadata, 'stage_metadata, 'pipeline_metadata) pipeline = {
  stages : ('output, 'node_metadata, 'stage_metadata) stage list;
  metadata : 'pipeline_metadata;
}

let job_tree_group metadata jobs = { metadata; node = Group jobs }

let job_tree_item node_metadata ?metadata result =
  { metadata = node_metadata; node = Item { result; metadata } }

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
