type 'output job_result =
  ( 'output,
    [ `Active of [ `Running | `Ready ]
    | `Msg of string
    | `Cancelled
    | `Blocked
    | `Skipped_failure
    | `Skipped of string ] )
  result

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

val job_tree_item :
  'node_metadata ->
  ?metadata:Current.Metadata.t ->
  'output job_result ->
  ('output, 'node_metadata) job_tree

val job_tree_group :
  'node_metadata ->
  ('output, 'node_metadata) job_tree list ->
  ('output, 'node_metadata) job_tree

type ('output, 'node_metadata, 'stage_metadata) stage = {
  jobs : ('output, 'node_metadata) job_tree list;
  metadata : 'stage_metadata;
}

type ('output, 'node_metadata, 'stage_metadata, 'pipeline_metadata) pipeline = {
  stages : ('output, 'node_metadata, 'stage_metadata) stage list;
  metadata : 'pipeline_metadata;
}

val job_tree_status : ('output, _) job_tree -> 'output job_result

val stage_status : ('output, _, _) stage -> 'output job_result

val pipeline_status : ('output, _, _, _) pipeline -> 'output job_result

val map :
  ([ `Children of 'new_node_metadata list | `Leaf of 'output job ] ->
  'node_metadata ->
  'new_node_metadata) ->
  ('new_node_metadata list -> 'stage_metadata -> 'new_stage_metadata) ->
  ('new_stage_metadata list -> 'pipeline_metadata -> 'new_pipeline_metadata) ->
  ('output, 'node_metadata, 'stage_metadata, 'pipeline_metadata) pipeline ->
  ( 'output,
    'new_node_metadata,
    'new_stage_metadata,
    'new_pipeline_metadata )
  pipeline
