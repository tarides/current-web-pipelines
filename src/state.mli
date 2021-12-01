(**
This module contains a generic layered state that can be rendered using the Web module.
*)

type 'output job_result =
  ( 'output,
    [ `Active of [ `Running | `Ready ]
    | `Msg of string
    | `Cancelled
    | `Blocked
    | `Skipped_failure
    | `Skipped of string ] )
  result
(** Job data *)

type 'output job = {
  result : 'output job_result;
  metadata : Current.Metadata.t option;
}
(** Job data and metadata *)

type ('output, 'node_metadata) job_tree_node =
  | Item of 'output job
  | Group of ('output, 'node_metadata) job_tree list

and ('output, 'node_metadata) job_tree = {
  node : ('output, 'node_metadata) job_tree_node;
  metadata : 'node_metadata;
}
(** A tree of jobs *)

val job_tree_item :
  'node_metadata ->
  ?metadata:Current.Metadata.t ->
  'output job_result ->
  ('output, 'node_metadata) job_tree
(** Create a single leaf job *)

val job_tree_group :
  'node_metadata ->
  ('output, 'node_metadata) job_tree list ->
  ('output, 'node_metadata) job_tree
(** Group jobs in a node *)

type ('output, 'node_metadata, 'stage_metadata) stage = {
  jobs : ('output, 'node_metadata) job_tree list;
  metadata : 'stage_metadata;
}
(** A stage is a group of trees *)

val stage :
  'stage_metadata ->
  ('output, 'node_metadata) job_tree list ->
  ('output, 'node_metadata, 'stage_metadata) stage

type ('output, 'node_metadata, 'stage_metadata, 'pipeline_metadata) pipeline = {
  stages : ('output, 'node_metadata, 'stage_metadata) stage list;
  metadata : 'pipeline_metadata;
}
(** A pipeline is a sequence of stages *)

val pipeline :
  'pipeline_metadata ->
  ('output, 'node_metadata, 'stage_metadata) stage list ->
  ('output, 'node_metadata, 'stage_metadata, 'pipeline_metadata) pipeline

val job_tree_status :
  node_map_status:('node_metadata -> 'output job_result -> 'output job_result) ->
  ('output, 'node_metadata) job_tree ->
  'output job_result
(** Aggregated tree status *)

val stage_status :
  node_map_status:('node_metadata -> 'output job_result -> 'output job_result) ->
  ('output, 'node_metadata, _) stage ->
  'output job_result
(** Aggregated stage status *)

val pipeline_status :
  node_map_status:('node_metadata -> 'output job_result -> 'output job_result) ->
  ('output, 'node_metadata, _, _) pipeline ->
  'output job_result
(** Aggregated pipeline status  *)

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
(** [map fn_leaf fn_node fn_stage fn_pipeline pipeline] transforms the metadata at every stage. Each function takes the new metadata of the lower stages and the current metadata and should return the new metadata. *)

val marshal :
  ('output -> string) ->
  ('new_node_metadata -> string) ->
  ('new_stage_metadata -> string) ->
  ('new_pipeline_metadata -> string) ->
  ( 'output,
    'new_node_metadata,
    'new_stage_metadata,
    'new_pipeline_metadata )
  pipeline ->
  string

val unmarshal :
  (string -> 'output) ->
  (string -> 'new_node_metadata) ->
  (string -> 'new_stage_metadata) ->
  (string -> 'new_pipeline_metadata) ->
  string ->
  ( 'output,
    'new_node_metadata,
    'new_stage_metadata,
    'new_pipeline_metadata )
  pipeline
