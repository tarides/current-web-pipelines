(*
This module contains utilities for manipulating job_trees
*)
val job_tree_to_job_ids :
  ('output, 'node_metadata) State.job_tree -> string list
(** Given a job_tree, extract all the job_ids of the jobs in the tree into a list*)

val failed : 'output State.job_result -> bool
(** encode what job results correspond to failures and what do not*)

val rebuildable_jobs :
  node_map_status:
    ('node_metadata -> 'output State.job_result -> 'output State.job_result) ->
  ('output, 'node_metadata, _) State.stage list ->
  string list
(** Given a list of stages, extract the job_ids of rebuildable jobs (i.e. jobs that have failed)*)
