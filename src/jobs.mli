val job_tree_to_job_ids : ('a, 'b) State.job_tree -> string list
val failed : 'a State.job_result -> bool

val rebuildable_jobs :
  ('a -> 'b State.job_result -> 'b State.job_result) ->
  ('b, 'a, 'c) State.stage list ->
  string list
