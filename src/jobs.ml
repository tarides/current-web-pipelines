let job_tree_to_job_ids job_tree =
  let rec loop job_tree_list jt =
    match jt.State.node with
    | State.Item
        { metadata = Some { Current.Metadata.job_id = Some job_id; _ }; _ } ->
        job_id :: job_tree_list
    | State.Item _ -> job_tree_list
    | State.Group nodes -> List.concat_map (loop job_tree_list) nodes
  in
  loop [] job_tree

let failed = function
  | Ok _
  | Error (`Active `Ready)
  | Error (`Active `Waiting_for_confirmation)
  | Error (`Active `Running) ->
      false
  | Error `Blocked | Error (`Skipped _) | Error `Skipped_failure -> false
  | Error `Cancelled | Error (`Msg _) -> true

let rebuildable_jobs ~node_map_status stages =
  let failing_stages =
    List.filter
      (fun stage -> failed @@ State.stage_status ~node_map_status stage)
      stages
  in
  let job_trees_of_failing_stages =
    List.concat_map (fun stage -> stage.State.jobs) failing_stages
  in
  List.concat_map job_tree_to_job_ids job_trees_of_failing_stages
