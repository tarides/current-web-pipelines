let job_tree_to_job_ids (job_tree : _ State.job_tree) =
  let rec loop job_tree_list (jt : _ State.job_tree) =
    match jt.node with
    | State.Item
        {
          metadata = Some { Current.Metadata.job_id = Some job_id; _ };
          result = _;
        } ->
        List.append [ Some job_id ] job_tree_list
    | State.Item _r -> List.append [ None ] job_tree_list
    | State.Group nodes -> List.concat @@ List.map (loop job_tree_list) nodes
  in
  let some_job_ids = loop [] job_tree in
  List.fold_left
    (fun acc sx -> match sx with None -> acc | Some x -> x :: acc)
    [] some_job_ids

let failed = function
  | Ok _ | Error (`Active `Ready) | Error (`Active `Running) -> false
  | Error `Blocked | Error (`Skipped _) | Error `Skipped_failure -> false
  | Error `Cancelled | Error (`Msg _) -> true

let rebuildable_jobs node_map_status stages =
  let failing_stages =
    List.filter
      (fun stage -> failed @@ State.stage_status ~node_map_status stage)
      stages
  in
  let job_trees_of_failing_stages =
    List.concat
    @@ List.map (fun (stage : _ State.stage) -> stage.jobs) failing_stages
  in
  List.concat @@ List.map job_tree_to_job_ids job_trees_of_failing_stages
