type 'a inline =
  ([> Html_types.core_phrasing_without_interactive ] as 'a) Tyxml_html.elt

type 'a block = ([> Html_types.div_content ] as 'a) Tyxml_html.elt

module type Renderer = sig
  val extra_routes : Current_web.Resource.t Routes.route list

  module Output : sig
    type t

    val render_inline : t -> [> Html_types.div_content ] Tyxml_html.elt
  end

  module Node : sig
    type t

    val render_inline : t -> _ inline

    val map_status : t -> 'a State.job_result -> 'a State.job_result
  end

  module Stage : sig
    type t

    val id : t -> string

    val render_inline : t -> _ inline

    val render : t -> _ block
  end

  module Pipeline : sig
    type t

    val id : t -> string

    val render_inline : t -> _ inline

    val render : t -> _ block
  end

  val render_index : unit -> _ block
end

module StringMap = Map.Make (String)

module Make (R : Renderer) = struct
  type pipeline_state =
    (R.Output.t, R.Node.t, R.Stage.t, R.Pipeline.t) State.pipeline

  type pipeline_state_internal =
    ( R.Output.t,
      R.Node.t * Run_time.t,
      R.Stage.t * Run_time.t,
      R.Pipeline.t * Run_time.t )
    State.pipeline

  type t = pipeline_state_internal StringMap.t ref

  let make () = ref StringMap.empty

  let update_state (state : t) (new_state : pipeline_state Current.t) =
    let open Current.Syntax in
    let+ new_state = new_state in
    let new_state =
      State.map Run_time.map_node Run_time.map_stage Run_time.map_stage
        new_state
    in
    state :=
      StringMap.add
        (new_state.metadata |> fst |> R.Pipeline.id)
        new_state !state
  (* RENDERING *)

  let emoji_of_status =
    let open Tyxml_html in
    function
    | Ok _ -> span ~a:[ a_title "OK" ] [ txt "✔️ " ]
    | Error (`Active `Ready) -> span ~a:[ a_title "Ready" ] [ txt "🟡 " ]
    | Error (`Active `Running) -> span ~a:[ a_title "Running" ] [ txt "🟠 " ]
    | Error `Blocked -> span ~a:[ a_title "Blocked" ] [ txt "🔘 " ]
    | Error `Cancelled -> span ~a:[ a_title "Cancelled" ] [ txt "🛑 " ]
    | Error (`Msg msg) -> span ~a:[ a_title ("Error: " ^ msg) ] [ txt "❌ " ]
    | Error (`Skipped msg) ->
        span ~a:[ a_title ("Skipped: " ^ msg) ] [ txt "⚪ " ]
    | Error `Skipped_failure ->
        span ~a:[ a_title "Skipped failure" ] [ txt "⏹ " ]

  (* LIST PIPELINES *)
  let list_pipelines ~(state : t) =
    let open Tyxml_html in
    let show_pipeline (pipeline : pipeline_state_internal) =
      let metadata, run_time = pipeline.metadata in
      let id = R.Pipeline.id metadata in
      [
        h2
          [
            emoji_of_status (State.pipeline_status pipeline);
            a
              ~a:[ a_href ("/pipelines/" ^ id) ]
              [ R.Pipeline.render_inline metadata ];
            i [ Run_time.to_elem run_time ];
          ];
      ]
    in
    [
      div [ R.render_index () ];
      h2 [ txt "Pipelines" ];
      ul
        (List.map
           (fun (_, pipeline) -> li (show_pipeline pipeline))
           (StringMap.bindings !state));
    ]

  (* SHOW PIPELINES *)

  let show_pipeline ~(state : t) pipeline_id =
    let pipeline = StringMap.find pipeline_id !state in
    let metadata, _ = pipeline.metadata in
    let open Tyxml_html in
    [
      h1 [ txt "Pipeline "; R.Pipeline.render_inline metadata ];
      R.Pipeline.render metadata;
      h2 [ txt "Stages:" ];
      ul
        (List.map
           (fun (stage : _ State.stage) ->
             let metadata, run_time = stage.metadata in
             li
               [
                 emoji_of_status (State.stage_status stage);
                 a
                   ~a:
                     [
                       a_href
                         ("/pipelines/" ^ pipeline_id ^ "/"
                        ^ R.Stage.id metadata);
                     ]
                   [ R.Stage.render_inline metadata ];
                 i [ Run_time.to_elem run_time ];
               ])
           pipeline.stages);
    ]

  (* JOB TREE *)

  let render_result v = Result.map R.Output.render_inline v |> Result.to_list

  let rec get_job_tree ~uri_base (job_tree : _ State.job_tree) =
    let status = State.job_tree_status job_tree in
    let emoji = emoji_of_status status in
    let metadata, run_time = job_tree.metadata in
    let description = R.Node.render_inline metadata in
    let open Tyxml_html in
    match job_tree.node with
    | State.Group nodes ->
        let children_nodes = List.map (get_job_tree ~uri_base) nodes in
        [
          emoji;
          description;
          i [ Run_time.to_elem run_time ];
          ul (List.map li children_nodes);
        ]
    | Item
        { metadata = Some { Current.Metadata.job_id = Some job_id; _ }; result }
      ->
        [
          emoji;
          a ~a:[ a_href (uri_base ^ "/" ^ job_id) ] [ description ];
          txt " ";
          i [ Run_time.to_elem run_time ];
          txt " ";
        ]
        @ render_result result
    | Item { result; _ } ->
        [
          emoji;
          span [ description ];
          txt " ";
          i [ Run_time.to_elem run_time ];
          txt " ";
        ]
        @ render_result result

  (* SHOW STAGE *)

  let show_pipeline_task ~(state : t) pipeline_id stage_id =
    let pipeline = StringMap.find pipeline_id !state in
    let stage =
      List.find
        (fun (t : _ State.stage) -> R.Stage.id (fst t.metadata) = stage_id)
        pipeline.stages
    in
    let job_trees =
      List.map
        (get_job_tree ~uri_base:("/pipelines/" ^ pipeline_id ^ "/" ^ stage_id))
        stage.jobs
    in
    let open Tyxml_html in
    [
      h1
        [
          emoji_of_status (State.pipeline_status pipeline);
          a
            ~a:[ a_href ("/pipelines/" ^ pipeline_id) ]
            [ R.Pipeline.render_inline (fst pipeline.metadata) ];
        ];
      h2
        [
          emoji_of_status (State.stage_status stage);
          R.Stage.render_inline (fst stage.metadata);
        ];
      h3 [ txt "Job tree" ];
      ul (List.map li job_trees);
    ]

  (* SHOW STAGE + LOGS *)

  let get_job_text job_id =
    let path = Current.Job.log_path job_id |> Result.get_ok in
    let max_log_chunk_size = 102400L in
    (* ocurrent/lib_web/job.ml *)
    let ch = open_in_bin (Fpath.to_string path) in
    Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
    let len = LargeFile.in_channel_length ch in
    LargeFile.seek_in ch 0L;
    let truncated = if max_log_chunk_size < len then "\n(truncated)" else "" in
    let len = min max_log_chunk_size len in
    really_input_string ch (Int64.to_int len) ^ truncated

  let show_pipeline_task_job ~(state : t) pipeline_id stage_id wildcard =
    let job_id =
      let wld = Routes.Parts.wildcard_match wildcard in
      String.sub wld 1 (String.length wld - 1)
    in
    let pipeline_tasks = show_pipeline_task ~state pipeline_id stage_id in
    let open Tyxml_html in
    [
      div
        ~a:[ a_style "display: flex;" ]
        [
          div ~a:[ a_style "width: 50%" ] pipeline_tasks;
          div ~a:[ a_style "width: 50%" ]
            [
              h2 [ txt "Job log" ];
              a
                ~a:[ a_href ("/job/" ^ job_id) ]
                [ txt "See full log and operations" ];
              pre [ txt (get_job_text job_id) ];
            ];
        ];
    ]

  (* ROUTING *)
  let internal_routes ~state =
    Routes.
      [
        empty @--> list_pipelines ~state;
        (str /? nil) @--> show_pipeline ~state;
        (str / str /? nil) @--> show_pipeline_task ~state;
        (str / str /? wildcard) @--> show_pipeline_task_job ~state;
      ]

  let handle state wildcard_path =
    object
      inherit Current_web.Resource.t

      method! nav_link = Some "Pipelines"

      method! private get context =
        let target = Routes.Parts.wildcard_match wildcard_path in
        let response =
          Routes.one_of (internal_routes ~state)
          |> Routes.match' ~target
          |> Option.value ~default:[ Tyxml_html.txt "not found" ]
        in
        Current_web.Context.respond_ok context response
    end

  let routes t =
    Routes.
      [
        (s "pipelines" /? nil) @--> handle t (Parts.of_parts "");
        (s "pipelines" /? wildcard) @--> handle t;
        (* TODO: (s "artifacts" /? wildcard) @--> handle_artifacts;*)
      ]
    @ R.extra_routes
end