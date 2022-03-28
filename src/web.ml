type 'a inline =
  ([> Html_types.core_phrasing_without_interactive ] as 'a) Tyxml_html.elt

type 'a block = ([> Html_types.div_content ] as 'a) Tyxml_html.elt

module type Renderer = sig
  val extra_routes : Current_web.Resource.t Routes.route list

  module Output : sig
    type t

    val render_inline : t -> _ block
    val marshal : t -> string
    val unmarshal : string -> t
  end

  module Node : sig
    type t

    val render_inline : t -> _ inline
    val map_status : t -> 'a State.job_result -> 'a State.job_result
    val marshal : t -> string
    val unmarshal : string -> t
  end

  module Stage : sig
    type t

    val id : t -> string
    val render_inline : t -> _ inline
    val render : t -> _ block
    val marshal : t -> string
    val unmarshal : string -> t
  end

  module Pipeline : sig
    module Group : sig
      type t

      val to_string : t -> string
      val id : t -> string
    end

    module Source : sig
      type t

      val to_string : t -> string
      val id : t -> string
      val group : t -> Group.t
      val compare : t -> t -> int
    end

    type t

    val id : t -> string
    val source : t -> Source.t
    val render_inline : t -> _ inline
    val render : t -> _ block
    val marshal : t -> string
    val unmarshal : string -> t
  end

  val render_index : unit -> _ block
end

module Db = struct
  type t = {
    db : Sqlite3.db;
    update_pipeline : Sqlite3.stmt;
    get_pipelines : Sqlite3.stmt;
    remove_pipeline : Sqlite3.stmt;
  }

  let or_fail label x =
    match x with
    | Sqlite3.Rc.OK -> ()
    | err ->
        Fmt.failwith "Sqlite3 %s error: %s" label (Sqlite3.Rc.to_string err)

  type entry = {
    pipeline_source : string;
    pipeline_id : string;
    pipeline_content : string;
    creation_date : int64;
  }

  let db =
    lazy
      (let db = Lazy.force Current.Db.v in
       Current_cache.Db.init ();
       Sqlite3.exec db
         {|
CREATE TABLE IF NOT EXISTS current_web_pipelines_index (
  pipeline_source   TEXT NOT NULL,
  pipeline_id       TEXT NOT NULL,
  pipeline_content  BLOB NOT NULL,
  creation_date     INTEGER NOT NULL,
  PRIMARY KEY (pipeline_source, pipeline_id)
)|}
       |> or_fail "create table";
       let update_pipeline =
         Sqlite3.prepare db
           "INSERT OR REPLACE INTO current_web_pipelines_index \
            (pipeline_source, pipeline_id, pipeline_content, creation_date) \
            VALUES (?, ?, ?, ?)"
       in
       let get_pipelines =
         Sqlite3.prepare db
           "SELECT pipeline_source, pipeline_id, pipeline_content, \
            creation_date FROM current_web_pipelines_index"
       in
       let remove_pipeline =
         Sqlite3.prepare db
           "DELETE FROM current_web_pipelines_index WHERE pipeline_source = ? \
            AND pipeline_id = ?"
       in
       { db; update_pipeline; get_pipelines; remove_pipeline })

  let get_pipelines t =
    Current.Db.query t.get_pipelines []
    |> List.map @@ function
       | Sqlite3.Data.
           [
             TEXT pipeline_source;
             TEXT pipeline_id;
             BLOB pipeline_content;
             INT creation_date;
           ] ->
           { pipeline_source; pipeline_id; pipeline_content; creation_date }
       | row ->
           Fmt.failwith "get_pipelines: invalid row %a" Current.Db.dump_row row

  let update_pipeline t v =
    Current.Db.exec t.update_pipeline
      Sqlite3.Data.
        [
          TEXT v.pipeline_source;
          TEXT v.pipeline_id;
          BLOB v.pipeline_content;
          INT v.creation_date;
        ]

  let remove_pipeline t pipeline_source pipeline_id =
    Current.Db.exec t.remove_pipeline
      Sqlite3.Data.[ TEXT pipeline_source; TEXT pipeline_id ]
end

module StringMap = Map.Make (String)

module Make (R : Renderer) = struct
  module GroupMap = Map.Make (struct
    type t = R.Pipeline.Group.t

    let compare a b =
      String.compare (R.Pipeline.Group.id a) (R.Pipeline.Group.id b)
  end)

  module SourceMap = Map.Make (struct
    type t = R.Pipeline.Source.t

    let compare a b =
      String.compare (R.Pipeline.Source.id a) (R.Pipeline.Source.id b)
  end)

  type pipeline_state =
    (R.Output.t, R.Node.t, R.Stage.t, R.Pipeline.t) State.pipeline

  type pipeline_metadata = {
    user_meta : R.Pipeline.t;
    run_time : Run_time.t;
    creation_date : float;
  }

  type pipeline_state_internal =
    ( R.Output.t,
      R.Node.t * Run_time.t,
      R.Stage.t * Run_time.t,
      pipeline_metadata )
    State.pipeline

  type t = pipeline_state_internal StringMap.t SourceMap.t ref

  let unmarshal =
    State.unmarshal R.Output.unmarshal R.Node.unmarshal R.Stage.unmarshal
      R.Pipeline.unmarshal

  let marshal =
    State.marshal R.Output.marshal R.Node.marshal R.Stage.marshal
      R.Pipeline.marshal

  let compute_run_time ~creation_date state =
    state
    |> State.map
         (Run_time.map_node ~creation_date)
         Run_time.map_stage Run_time.map_stage
    |> fun ({ metadata = user_meta, run_time; _ } as v) ->
    { v with metadata = { user_meta; run_time; creation_date } }

  let group_by_key fn lst =
    List.sort (fun (a, _) (b, _) -> String.compare (fn a) (fn b)) lst
    |> function
    | [] -> []
    | (k0, v0) :: rest ->
        let (last_key, last_value), acc =
          List.fold_left
            (fun ((current_key, current_values), acc) (new_key, new_value) ->
              if String.equal (fn current_key) (fn new_key) then
                ((current_key, new_value :: current_values), acc)
              else
                ( (new_key, [ new_value ]),
                  (current_key, List.rev current_values) :: acc ))
            ((k0, [ v0 ]), [])
            rest
        in
        List.rev ((last_key, List.rev last_value) :: acc)

  let make () : t =
    let db = Lazy.force Db.db in
    let init_state =
      Db.get_pipelines db
      |> List.filter_map (fun (t : Db.entry) ->
             let creation_date = Int64.to_float t.creation_date in
             try
               let state = unmarshal t.pipeline_content in
               let source = R.Pipeline.source state.metadata in
               let state = compute_run_time ~creation_date state in
               Some (source, (t.pipeline_id, state))
             with _ ->
               Printf.printf
                 "[warning] failed to unmarshal. The entry should be removed.\n";
               Db.remove_pipeline db t.pipeline_source t.pipeline_id;
               None)
      |> group_by_key R.Pipeline.Source.id
      |> List.map (fun (k, pipelines) ->
             (k, List.to_seq pipelines |> StringMap.of_seq))
      |> List.to_seq |> SourceMap.of_seq
    in
    ref init_state

  let get ~pipeline_source ~pipeline_id (state : t) =
    match SourceMap.find_opt pipeline_source !state with
    | None -> None
    | Some v -> StringMap.find_opt pipeline_id v

  let set ~pipeline_source ~pipeline_id (state : t) new_state =
    match SourceMap.find_opt pipeline_source !state with
    | None ->
        state :=
          SourceMap.add pipeline_source
            (StringMap.singleton pipeline_id new_state)
            !state
    | Some v ->
        state :=
          SourceMap.add pipeline_source
            ((StringMap.add pipeline_id new_state) v)
            !state

  let update_state (state : t) (new_state : pipeline_state Current.t) =
    let open Current.Syntax in
    let+ new_state = new_state in

    let pipeline_id = new_state.metadata |> R.Pipeline.id in
    let pipeline_source = new_state.metadata |> R.Pipeline.source in

    let creation_date =
      match get ~pipeline_id ~pipeline_source state with
      | None -> Unix.gettimeofday ()
      | Some { metadata = { creation_date; _ }; _ } -> creation_date
    in

    let () =
      let db = Lazy.force Db.db in
      Db.update_pipeline db
        {
          pipeline_source = R.Pipeline.Source.id pipeline_source;
          pipeline_id;
          pipeline_content = marshal new_state;
          creation_date = Int64.of_float creation_date;
        }
    in
    let new_state =
      State.map
        (Run_time.map_node ~creation_date)
        Run_time.map_stage Run_time.map_stage new_state
      |> fun ({ metadata = user_meta, run_time; _ } as v) ->
      { v with metadata = { user_meta; run_time; creation_date } }
    in
    set ~pipeline_source ~pipeline_id state new_state
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

  let list_max_by fn = function
    | [] -> raise Not_found
    | a :: b ->
        List.fold_left (fun max cur -> if fn max > fn cur then max else cur) a b

  (* LIST PIPELINES *)

  let node_map_status (node_meta, _) = R.Node.map_status node_meta

  let list_pipelines ~(state : t) =
    let open Tyxml_html in
    let show_pipeline (pipeline : pipeline_state_internal) =
      let { user_meta; run_time; _ } = pipeline.metadata in
      let id = R.Pipeline.id user_meta in
      let source = R.Pipeline.source user_meta in
      let source_id = R.Pipeline.Source.id source in
      [
        h4
          [
            txt (R.Pipeline.Source.to_string source ^ ": ");
            emoji_of_status (State.pipeline_status ~node_map_status pipeline);
            a
              ~a:[ a_href (Fmt.str "/pipelines/%s/%s" source_id id) ]
              [ R.Pipeline.render_inline user_meta ];
            i [ Run_time.to_elem run_time ];
          ];
      ]
    in
    let render_source pipelines =
      let _, latest =
        StringMap.bindings pipelines
        |> list_max_by
             (fun ((_, { metadata; _ }) : _ * pipeline_state_internal) ->
               metadata.creation_date)
      in
      show_pipeline latest
    in
    [ div [ R.render_index () ]; h2 [ txt "Pipelines" ] ]
    @ (SourceMap.bindings !state
      |> List.sort (fun (a, _) (b, _) -> R.Pipeline.Source.compare a b)
      |> List.map (fun (source, pipelines) ->
             (R.Pipeline.Source.group source, (source, pipelines)))
      |> group_by_key R.Pipeline.Group.id
      |> List.map (fun (group, sources) ->
             div
               [
                 h3 [ txt (R.Pipeline.Group.to_string group) ];
                 ul
                   (List.map
                      (fun (_, pipelines) -> li (render_source pipelines))
                      sources);
               ]))

  (* SHOW PIPELINES *)
  let find_pipeline ~(state : t) pipeline_source_id pipeline_id =
    let src, pipelines =
      SourceMap.bindings !state
      |> List.find (fun (source, _) ->
             R.Pipeline.Source.id source = pipeline_source_id)
    in
    (src, StringMap.find pipeline_id pipelines)

  let show_pipeline ~(state : t) pipeline_source_id pipeline_id =
    let src, pipeline = find_pipeline ~state pipeline_source_id pipeline_id in
    let { user_meta; creation_date; _ } = pipeline.metadata in
    let date = Unix.gmtime creation_date in
    let datestr (date : Unix.tm) =
      Fmt.str "%02d/%02d/%4d %02d:%02d" date.tm_mday (1 + date.tm_mon)
        (1900 + date.tm_year) date.tm_hour date.tm_min
    in
    let open Tyxml_html in
    [
      br ();
      a ~a:[ a_href ".." ] [ txt "Back" ];
      h1
        [
          txt
            (R.Pipeline.Group.to_string (R.Pipeline.Source.group src)
            ^ ": "
            ^ R.Pipeline.Source.to_string src);
        ];
      h2 [ R.Pipeline.render_inline user_meta ];
      i [ txt (datestr date) ];
      br ();
      br ();
      R.Pipeline.render user_meta;
      h2 [ txt "Stages:" ];
      ul
        (List.map
           (fun (stage : _ State.stage) ->
             let metadata, run_time = stage.metadata in
             li
               [
                 emoji_of_status (State.stage_status ~node_map_status stage);
                 a
                   ~a:
                     [
                       a_href
                         (Fmt.str "/pipelines/%s/%s/%s" pipeline_source_id
                            pipeline_id (R.Stage.id metadata));
                     ]
                   [ R.Stage.render_inline metadata ];
                 i [ Run_time.to_elem run_time ];
               ])
           pipeline.stages);
      br ();
      h2 [ txt "History:" ];
      ul
        (SourceMap.find src !state |> StringMap.bindings |> List.rev_map snd
        |> List.sort
             (fun
               ({ metadata = { creation_date = a; _ }; _ } :
                 pipeline_state_internal)
               ({ metadata = { creation_date = b; _ }; _ } :
                 pipeline_state_internal)
             -> Float.compare b a)
        |> List.map
             (fun
               ({ metadata = { user_meta; run_time; creation_date }; _ } as
                pipeline :
                 pipeline_state_internal)
             ->
               let id = R.Pipeline.id user_meta in
               let source = R.Pipeline.source user_meta in
               let source_id = R.Pipeline.Source.id source in
               let date = Unix.gmtime creation_date in
               let maybe_b = if pipeline_id = id then b else span in
               li
                 [
                   maybe_b [ i [ txt (datestr date ^ "   ") ] ];
                   emoji_of_status
                     (State.pipeline_status ~node_map_status pipeline);
                   a
                     ~a:[ a_href (Fmt.str "/pipelines/%s/%s" source_id id) ]
                     [ R.Pipeline.render_inline user_meta ];
                   i [ Run_time.to_elem run_time ];
                 ]));
    ]

  (* JOB TREE *)

  let render_result v = Result.map R.Output.render_inline v |> Result.to_list

  let rec get_job_tree ~uri_base (job_tree : _ State.job_tree) =
    let status = State.job_tree_status ~node_map_status job_tree in
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

  let show_pipeline_task ~(state : t) pipeline_source_id pipeline_id stage_id =
    let _, pipeline = find_pipeline ~state pipeline_source_id pipeline_id in
    let { user_meta; _ } = pipeline.metadata in
    let stage =
      List.find
        (fun (t : _ State.stage) -> R.Stage.id (fst t.metadata) = stage_id)
        pipeline.stages
    in
    let job_trees =
      List.map
        (get_job_tree
           ~uri_base:
             (Fmt.str "/pipelines/%s/%s/%s" pipeline_source_id pipeline_id
                stage_id))
        stage.jobs
    in
    let open Tyxml_html in
    [
      h1
        [
          emoji_of_status (State.pipeline_status ~node_map_status pipeline);
          a
            ~a:
              [
                a_href
                  (Fmt.str "/pipelines/%s/%s" pipeline_source_id pipeline_id);
              ]
            [ R.Pipeline.render_inline user_meta ];
        ];
      h2
        [
          emoji_of_status (State.stage_status ~node_map_status stage);
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

  let show_pipeline_task_job ~(state : t) ipeline_source_id pipeline_id stage_id
      wildcard =
    let job_id =
      let wld = Routes.Parts.wildcard_match wildcard in
      String.sub wld 1 (String.length wld - 1)
    in
    let pipeline_tasks =
      show_pipeline_task ~state ipeline_source_id pipeline_id stage_id
    in
    let open Tyxml_html in
    [
      div
        ~a:[ a_style "display: flex;" ]
        [
          div ~a:[ a_style "width: 50%" ] pipeline_tasks;
          div
            ~a:[ a_style "width: 50%" ]
            [
              h2 [ txt "Job log" ];
              a
                ~a:[ a_href ("/job/" ^ job_id) ]
                [ txt "See full log and operations" ];
              pre [ txt (get_job_text job_id) ];
            ];
        ];
    ]

  let pipeline_page_url (data : R.Pipeline.t) =
    Fmt.str "/pipelines/%s/%s"
      R.Pipeline.(source data |> Source.id)
      (R.Pipeline.id data)

  let pipeline_stage_url (data : R.Pipeline.t) (stage : R.Stage.t) =
    Fmt.str "/pipelines/%s/%s/%s"
      R.Pipeline.(source data |> Source.id)
      (R.Pipeline.id data) (R.Stage.id stage)

  (* ROUTING *)
  let internal_routes ~state =
    Routes.
      [
        empty @--> list_pipelines ~state;
        (str / str /? nil) @--> show_pipeline ~state;
        (str / str / str /? nil) @--> show_pipeline_task ~state;
        (str / str / str /? wildcard) @--> show_pipeline_task_job ~state;
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
        (s "pipelines" /? wildcard) @--> handle t
        (* TODO: (s "artifacts" /? wildcard) @--> handle_artifacts;*);
      ]
    @ R.extra_routes
end
