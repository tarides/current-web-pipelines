open Current_web_pipelines
module Git = Current_git

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

module Pipeline = struct
  type output = [ `Git of Git.Commit.t | `No_data ]

  let job url =
    Git.clone ~schedule:daily url
    |> Current.map (fun s -> `Git s)
    |> Task.single ("Git clone " ^ url)
    |> Task.map_current ignore

  let mirage () =
    [
      job "https://github.com/mirage/mirage";
      job "https://github.com/mirage/irmin";
    ]
    |> Task.all
    |> Task.map_state (State.job_tree_group "mirage projects")

  let ocurrent () =
    [
      job "https://github.com/ocurrent/ocurrent";
      job "https://github.com/ocurrent/obuilder";
      job "https://github.com/ocurrent/ocluster";
    ]
    |> Task.all
    |> Task.map_state (State.job_tree_group "ocurrent projects")

  let stage_clone () =
    [ mirage (); ocurrent () ] |> Task.all
    |> Task.map_state (State.stage "clone projects")

  let stage_write () =
    [
      Current_fs.save
        (Current.return (Fpath.v "/tmp/dummy"))
        (Current.return "hello")
      |> Current.map (fun () -> `No_data)
      |> Task.single "Save hello to /tmp/dummy"
      |> Task.map_current ignore;
    ]
    |> Task.all
    |> Task.map_state (State.stage "write files")

  let v () =
    [ stage_clone (); stage_write () ]
    |> Task.all
    |> Task.map_state (State.pipeline "a pipeline")
end

module Website_render = struct
  open Tyxml_html

  let extra_routes = []

  module Output = struct
    type t = Pipeline.output

    open Tyxml_html

    let render_inline = function
      | `No_data -> txt ""
      | `Git commit ->
          let h = Git.Commit.id commit |> Git.Commit_id.hash in
          span [ txt "git commit is "; i [ txt (String.sub h 0 7) ] ]

    let marshal = function
      | `No_data -> "N"
      | `Git commit -> "G" ^ Git.Commit.marshal commit

    let unmarshal s =
      match s.[0] with
      | 'N' -> `No_data
      | 'G' ->
          `Git (Git.Commit.unmarshal (String.sub s 1 (String.length s - 1)))
      | _ -> failwith "type error"
  end

  let sanitize = String.map (function ' ' -> '-' | c -> c)

  module Node = struct
    type t = string

    let render_inline name = txt name

    let map_status _ = Fun.id

    let marshal = Fun.id

    let unmarshal = Fun.id
  end

  module Stage = struct
    type t = string

    let id name = sanitize name

    let render_inline name = txt name

    let render _ = txt ""

    let marshal = Fun.id

    let unmarshal = Fun.id
  end

  module Pipeline = struct
    type t = string

    let id (t : t) = sanitize t

    let branch_name ref =
      match String.split_on_char '/' ref with
      | [ "refs"; "heads"; b ] -> b
      | _ -> "failure"

    let render_inline (t : t) = txt t

    let render _ = txt ""

    let marshal = Fun.id

    let unmarshal = Fun.id
  end

  let render_index () = div [ h1 [ txt "This is a CI" ] ]
end

module Website = Web.Make (Website_render)

module Fake_pipeline_state = struct end

module Logging = struct
  let reporter =
    let report src level ~over k msgf =
      let k _ =
        over ();
        k ()
      in
      let src = Logs.Src.name src in
      msgf @@ fun ?header ?tags:_ fmt ->
      Fmt.kpf k Fmt.stdout
        ("%a %a @[" ^^ fmt ^^ "@]@.")
        Fmt.(styled `Magenta string)
        (Printf.sprintf "%14s" src)
        Logs_fmt.pp_header (level, header)
    in
    { Logs.report }

  let init ?(level = Logs.Info) () =
    Fmt_tty.setup_std_outputs ();
    Logs.set_level (Some level);
    Logs.set_reporter reporter

  let run x =
    match Lwt_main.run x with
    | Ok () -> Ok ()
    | Error (`Msg m) as e ->
        Logs.err (fun f -> f "%a" Fmt.lines m);
        e
end

let () = Logging.init ()

let main config mode =
  let website = Website.make () in
  let engine =
    Current.Engine.create ~config (fun () ->
        let pipeline = Pipeline.v () in

        let current = Current_web_pipelines.Task.current pipeline in
        let state = Current_web_pipelines.Task.state pipeline in
        let state_update = Website.update_state website state in
        Current.all [ current; state_update ])
  in

  let site =
    let routes = Current_web.routes engine @ Website.routes website in
    Current_web.Site.(v ~has_role:Current_web.Site.allow_all)
      ~name:"current-web-pipelines-example" routes
  in
  Logging.run
    (Lwt.choose
       [
         Current.Engine.thread engine;
         (* The main thread evaluating the pipeline. *)
         Current_web.run ~mode site;
         (* Optional: provides a web UI *)
       ])

open Cmdliner

let cmd =
  let doc = "an OCurrent pipeline" in
  ( Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner),
    Term.info "current-web-pipelines-example" ~doc )

let () = Term.(exit @@ eval cmd)
