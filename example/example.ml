open Current_web_pipelines
module Git = Current_git

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

module Metadata = struct
  module Output = struct
    type t = [ `Git of Git.Commit.t | `No_data ]
  end

  module Node = struct
    type t = string
  end

  module Stage = struct
    type t = Build | Test | Deploy
  end

  module Pipeline = struct
    module Group = struct
      type t = Project_A | Project_B
    end

    module Source = struct
      type src = Pull_request of int | Branch of string
      type t = { src : src; group : Group.t }
    end

    type t = { run : string; source : Source.t }
  end
end

module Pipeline = struct
  let job gref url =
    Git.clone ~gref ~schedule:daily url
    |> Current.map (fun s -> `Git s)
    |> Task.single ("Git clone " ^ url)
    |> Task.map_current ignore

  let mirage () =
    [
      job "main" "https://github.com/mirage/mirage";
      job "main" "https://github.com/mirage/irmin";
    ]
    |> Task.all
    |> Task.map_state (State.job_tree_group "mirage projects")

  let ocurrent () =
    [
      job "master" "https://github.com/ocurrent/ocurrent";
      job "master" "https://github.com/ocurrent/obuilder";
      job "master" "https://github.com/ocurrent/ocluster";
    ]
    |> Task.all
    |> Task.map_state (State.job_tree_group "ocurrent projects")

  let stage_clone () =
    [ mirage (); ocurrent () ]
    |> Task.all
    |> Task.map_state (State.stage Metadata.Stage.Build)

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
    |> Task.map_state (State.stage Metadata.Stage.Deploy)

  let v ~run ~source () =
    [ stage_clone (); stage_write () ]
    |> Task.all
    |> Task.map_state (State.pipeline { Metadata.Pipeline.run; source })
end

module Website_render = struct
  open Tyxml_html

  let extra_routes = []

  module Output = struct
    include Metadata.Output
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
    include Metadata.Node

    let render_inline name = txt name
    let map_status _ = Fun.id
    let marshal = Fun.id
    let unmarshal = Fun.id
  end

  module Stage = struct
    include Metadata.Stage

    let id = function Build -> "build" | Test -> "test" | Deploy -> "deploy"
    let render_inline t = txt (String.capitalize_ascii (id t))
    let render _ = txt ""
    let marshal = id

    let unmarshal = function
      | "build" -> Build
      | "test" -> Test
      | "deploy" -> Deploy
      | _ -> failwith "type error"
  end

  module Pipeline = struct
    include Metadata.Pipeline

    module Group = struct
      include Metadata.Pipeline.Group

      let to_string = function
        | Project_A -> "Project A"
        | Project_B -> "Project B"

      let id = function Project_A -> "project-a" | Project_B -> "project-b"
    end

    module Source = struct
      include Metadata.Pipeline.Source

      let to_string { src; _ } =
        match src with
        | Pull_request n -> Fmt.str "PR #%d" n
        | Branch str -> Fmt.str "Branch %s" str

      let id { src; group } =
        match src with
        | Pull_request n -> Fmt.str "%s-pr-%d" (Group.id group) n
        | Branch str -> Fmt.str "%s-branch-%s" (Group.id group) str

      let group t = t.group
      let compare a b = String.compare (id a) (id b)
    end

    let id (t : t) = t.run
    let source t = t.source

    let branch_name ref =
      match String.split_on_char '/' ref with
      | [ "refs"; "heads"; b ] -> b
      | _ -> "failure"

    let render_inline (t : t) = txt t.run
    let render _ = txt ""
    let marshal v = Marshal.to_string v []
    let unmarshal v = Marshal.from_string v 0
  end

  let render_index () = div [ h1 [ txt "This is a CI" ] ]
end

module Website = Web.Make (Website_render)

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

let do' ~website pipeline =
  let current = Current_web_pipelines.Task.current pipeline in
  let state = Current_web_pipelines.Task.state pipeline in
  let state_update = Website.update_state website state in
  Current.all [ current; state_update ]

let main config mode =
  let website = Website.make () in
  let engine =
    Current.Engine.create ~config (fun () ->
        Current.all
          [
            do' ~website
              (Pipeline.v
                 ~source:{ src = Pull_request 1; group = Project_A }
                 ~run:"5210f2e" ());
            do' ~website
              (Pipeline.v
                 ~source:{ src = Pull_request 1; group = Project_A }
                 ~run:"f60c606" ());
            do' ~website
              (Pipeline.v
                 ~source:{ src = Pull_request 1; group = Project_A }
                 ~run:"473cadf" ());
            do' ~website
              (Pipeline.v
                 ~source:{ src = Pull_request 2; group = Project_A }
                 ~run:"d3702c7" ());
            do' ~website
              (Pipeline.v
                 ~source:{ src = Branch "main"; group = Project_B }
                 ~run:"b5233d0" ());
            Website.set_active_sources website
              (Current.return
                 [
                   {
                     Metadata.Pipeline.Source.src = Pull_request 2;
                     group = Project_A;
                   };
                   {
                     Metadata.Pipeline.Source.src = Branch "main";
                     group = Project_B;
                   };
                 ]);
          ])
  in

  let site =
    let routes = Current_web.routes engine @ Website.routes website engine in
    Current_web.Site.(v ~has_role:Current_web.Site.allow_all)
      ~name:"current-web-pipelines-example" routes
  in
  Logging.run
    (Lwt.choose
       [
         Current.Engine.thread engine;
         (* The main thread evaluating the pipeline. *)
         Current_web.run ~mode site (* Optional: provides a web UI *);
       ])

open Cmdliner

let cmd =
  let doc = "an OCurrent pipeline" in
  let term =
    Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner)
  in
  let info = Cmd.info "current-web-pipelines-example" ~doc in
  Cmd.v info (Term.term_result term)

let () = exit @@ Cmd.eval cmd
