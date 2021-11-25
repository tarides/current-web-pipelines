let duration_pp ppf t =
  let hour = 3600_000_000_000L in
  let day = Int64.mul 24L hour in
  let year = Int64.mul 8766L hour in
  let open Duration in
  let min = to_min t in
  if min > 0 then
    let y = to_year t in
    let left = Int64.rem t year in
    let d = to_day left in
    let left = Int64.rem left day in
    if y > 0 then Format.fprintf ppf "%da%dd" y d
    else
      let h = to_hour left in
      let left = Int64.rem left hour in
      if d > 0 then Format.fprintf ppf "%dd%02dh" d h
      else
        let min = to_min left in
        let left = Int64.sub t (of_min min) in
        let sec = to_sec left in
        if h > 0 then Format.fprintf ppf "%dh%02dm" h min
        else (* if m > 0 then *)
          Format.fprintf ppf "%dm%02ds" min sec
  else
    (* below one minute *)
    let fields t =
      let sec = to_sec_64 t in
      let left = Int64.sub t (of_sec_64 sec) in
      let ms = to_ms_64 left in
      let left = Int64.sub left (of_ms_64 ms) in
      let us = to_us_64 left in
      let ns = Int64.(sub left (of_us_64 us)) in
      (sec, ms, us, ns)
    in
    let s, ms, us, ns = fields t in
    if s > 0L then Format.fprintf ppf "%Lds" s
    else if ms > 0L then Format.fprintf ppf "%Ldms" ms
    else (* if us > 0 then *)
      Format.fprintf ppf "%Ld.%03Ldus" us ns

type info =
  | No_info
  | Cached
  | Running_since of float
  | Finished of { ready : float; running : float option; finished : float }

type t = { total : float option; info : info }

let empty = { total = None; info = No_info }

let info_to_string = function
  | No_info -> ""
  | Cached -> " (cached)"
  | Running_since v ->
      Fmt.str " (running for %a)" duration_pp
        (Duration.of_f (Unix.gettimeofday () -. v))
  | Finished { ready; running = None; finished } ->
      Fmt.str " (%a queued)" duration_pp (Duration.of_f (finished -. ready))
  | Finished { running = Some running; finished; _ } ->
      Fmt.str " (%a)" duration_pp (Duration.of_f (finished -. running))

let to_elem t =
  let open Tyxml_html in
  let a =
    Option.map
      (fun total ->
        Fmt.str "Total runtime: %a" duration_pp (Duration.of_f total) |> a_title)
      t.total
    |> Option.to_list
  in
  span ~a [ txt (info_to_string t.info) ]

let of_job ~creation_date job_id =
  let info =
    match Current.Job.lookup_running job_id with
    | Some job -> (
        match Lwt.state (Current.Job.start_time job) with
        | Lwt.Sleep | Lwt.Fail _ -> No_info
        | Lwt.Return t -> Running_since (max creation_date t))
    | None -> (
        let results = Current_cache.Db.query ~job_prefix:job_id () in
        match results with
        | [ { finished; _ } ] when finished < creation_date -> Cached
        | [ { Current_cache.Db.ready; running; finished; _ } ] ->
            Finished { ready; running; finished }
        | _ -> No_info)
  in
  { total = None; info }

let run_time = function
  | { total = Some v; _ } -> v
  | { info = No_info; _ } -> 0.
  | { info = Cached; _ } -> 0.
  | { info = Running_since v; _ } -> Unix.gettimeofday () -. v
  | { info = Finished { finished; running = Some running; _ }; _ } ->
      finished -. running
  | _ -> 0.

let merge t1 t2 =
  let info =
    match (t1.info, t2.info) with
    | No_info, t2 -> t2
    | t1, No_info -> t1
    | Cached, t2 -> t2
    | t1, Cached -> t1
    | Running_since v1, Running_since v2 -> Running_since (Float.min v1 v2)
    | Running_since v1, Finished { ready; _ } ->
        Running_since (Float.min v1 ready)
    | Finished { ready; _ }, Running_since v2 ->
        Running_since (Float.min ready v2)
    | Finished v1, Finished v2 ->
        Finished
          {
            ready = Float.min v1.ready v2.ready;
            running =
              (match (v1.running, v2.running) with
              | None, None -> None
              | Some v1, None -> Some v1
              | None, Some v2 -> Some v2
              | Some v1, Some v2 -> Some (Float.min v1 v2));
            finished = Float.max v1.finished v2.finished;
          }
  in
  let total = Some (run_time t1 +. run_time t2) in
  { info; total }

let map_node ~creation_date children metadata =
  match children with
  | `Leaf
      {
        State.metadata = Some { Current.Metadata.job_id = Some job_id; _ };
        result = _;
      } ->
      (metadata, of_job ~creation_date job_id)
  | `Leaf _ -> (metadata, empty)
  | `Children children ->
      let run_time_info = List.map snd children in
      (metadata, List.fold_left merge empty run_time_info)

let map_stage children metadata =
  let run_time_info = List.map snd children in
  (metadata, List.fold_left merge empty run_time_info)
