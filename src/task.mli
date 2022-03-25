type ('current, 'state) t
(** A pipeline with current 'current and metadata 'state.
 the current can be blocking, but the state shouldn't.  *)

type 'state simple = (unit, 'state) t

val current : ('current, _) t -> 'current Current.t
(** Extractors *)

val state : (_, 'state) t -> 'state Current.t

(** Constructors *)

val v :
  current:'current Current.t -> state:'state Current.t -> ('current, 'state) t

(** Operations *)

val list_iter :
  collapse_key:string ->
  (module Current_term.S.ORDERED with type t = 'a) ->
  ('a Current.t -> (unit, 'state) t) ->
  'a list Current.t ->
  (unit, 'state list) t

val all : (unit, 'state) t list -> (unit, 'state list) t
val list_seq : ('current, 'state) t list -> ('current list, 'state list) t

val map_state :
  ('state -> 'next_state) -> ('current, 'state) t -> ('current, 'next_state) t

val map_current :
  ('current -> 'next_current) ->
  ('current, 'state) t ->
  ('next_current, 'state) t

val apply_current :
  ('current Current.t -> 'next_current Current.t) ->
  ('current, 'state) t ->
  ('next_current, 'state) t

(** Specialized constructors *)

val single :
  'metadata ->
  'output Current.t ->
  ('output, ('output, 'metadata) State.job_tree) t

val single_c :
  'metadata Current.t ->
  'output Current.t ->
  ('output, ('output, 'metadata) State.job_tree) t
