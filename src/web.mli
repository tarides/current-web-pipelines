type 'a inline =
  ([> Html_types.core_phrasing_without_interactive ] as 'a) Tyxml_html.elt

type 'a block = ([> Html_types.div_content ] as 'a) Tyxml_html.elt

module type Renderer = sig
  val extra_routes : Current_web.Resource.t Routes.route list

  module Output : sig
    type t

    val render_inline : t -> [> Html_types.div_content ] Tyxml_html.elt
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

module Make (R : Renderer) : sig
  type t

  type pipeline_state =
    (R.Output.t, R.Node.t, R.Stage.t, R.Pipeline.t) State.pipeline

  val make : unit -> t
  val update_state : t -> pipeline_state Current.t -> unit Current.t

  val set_active_sources :
    t -> R.Pipeline.Source.t list Current.t -> unit Current.t

  val routes : t -> Current.Engine.t -> Current_web.Resource.t Routes.route list
  val pipeline_page_url : R.Pipeline.t -> string
  val pipeline_stage_url : R.Pipeline.t -> R.Stage.t -> string
end
