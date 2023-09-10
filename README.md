#  current-web-pipelines

` current-web-pipelines` is a wrapper on top of `ocurrent` pipelines that
provide tools to build complex interfaces, for example representing multi-stage
pipelines.

# Usage

There are three modules:

## `Task`

`('value, 'state) Task.t` is the equivalent of `'value Current.t` with extra
metadata of type `'state`. While the value might not be available (for example
because it is not computed yet), the state can be obtained at all time. `Task`
provides combinators and specialized constructors to  construct a reactive state
structure that can be extracted and displayed in various ways.

## `State`

`('output, 'node_metadata, 'stage_metadata, 'pipeline_metadata) State.pipeline`
is one example of state that can be bound to a task. This type represents the
state of a pipeline run, using the following structure:
- pipeline metadata
- stage list:
  - stage metadata
  - node tree list:
    - node metadata
    - output or status

This tree structure is displayed using the `Web` module.

## `Web`

Use `Web.Make` to create an instance of the website. The API requires to use:
- `make` to initialize
- `update_state` to track the state of a pipeline run. In particular each
  pipeline run is bound to a _source_ (the reason why it runs) and is part of a
  group.
- `set_active_sources` to track which sources are still active
- `routes` to provide `current_web` with additional routes for the
  generated website

# Examples

See the `example/` folder for an end-to-end usage of `current-web-pipelines`.
It's also deployed in [mirage-ci](https://github.com/ocurrent/mirage-ci), [ocaml-docs-ci](https://github.com/ocurrent/ocaml-docs-ci) and
was used in [tezos-ci](https://github.com/tarides/tezos-ci).
