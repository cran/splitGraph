# Graph assembly helpers.

# Build a helpful error message when edge endpoints don't match any node ID.
# If the missing references look like prefixed IDs (e.g. "sample:S1") while
# the node table uses unprefixed keys (or vice versa), point at that as a
# likely cause -- this is the most common first-time-user mistake.
.depgraph_missing_reference_message <- function(missing, node_ids, side) {
  base_msg <- paste0(
    "Edge data contains `", side, "` references that are not present in the node table: ",
    paste(utils::head(missing, 5L), collapse = ", "),
    if (length(missing) > 5L) paste0(" ... (+", length(missing) - 5L, " more)") else ""
  )

  missing_has_prefix <- any(grepl(":", missing, fixed = TRUE))
  nodes_have_prefix <- any(grepl(":", node_ids, fixed = TRUE))

  hint <- ""
  if (missing_has_prefix && !nodes_have_prefix) {
    hint <- paste0(
      "\nHint: edge endpoints look prefixed (e.g. `sample:S1`) but node IDs are not. ",
      "Either rebuild nodes with `prefix = TRUE` (the default) or rebuild edges with ",
      "`from_prefix = FALSE` and `to_prefix = FALSE`."
    )
  } else if (!missing_has_prefix && nodes_have_prefix) {
    hint <- paste0(
      "\nHint: node IDs are prefixed (e.g. `sample:S1`) but edge endpoints are not. ",
      "Rebuild edges with `from_prefix = TRUE` and `to_prefix = TRUE`, or rebuild nodes ",
      "with `prefix = FALSE`."
    )
  }

  paste0(base_msg, hint)
}

# Provenance of each edge set, keyed by relation: the source columns recorded
# by `create_edges()` and, for the thresholded pairwise helpers, the threshold
# and metric that were applied. Carried in `metadata$edge_sources` so a
# derivation (and the written split_spec) can report the threshold behind a
# relatedness or spatial grouping. When several sets share a relation the last
# one wins; they cannot carry different thresholds in a valid graph anyway.
.depgraph_collect_edge_sources <- function(edge_sets) {
  if (inherits(edge_sets, "graph_edge_set")) edge_sets <- list(edge_sets)
  out <- list()
  for (set in edge_sets) {
    src <- set$source
    relation <- src$relation
    if (is.null(relation) || !nzchar(relation)) next
    out[[relation]] <- src
  }
  out
}

#' Assemble and Validate Dependency Graphs
#'
#' Combine canonical node and edge tables into a typed dependency graph and
#' perform structural, semantic, and graph-local leakage-aware validation.
#'
#' @section Timeline rules:
#' Two leakage-level rules compare timestamps against a sample's index date
#' (\code{index_time}, t0). They check the order of recorded timestamps and
#' nothing else.
#' \describe{
#'   \item{\code{feature_postdates_index_time}}{This is look-ahead leakage
#'     when a sample's predictor data were recorded after its index date: the
#'     sample's \code{collection_time}, or the \code{feature_window_end}
#'     (else \code{collection_time}) of a FeatureSet it uses, is later than
#'     its \code{index_time}. Equal timestamps are allowed.}
#'   \item{\code{outcome_precedes_index_time}}{Flagged when a sample's outcome
#'     timestamp is earlier than its \code{index_time}, its
#'     \code{collection_time} or a linked FeatureSet's window end (else its
#'     \code{collection_time}). The outcome
#'     timestamp is the sample's own \code{outcome_time} (else
#'     \code{event_time}), or else that of the Outcome nodes it reaches through
#'     \code{sample_has_outcome} or its subject's \code{subject_has_outcome}.
#'     This is a timestamp ordering check. It does not detect immortal-time
#'     bias (how follow-up time before treatment or exposure is classified)
#'     or reverse causation; those need the study design, not the graph.}
#' }
#' \code{feature_window_end} lives on the FeatureSet node, so it is one value
#' for every sample using that FeatureSet. It cannot express per-patient
#' windows relative to each patient's own index date; record those as the
#' sample's \code{collection_time}.
#'
#' Timestamps are compared only in the same unit. A \code{Date} or
#' \code{"YYYY-MM-DD"} string is a date; a \code{POSIXct} or
#' \code{"YYYY-MM-DD HH:MM[:SS]"} string (\code{T} or space, optional
#' \code{Z} or \code{+HH:MM} offset; a string without an offset is taken as
#' UTC) is a date-time; a \code{difftime} or ISO 8601 duration string using
#' fixed-length units (\code{W}, \code{D}, \code{H}, \code{M}, \code{S}, e.g.
#' \code{"P1DT2H"}, \code{"-PT30M"}) is a time span, compared in seconds with
#' other time spans only; a number or numeric string is a bare
#' number of unknown unit. Two bare numbers are assumed to be in the same unit.
#' Missing and non-finite values are skipped. A bare number is never compared with a date or
#' date-time, and text that is none of these is never compared: such pairs are
#' reported once per attribute pair as \code{temporal_unit_mismatch} (warning)
#' and the rule is not checked for them. A date and a date-time are compared
#' by calendar day (as written, in the value's own time zone) and reported as
#' \code{temporal_resolution_mismatch} (advisory). \code{summarize_leakage_risks()}
#' reports both rules as not severed by any split, and the two unit codes with
#' \code{severed = NA}.
#'
#' @param nodes,edges Lists of \code{graph_node_set} and \code{graph_edge_set}
#'   objects.
#' @param graph_name,dataset_name Optional metadata labels.
#' @param validate If \code{TRUE}, run \code{validate_graph()} before returning.
#' @param validation_overrides Optional named list of explicit validation
#'   exceptions. Currently supported keys:
#'   \describe{
#'     \item{\code{allow_multi_subject_samples}}{If \code{TRUE}, the semantic
#'       validator does not flag samples linked to multiple subjects, and
#'       \code{derive_split_constraints(mode = "subject")} silently keeps the
#'       first listed subject assignment (recording the ambiguity in
#'       \code{metadata$warnings}). Defaults to \code{FALSE}.}
#'   }
#'   When passed to \code{validate_graph()}, the override is merged into the
#'   graph's existing \code{validation_overrides} for the duration of the call
#'   only.
#' @param graph A \code{dependency_graph}.
#' @param error_on_fail If \code{TRUE}, stop when validation errors are found
#'   across all detected issues from the selected validation levels, even if
#'   those errors are hidden from \code{issues} by \code{severities}.
#' @param levels Optional validation layers to run.
#' @param severities Optional severities to retain in the returned
#'   \code{issues} table. This filter does not change whether the graph is
#'   considered valid.
#' @param x A \code{dependency_graph}.
#' @return For \code{build_dependency_graph()}, a \code{dependency_graph}. For
#'   \code{validate_graph()}, a \code{depgraph_validation_report}. For
#'   \code{as_igraph()}, the underlying \code{igraph} object.
#' @examples
#' meta <- data.frame(
#'   sample_id = c("S1", "S2"),
#'   subject_id = c("P1", "P2")
#' )
#'
#' samples <- create_nodes(meta, type = "Sample", id_col = "sample_id")
#' subjects <- create_nodes(meta, type = "Subject", id_col = "subject_id")
#' edges <- create_edges(
#'   meta,
#'   "sample_id",
#'   "subject_id",
#'   "Sample",
#'   "Subject",
#'   "sample_belongs_to_subject"
#' )
#'
#' g <- build_dependency_graph(list(samples, subjects), list(edges))
#' validate_graph(g)
#' @export
build_dependency_graph <- function(nodes, edges, graph_name = NULL, dataset_name = NULL, validate = TRUE, validation_overrides = list()) {
  node_data <- .depgraph_bind_data(nodes, "data")
  edge_data <- .depgraph_bind_data(edges, "data")
  edge_sources <- .depgraph_collect_edge_sources(edges)
  # Overrides are looked up by name, and an unnamed list would also serialise
  # as a JSON array where the schema declares an object. Reject it here rather
  # than writing an invalid file later.
  .depgraph_assert(
    is.list(validation_overrides) &&
      (length(validation_overrides) == 0L ||
         (!is.null(names(validation_overrides)) && all(nzchar(names(validation_overrides))))),
    "`validation_overrides` must be a named list.",
    code = "invalid_argument"
  )

  .depgraph_assert(
    any(node_data$node_type == "Sample"), "A dependency graph must contain at least one `Sample` node.",
    class = "splitgraph_schema_error", code = "missing_sample_nodes"
  )
  .depgraph_assert(
    anyDuplicated(node_data$node_id) == 0L, "Duplicate `node_id` values found in node sets.",
    class = "splitgraph_reference_error", code = "duplicate_node_id"
  )
  .depgraph_assert(
    anyDuplicated(edge_data$edge_id) == 0L, "Duplicate `edge_id` values found in edge sets.",
    class = "splitgraph_reference_error", code = "duplicate_edge_id"
  )
  .depgraph_assert(
    all(edge_data$from %in% node_data$node_id),
    .depgraph_missing_reference_message(
      missing = setdiff(unique(edge_data$from), node_data$node_id),
      node_ids = node_data$node_id,
      side = "from"
    ),
    class = "splitgraph_reference_error", code = "missing_source_node"
  )
  .depgraph_assert(
    all(edge_data$to %in% node_data$node_id),
    .depgraph_missing_reference_message(
      missing = setdiff(unique(edge_data$to), node_data$node_id),
      node_ids = node_data$node_id,
      side = "to"
    ),
    class = "splitgraph_reference_error", code = "missing_target_node"
  )

  graph_obj <- dependency_graph(
    nodes = graph_node_set(node_data),
    edges = graph_edge_set(edge_data),
    graph = NULL,
    metadata = list(
      graph_name = graph_name,
      dataset_name = dataset_name,
      created_at = Sys.time(),
      schema_version = .depgraph_schema_version,
      validation_overrides = validation_overrides,
      edge_sources = edge_sources
    )
  )

  if (isTRUE(validate)) {
    validation <- validate_graph(graph_obj)
    if (!isTRUE(validation$valid)) {
      .depgraph_stop(
        paste(
          c("Graph validation failed.", validation$errors, validation$warnings),
          collapse = "\n"
        ),
        class = "splitgraph_validation_error",
        code = "graph_validation_failed"
      )
    }
  }

  graph_obj
}

#' @rdname build_dependency_graph
#' @export
as_igraph <- function(x) {
  .depgraph_assert(inherits(x, "dependency_graph"), "`x` must be a `dependency_graph`.")
  x$graph
}
