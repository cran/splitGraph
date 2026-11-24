# Validation helpers for dependency_graph objects.

.depgraph_empty_issue_data <- function() {
  data.frame(
    issue_id = character(),
    level = character(),
    severity = character(),
    code = character(),
    message = character(),
    node_ids = I(list()),
    edge_ids = I(list()),
    details = I(list()),
    stringsAsFactors = FALSE
  )
}

.new_validation_issue <- function(level, severity, code, message, node_ids = character(), edge_ids = character(), details = list()) {
  .depgraph_assert(level %in% .depgraph_validation_levels, paste0("Unsupported validation level: ", level))
  .depgraph_assert(severity %in% .depgraph_validation_severities, paste0("Unsupported validation severity: ", severity))

  data.frame(
    issue_id = NA_character_,
    level = level,
    severity = severity,
    code = code,
    message = as.character(message)[1L],
    node_ids = I(list(as.character(node_ids))),
    edge_ids = I(list(as.character(edge_ids))),
    details = I(list(.depgraph_normalize_attr_entry(details, context = "`details`"))),
    stringsAsFactors = FALSE
  )
}

# Bulk variant of `.new_validation_issue()`: one issue per element of
# `messages`, with per-issue node/edge id vectors supplied as lists. Building the
# table once instead of rbind-ing one-row frames is what keeps validation linear
# on cohorts with thousands of per-subject advisories.
.new_validation_issues <- function(level, severity, code, messages, node_ids = NULL, edge_ids = NULL, details = NULL) {
  n <- length(messages)
  if (n == 0L) {
    return(NULL)
  }
  .depgraph_assert(level %in% .depgraph_validation_levels, paste0("Unsupported validation level: ", level))
  .depgraph_assert(severity %in% .depgraph_validation_severities, paste0("Unsupported validation severity: ", severity))

  node_ids <- if (is.null(node_ids)) rep(list(character()), n) else lapply(node_ids, as.character)
  edge_ids <- if (is.null(edge_ids)) rep(list(character()), n) else lapply(edge_ids, as.character)
  details <- if (is.null(details)) rep(list(list()), n) else details
  details <- lapply(details, .depgraph_normalize_attr_entry, context = "`details`")

  data.frame(
    issue_id = NA_character_,
    level = level,
    severity = severity,
    code = code,
    message = as.character(messages),
    node_ids = I(unname(node_ids)),
    edge_ids = I(unname(edge_ids)),
    details = I(unname(details)),
    stringsAsFactors = FALSE
  )
}

.depgraph_bind_issues <- function(issues) {
  issues <- Filter(Negate(is.null), issues)
  if (length(issues) == 0L) {
    return(.depgraph_empty_issue_data())
  }

  out <- do.call(rbind, issues)
  row.names(out) <- NULL
  if (nrow(out) == 0L) {
    return(.depgraph_empty_issue_data())
  }
  out$issue_id <- paste0("issue_", seq_len(nrow(out)))
  out
}

.depgraph_issue_count_table <- function(x, column) {
  if (nrow(x) == 0L) {
    return(data.frame(value = character(), n = integer(), stringsAsFactors = FALSE))
  }
  .depgraph_count_table(x[[column]])
}

.summarize_validation_issues <- function(issues) {
  list(
    n_issues = nrow(issues),
    by_level = .depgraph_issue_count_table(issues, "level"),
    by_severity = .depgraph_issue_count_table(issues, "severity"),
    by_code = .depgraph_issue_count_table(issues, "code")
  )
}

.validate_structural <- function(graph) {
  node_data <- graph$nodes$data
  edge_data <- graph$edges$data
  issues <- list()

  alignment <- .depgraph_graph_table_alignment(graph$graph, node_data, edge_data)
  if (!isTRUE(alignment$valid)) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "igraph_table_mismatch",
      message = alignment$message
    )
  }

  if (anyNA(node_data$node_id) || any(!nzchar(node_data$node_id))) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "missing_node_id",
      message = "Node table contains missing or empty `node_id` values."
    )
  }

  if (anyDuplicated(node_data$node_id) > 0L) {
    dup_ids <- unique(node_data$node_id[duplicated(node_data$node_id)])
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "duplicate_node_id",
      message = paste0("Node table contains duplicated `node_id` values: ", paste(dup_ids, collapse = ", ")),
      node_ids = dup_ids
    )
  }

  if (anyNA(edge_data$edge_id) || any(!nzchar(edge_data$edge_id))) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "missing_edge_id",
      message = "Edge table contains missing or empty `edge_id` values."
    )
  }

  if (anyDuplicated(edge_data$edge_id) > 0L) {
    dup_ids <- unique(edge_data$edge_id[duplicated(edge_data$edge_id)])
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "duplicate_edge_id",
      message = paste0("Edge table contains duplicated `edge_id` values: ", paste(dup_ids, collapse = ", ")),
      edge_ids = dup_ids
    )
  }

  if (nrow(edge_data) > 0L) {
    triplets <- paste(edge_data$from, edge_data$to, edge_data$edge_type, sep = "||")
    if (anyDuplicated(triplets) > 0L) {
      dup_rows <- duplicated(triplets) | duplicated(triplets, fromLast = TRUE)
      dup_edges <- edge_data$edge_id[dup_rows]
      issues[[length(issues) + 1L]] <- .new_validation_issue(
        level = "structural",
        severity = "error",
        code = "duplicate_edge_triplet",
        message = "Edge table contains duplicate `(from, to, edge_type)` relations.",
        edge_ids = dup_edges
      )
    }
  }

  valid_nodes <- node_data$node_id
  bad_from <- setdiff(unique(edge_data$from), valid_nodes)
  bad_to <- setdiff(unique(edge_data$to), valid_nodes)
  if (length(bad_from) > 0L) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "missing_source_node",
      message = paste0("Edges reference missing source nodes: ", paste(bad_from, collapse = ", ")),
      node_ids = bad_from
    )
  }
  if (length(bad_to) > 0L) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "missing_target_node",
      message = paste0("Edges reference missing target nodes: ", paste(bad_to, collapse = ", ")),
      node_ids = bad_to
    )
  }

  bad_node_types <- setdiff(unique(node_data$node_type), .depgraph_node_types)
  if (length(bad_node_types) > 0L) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "structural",
      severity = "error",
      code = "unsupported_node_type",
      message = paste0("Unsupported node types found: ", paste(bad_node_types, collapse = ", "))
    )
  }

  if (nrow(edge_data) > 0L) {
    # Vectorised signature check: one join of the edge table against the schema.
    type_map <- stats::setNames(node_data$node_type, node_data$node_id)
    schema_idx <- match(edge_data$edge_type, .depgraph_edge_schema$edge_type)
    unknown <- is.na(schema_idx)

    if (any(unknown)) {
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "structural",
        severity = "error",
        code = "unsupported_edge_type",
        messages = paste0("Unknown edge type encountered: ", edge_data$edge_type[unknown]),
        edge_ids = as.list(edge_data$edge_id[unknown])
      )
    }

    known <- which(!unknown)
    if (length(known) > 0L) {
      expected_from <- .depgraph_edge_schema$from_type[schema_idx[known]]
      expected_to <- .depgraph_edge_schema$to_type[schema_idx[known]]
      observed_from <- unname(type_map[edge_data$from[known]])
      observed_to <- unname(type_map[edge_data$to[known]])
      # An endpoint missing from the node table (reported separately above)
      # compares as NA; treat it as a mismatch, as the per-edge loop did.
      bad <- is.na(observed_from) | is.na(observed_to) |
        observed_from != expected_from | observed_to != expected_to
      bad_idx <- known[bad]
      if (length(bad_idx) > 0L) {
        issues[[length(issues) + 1L]] <- .new_validation_issues(
          level = "structural",
          severity = "error",
          code = "invalid_edge_signature",
          messages = paste0(
            "Edge `", edge_data$edge_id[bad_idx], "` violates schema for `", edge_data$edge_type[bad_idx],
            "` (expected ", expected_from[bad], " -> ", expected_to[bad],
            ", found ", observed_from[bad], " -> ", observed_to[bad], ")."
          ),
          node_ids = Map(c, edge_data$from[bad_idx], edge_data$to[bad_idx]),
          edge_ids = as.list(edge_data$edge_id[bad_idx])
        )
      }
    }
  }

  for (edge_type in .depgraph_single_target_edges) {
    subset_idx <- edge_data$edge_type == edge_type
    if (!any(subset_idx)) {
      next
    }
    edge_subset <- edge_data[subset_idx, c("edge_id", "from", "to"), drop = FALSE]
    counts <- .depgraph_count_unique_by(edge_subset$to, edge_subset$from)
    bad <- data.frame(from = counts$by[counts$n > 1L], stringsAsFactors = FALSE)
    if (nrow(bad) > 0L) {
      offending_edges <- edge_subset$edge_id[edge_subset$from %in% bad$from]
      issues[[length(issues) + 1L]] <- .new_validation_issue(
        level = "structural",
        severity = "error",
        code = "single_target_violation",
        message = paste0(
          "Edge type `", edge_type, "` links a source node to multiple targets: ",
          paste(bad$from, collapse = ", ")
        ),
        node_ids = bad$from,
        edge_ids = offending_edges,
        details = list(edge_type = edge_type)
      )
    }
  }

  .depgraph_bind_issues(issues)
}

.validate_semantic <- function(graph) {
  node_data <- graph$nodes$data
  edge_data <- graph$edges$data
  issues <- list()

  if (nrow(node_data) > 0L) {
    # Only nodes that carry attributes can have unknown ones, and no node type
    # currently declares required attributes; still evaluate the required check
    # for every node type present (once per type, not once per node) so a
    # future schema with required attrs keeps working.
    has_attrs <- lengths(node_data$attrs) > 0L
    required_by_type <- vapply(
      unique(node_data$node_type),
      function(type) length(.depgraph_node_type_schema(type)$required_attrs) > 0L,
      logical(1)
    )
    needs_check <- has_attrs | required_by_type[node_data$node_type]

    checks <- vector("list", nrow(node_data))
    checks[needs_check] <- Map(
      function(type, attrs) .depgraph_validate_node_attrs(type, attrs),
      node_data$node_type[needs_check], node_data$attrs[needs_check]
    )
    missing_required <- lapply(checks, function(x) if (is.null(x)) character() else x$missing_required)
    unknown_attrs <- lapply(checks, function(x) if (is.null(x)) character() else x$unknown_attrs)

    miss_idx <- which(lengths(missing_required) > 0L)
    if (length(miss_idx) > 0L) {
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "semantic",
        severity = "error",
        code = "missing_required_node_attr",
        messages = paste0(
          "Node `", node_data$node_id[miss_idx], "` is missing required attributes: ",
          vapply(missing_required[miss_idx], paste, character(1), collapse = ", ")
        ),
        node_ids = as.list(node_data$node_id[miss_idx])
      )
    }

    unk_idx <- which(has_attrs & lengths(unknown_attrs) > 0L)
    if (length(unk_idx) > 0L) {
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "semantic",
        severity = "warning",
        code = "non_canonical_node_attr",
        messages = paste0(
          "Node `", node_data$node_id[unk_idx], "` contains non-canonical attributes: ",
          vapply(unknown_attrs[unk_idx], paste, character(1), collapse = ", ")
        ),
        node_ids = as.list(node_data$node_id[unk_idx])
      )
    }
  }

  sample_ids <- node_data$node_id[node_data$node_type == "Sample"]
  for (relation in names(.depgraph_semantic_relation_rules)) {
    rule <- .depgraph_semantic_relation_rules[[relation]]
    schema_row <- .depgraph_edge_type_schema(relation)
    relation_edges <- edge_data[edge_data$edge_type == relation, c("edge_id", "from", "to"), drop = FALSE]

    counts <- stats::setNames(integer(length(sample_ids)), sample_ids)
    targets <- vector("list", length(sample_ids))
    names(targets) <- sample_ids

    if (nrow(relation_edges) > 0L) {
      targets_by_sample <- lapply(split(relation_edges$to, relation_edges$from), unique)
      counts[names(targets_by_sample)] <- lengths(targets_by_sample)
      targets[names(targets_by_sample)] <- targets_by_sample
    }

    if (!is.null(rule$missing_code) && !is.null(rule$missing_severity) && rule$min_targets > 0L) {
      target_type_present <- nrow(schema_row) == 1L &&
        any(node_data$node_type == schema_row$to_type[[1L]])
      if (nrow(relation_edges) > 0L || target_type_present) {
        missing_samples <- names(counts)[counts < rule$min_targets]
        if (length(missing_samples) > 0L) {
          issues[[length(issues) + 1L]] <- .new_validation_issue(
            level = "semantic",
            severity = rule$missing_severity,
            code = rule$missing_code,
            message = paste0(
              "Samples missing required relation `", relation, "`: ",
              paste(missing_samples, collapse = ", ")
            ),
            node_ids = missing_samples,
            details = list(edge_type = relation)
          )
        }
      }
    }

    allow_multiple <- identical(relation, "sample_belongs_to_subject") &&
      .depgraph_validation_override(graph, "allow_multi_subject_samples", default = FALSE)

    if (!allow_multiple && !is.null(rule$multiple_code) && !is.null(rule$multiple_severity) && is.finite(rule$max_targets)) {
      multi_samples <- names(counts)[counts > rule$max_targets]
      if (length(multi_samples) > 0L) {
        edge_ids <- relation_edges$edge_id[relation_edges$from %in% multi_samples]
        related_nodes <- unique(c(
          multi_samples,
          unlist(targets[multi_samples], use.names = FALSE)
        ))
        issues[[length(issues) + 1L]] <- .new_validation_issue(
          level = "semantic",
          severity = rule$multiple_severity,
          code = rule$multiple_code,
          message = paste0(
            "Samples violate relation cardinality for `", relation, "`: ",
            paste(multi_samples, collapse = ", ")
          ),
          node_ids = related_nodes,
          edge_ids = edge_ids,
          details = list(edge_type = relation)
        )
      }
    }
  }

  feature_nodes <- node_data[node_data$node_type == "FeatureSet", , drop = FALSE]
  if (nrow(feature_nodes) > 0L) {
    for (i in seq_len(nrow(feature_nodes))) {
      derivation_scope <- .depgraph_extract_attr(feature_nodes$attrs[[i]], "derivation_scope")
      if (!is.na(derivation_scope) && !derivation_scope %in% .depgraph_allowed_derivation_scopes) {
        issues[[length(issues) + 1L]] <- .new_validation_issue(
          level = "semantic",
          severity = "warning",
          code = "invalid_featureset_derivation_scope",
          message = paste0(
            "FeatureSet node `", feature_nodes$node_id[[i]], "` has invalid `derivation_scope`: ",
            derivation_scope
          ),
          node_ids = feature_nodes$node_id[[i]]
        )
      }
    }
  }

  outcome_nodes <- node_data[node_data$node_type == "Outcome", , drop = FALSE]
  if (nrow(outcome_nodes) > 0L) {
    for (i in seq_len(nrow(outcome_nodes))) {
      observation_level <- .depgraph_extract_attr(outcome_nodes$attrs[[i]], "observation_level")
      if (!is.na(observation_level) && !observation_level %in% .depgraph_allowed_observation_levels) {
        issues[[length(issues) + 1L]] <- .new_validation_issue(
          level = "semantic",
          severity = "warning",
          code = "invalid_outcome_observation_level",
          message = paste0(
            "Outcome node `", outcome_nodes$node_id[[i]], "` has invalid `observation_level`: ",
            observation_level
          ),
          node_ids = outcome_nodes$node_id[[i]]
        )
      }
    }
  }

  time_nodes <- node_data[node_data$node_type == "Timepoint", , drop = FALSE]
  if (nrow(time_nodes) > 0L) {
    for (i in seq_len(nrow(time_nodes))) {
      value <- .depgraph_extract_attr(time_nodes$attrs[[i]], "time_index")
      has_raw_value <- !is.null(value) && length(value) > 0L &&
        (!is.na(value[[1L]]) || is.nan(value[[1L]])) &&
        (!is.character(value[[1L]]) || nzchar(trimws(value[[1L]])))
      parsed <- .depgraph_parse_time_index(value)
      if (has_raw_value && (length(parsed) == 0L || is.na(parsed[[1L]]))) {
        issues[[length(issues) + 1L]] <- .new_validation_issue(
          level = "semantic",
          severity = "warning",
          code = "invalid_time_index",
          message = paste0("Timepoint node `", time_nodes$node_id[[i]], "` has a non-numeric or non-finite `time_index`."),
          node_ids = time_nodes$node_id[[i]]
        )
      }
    }
  }

  precedes <- edge_data[edge_data$edge_type == "timepoint_precedes", , drop = FALSE]
  if (nrow(precedes) > 0L && any(precedes$from == precedes$to)) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "semantic",
      severity = "error",
      code = "timepoint_self_loop",
      message = "`timepoint_precedes` contains self-loops.",
      edge_ids = precedes$edge_id[precedes$from == precedes$to]
    )
  }

  time_consistency <- .depgraph_time_order_consistency(node_data, edge_data)
  if (length(time_consistency$cycle_edge_ids) > 0L) {
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "semantic",
      severity = "error",
      code = "timepoint_precedence_cycle",
      message = "`timepoint_precedes` must define an acyclic ordering.",
      edge_ids = time_consistency$cycle_edge_ids
    )
  }
  if (nrow(time_consistency$conflicting_pairs) > 0L) {
    conflicting_nodes <- unique(c(
      time_consistency$conflicting_pairs$from,
      time_consistency$conflicting_pairs$to
    ))
    pair_labels <- paste0(
      time_consistency$conflicting_pairs$from,
      " -> ",
      time_consistency$conflicting_pairs$to
    )
    issues[[length(issues) + 1L]] <- .new_validation_issue(
      level = "semantic",
      severity = "error",
      code = "time_order_conflict",
      message = paste0(
        "Timepoint ordering metadata conflict between `time_index` and `timepoint_precedes`: ",
        paste(pair_labels, collapse = ", ")
      ),
      node_ids = conflicting_nodes,
      edge_ids = time_consistency$conflicting_pairs$edge_id
    )
  }

  .depgraph_bind_issues(issues)
}

# Parse one temporal attribute value and record its unit, so that values are
# only ever compared in the same unit (CLAUDE.md §3). Returns a list:
#   value: seconds since the epoch for "date" / "datetime", seconds for
#          "duration", the number as given for "numeric", NA when missing or
#          unparseable;
#   unit:  "date" (Date, "YYYY-MM-DD" string), "datetime" (POSIXct,
#          "YYYY-MM-DD[T ]HH:MM[:SS[.fff]]" string with optional Z / +-HH:MM
#          offset; without one it is taken as UTC), "duration" (difftime or
#          ISO 8601 duration string with fixed-length units W/D/H/M/S),
#          "numeric" (a bare number or numeric string, unit unknown),
#          "unparseable" (present but none of these), or NA when missing
#          (including non-finite values, which JSON writes as null);
#   day:   for "date" / "datetime", the calendar day as written (days since the
#          epoch, in the value's own time zone), used to compare a date with a
#          date-time at day resolution.
.depgraph_parse_temporal_scalar <- function(x) {
  missing_value <- list(value = NA_real_, unit = NA_character_, day = NA_real_)
  if (is.null(x) || length(x) == 0L) return(missing_value)
  if (inherits(x, "difftime")) {
    # `[[` drops the class (and so the unit) of a difftime.
    secs <- as.numeric(x[1L], units = "secs")
    if (!is.finite(secs)) return(missing_value)
    return(list(value = secs, unit = "duration", day = NA_real_))
  }
  val <- x[[1L]]
  if (is.factor(val)) val <- as.character(val)
  if (length(val) != 1L || is.na(val)) return(missing_value)
  if (inherits(val, "Date")) {
    # A Date can carry a fractional day (e.g. a mean of Dates); it means the
    # calendar date it prints as.
    d <- floor(as.numeric(val))
    if (!is.finite(d)) return(missing_value)
    return(list(value = d * 86400, unit = "date", day = d))
  }
  if (inherits(val, "POSIXt")) {
    if (!is.finite(as.numeric(as.POSIXct(val)))) return(missing_value)
    tz <- attr(val, "tzone")
    tz <- if (length(tz) && nzchar(tz[[1L]])) tz[[1L]] else ""
    day <- as.numeric(as.Date(format(val, "%Y-%m-%d", tz = tz)))
    return(list(value = as.numeric(as.POSIXct(val)), unit = "datetime", day = day))
  }
  unparseable <- list(value = NA_real_, unit = "unparseable", day = NA_real_)
  if (is.numeric(val)) {
    num <- as.numeric(val)
    if (!is.finite(num)) return(missing_value)
    return(list(value = num, unit = "numeric", day = NA_real_))
  }
  if (!is.character(val)) return(unparseable)
  s <- trimws(val)
  if (!nzchar(s)) return(missing_value)
  num <- suppressWarnings(as.numeric(s))
  if (!is.na(num)) {
    if (!is.finite(num)) return(missing_value)
    return(list(value = num, unit = "numeric", day = NA_real_))
  }
  s <- gsub("/", "-", s, fixed = TRUE)
  # ISO-8601 date-time with 'T' or space separator and optional 'Z' or
  # +/-HH:MM offset. Range-check hours (0..23), minutes (0..59), seconds
  # ([0, 60)), and offset hours/minutes so malformed values never silently wrap
  # or truncate in strptime.
  iso_m <- regexec(
    "^([0-9]{4}-[0-9]{1,2}-[0-9]{1,2})[T ]([0-9]{1,2}):([0-9]{2})(?::([0-9]{2}(?:\\.[0-9]+)?))?(Z|[+-][0-9]{2}:?[0-9]{2})?$",
    s, perl = TRUE
  )
  parts <- regmatches(s, iso_m)[[1L]]
  if (length(parts) >= 4L && nzchar(parts[2L]) && nzchar(parts[3L]) && nzchar(parts[4L])) {
    h_val <- as.numeric(parts[3L])
    m_val <- as.numeric(parts[4L])
    has_sec <- length(parts) >= 5L && nzchar(parts[5L])
    s_val <- if (has_sec) as.numeric(parts[5L]) else 0
    if (h_val <= 23 && m_val <= 59 && s_val < 60) {
      time_str <- if (has_sec) paste0(parts[3L], ":", parts[4L], ":", parts[5L]) else paste0(parts[3L], ":", parts[4L])
      dt_str <- paste(parts[2L], time_str)
      fmt <- if (has_sec) "%Y-%m-%d %H:%M:%OS" else "%Y-%m-%d %H:%M"
      ts <- suppressWarnings(as.POSIXct(dt_str, format = fmt, tz = "UTC"))
      if (!is.na(ts)) {
        sec <- as.numeric(ts)
        tz_part <- if (length(parts) >= 6L) parts[6L] else ""
        if (nzchar(tz_part) && !identical(tz_part, "Z")) {
          sgn <- if (startsWith(tz_part, "-")) -1 else 1
          tz_clean <- gsub("[^0-9]", "", tz_part)
          hh <- as.numeric(substr(tz_clean, 1L, 2L))
          mm <- as.numeric(substr(tz_clean, 3L, 4L))
          if (hh > 23 || mm > 59) return(unparseable)
          sec <- sec - sgn * (hh * 3600 + mm * 60)
        }
        return(list(value = sec, unit = "datetime", day = as.numeric(as.Date(parts[2L]))))
      }
    }
  }
  if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", s)) {
    dt <- tryCatch(as.Date(s, format = "%Y-%m-%d"), error = function(e) as.Date(NA))
    if (!is.na(dt)) {
      d <- as.numeric(dt)
      return(list(value = d * 86400, unit = "date", day = d))
    }
  }
  # ISO-8601 duration with fixed-length units (W, D, H, M, S; optional sign).
  # Years (Y) and months (M before T) are intentionally not parsed because
  # their length in seconds is not fixed.
  dur_w <- regexec("^([+-]?)P([0-9]+(?:\\.[0-9]+)?)W$", s, perl = TRUE)
  w_parts <- regmatches(s, dur_w)[[1L]]
  if (length(w_parts) == 3L && nzchar(w_parts[3L])) {
    sgn <- if (identical(w_parts[2L], "-")) -1 else 1
    sec <- sgn * as.numeric(w_parts[3L]) * 604800
    if (!is.finite(sec)) return(missing_value)
    return(list(value = sec, unit = "duration", day = NA_real_))
  }
  dur_m <- regexec(
    "^([+-]?)P(?:([0-9]+(?:\\.[0-9]+)?)D)?(?:T(?:([0-9]+(?:\\.[0-9]+)?)H)?(?:([0-9]+(?:\\.[0-9]+)?)M)?(?:([0-9]+(?:\\.[0-9]+)?)S)?)?$",
    s, perl = TRUE
  )
  d_parts <- regmatches(s, dur_m)[[1L]]
  if (length(d_parts) == 6L) {
    has_d <- nzchar(d_parts[3L])
    has_t_comp <- any(nzchar(d_parts[4L:6L]))
    has_t <- grepl("T", s, fixed = TRUE)
    if ((has_d || has_t_comp) && (!has_t || has_t_comp)) {
      sgn <- if (identical(d_parts[2L], "-")) -1 else 1
      val_or_zero <- function(z) if (nzchar(z)) as.numeric(z) else 0
      sec <- sgn * (
        val_or_zero(d_parts[3L]) * 86400 +
          val_or_zero(d_parts[4L]) * 3600 +
          val_or_zero(d_parts[5L]) * 60 +
          val_or_zero(d_parts[6L])
      )
      if (!is.finite(sec)) return(missing_value)
      return(list(value = sec, unit = "duration", day = NA_real_))
    }
  }
  unparseable
}

# Order two parsed temporal values: sign of (a - b), with how it was decided.
#   status "same":           same unit, compared exactly;
#   status "day_resolution": a date against a date-time, compared by calendar
#                            day (equal days give 0);
#   status "mismatch":       a bare number against a date / date-time, or an
#                            unparseable value against anything present; not
#                            comparable, sign is NA;
#   status "missing":        either side missing, sign is NA.
.depgraph_temporal_order <- function(a, b) {
  if (is.na(a$unit) || is.na(b$unit)) return(list(sign = NA_real_, status = "missing"))
  if ("unparseable" %in% c(a$unit, b$unit)) return(list(sign = NA_real_, status = "mismatch"))
  if (identical(a$unit, b$unit)) return(list(sign = sign(a$value - b$value), status = "same"))
  if (all(c(a$unit, b$unit) %in% c("date", "datetime"))) {
    return(list(sign = sign(a$day - b$day), status = "day_resolution"))
  }
  list(sign = NA_real_, status = "mismatch")
}

.depgraph_temporal_unit_label <- function(unit) {
  switch(unit, date = "a date", datetime = "a date-time", numeric = "a bare number", duration = "a time span (difftime)",
         unparseable = "text that is not a date, date-time or number", unit)
}

.validate_leakage <- function(graph) {
  node_data <- graph$nodes$data
  edge_data <- graph$edges$data
  issues <- list()

  subject_edges <- edge_data[edge_data$edge_type == "sample_belongs_to_subject", c("edge_id", "from", "to"), drop = FALSE]
  if (nrow(subject_edges) > 0L) {
    sample_counts <- .depgraph_count_unique_by(subject_edges$from, subject_edges$to)
    subject_ids <- sample_counts$by[sample_counts$n > 1L]
    if (length(subject_ids) > 0L) {
      samples_by_subject <- split(subject_edges$from, subject_edges$to)
      edges_by_subject <- split(subject_edges$edge_id, subject_edges$to)
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "leakage",
        severity = "advisory",
        code = "repeated_subject_samples",
        messages = paste0("Subject `", subject_ids, "` is linked to multiple samples."),
        node_ids = Map(function(s, samples) c(s, unique(samples)), subject_ids, samples_by_subject[subject_ids]),
        edge_ids = edges_by_subject[subject_ids]
      )
    }
  }

  study_edges <- edge_data[edge_data$edge_type == "sample_from_study", c("edge_id", "from", "to"), drop = FALSE]
  if (nrow(subject_edges) > 0L && nrow(study_edges) > 0L) {
    subject_study <- merge(
      subject_edges[, c("from", "to")],
      study_edges[, c("from", "to")],
      by = "from",
      suffixes = c("_subject", "_study")
    )
    if (nrow(subject_study) > 0L) {
      study_counts <- .depgraph_count_unique_by(subject_study$to_study, subject_study$to_subject)
      subject_ids <- study_counts$by[study_counts$n > 1L]
      if (length(subject_ids) > 0L) {
        samples_by_subject <- split(subject_study$from, subject_study$to_subject)
        issues[[length(issues) + 1L]] <- .new_validation_issues(
          level = "leakage",
          severity = "warning",
          code = "subject_cross_study_overlap",
          messages = paste0("Subject `", subject_ids, "` appears across multiple studies."),
          node_ids = Map(function(s, samples) c(s, unique(samples)), subject_ids, samples_by_subject[subject_ids])
        )
      }
    }
  }

  # A subject whose samples were collected at several sites: mirrors the
  # cross-study rule. Grouping by site alone would then place one individual
  # in several site groups, and a site holdout would leak that individual.
  site_edges <- edge_data[edge_data$edge_type == "sample_collected_at_site", c("edge_id", "from", "to"), drop = FALSE]
  if (nrow(subject_edges) > 0L && nrow(site_edges) > 0L) {
    subject_site <- merge(
      subject_edges[, c("from", "to")],
      site_edges[, c("from", "to")],
      by = "from",
      suffixes = c("_subject", "_site")
    )
    if (nrow(subject_site) > 0L) {
      site_counts <- .depgraph_count_unique_by(subject_site$to_site, subject_site$to_subject)
      subject_ids <- site_counts$by[site_counts$n > 1L]
      if (length(subject_ids) > 0L) {
        samples_by_subject <- split(subject_site$from, subject_site$to_subject)
        sites_by_subject <- split(subject_site$to_site, subject_site$to_subject)
        issues[[length(issues) + 1L]] <- .new_validation_issues(
          level = "leakage",
          severity = "warning",
          code = "subject_cross_site_overlap",
          messages = paste0("Subject `", subject_ids, "` has samples collected at multiple sites."),
          node_ids = Map(
            function(s, samples, sites) c(s, unique(samples), unique(sites)),
            subject_ids, samples_by_subject[subject_ids], sites_by_subject[subject_ids]
          )
        )
      }
    }
  }

  feature_nodes <- node_data[node_data$node_type == "FeatureSet", , drop = FALSE]
  if (nrow(feature_nodes) > 0L) {
    for (i in seq_len(nrow(feature_nodes))) {
      derivation_scope <- .depgraph_extract_attr(feature_nodes$attrs[[i]], "derivation_scope")
      if (!is.na(derivation_scope) && identical(derivation_scope, "per_dataset")) {
        issues[[length(issues) + 1L]] <- .new_validation_issue(
          level = "leakage",
          severity = "advisory",
          code = "per_dataset_featureset",
          message = paste0("FeatureSet `", feature_nodes$node_id[[i]], "` was derived at the full-dataset scope."),
          node_ids = feature_nodes$node_id[[i]]
        )
      }
    }
  }

  feature_edges <- edge_data[edge_data$edge_type == "sample_uses_featureset", c("edge_id", "from", "to"), drop = FALSE]
  if (nrow(feature_edges) > 0L) {
    feature_counts <- .depgraph_count_unique_by(feature_edges$from, feature_edges$to)
    fs_ids <- feature_counts$by[feature_counts$n > 1L]
    if (length(fs_ids) > 0L) {
      samples_by_fs <- split(feature_edges$from, feature_edges$to)
      edges_by_fs <- split(feature_edges$edge_id, feature_edges$to)
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "leakage",
        severity = "advisory",
        code = "shared_featureset_provenance",
        messages = paste0("FeatureSet `", fs_ids, "` is shared across multiple samples."),
        node_ids = Map(function(f, samples) c(f, unique(samples)), fs_ids, samples_by_fs[fs_ids]),
        edge_ids = edges_by_fs[fs_ids]
      )
    }
  }

  has_time_edges <- any(edge_data$edge_type == "sample_collected_at_timepoint")
  if (has_time_edges) {
    has_precedes <- any(edge_data$edge_type == "timepoint_precedes")
    time_nodes <- node_data[node_data$node_type == "Timepoint", , drop = FALSE]
    time_indices <- vapply(
      time_nodes$attrs,
      function(x) {
        value <- .depgraph_extract_attr(x, "time_index")
        parsed <- .depgraph_parse_time_index(value)
        if (length(parsed) == 0L) NA_real_ else parsed[[1L]]
      },
      FUN.VALUE = numeric(1)
    )
    if (!has_precedes && (length(time_indices) == 0L || all(is.na(time_indices)))) {
      issues[[length(issues) + 1L]] <- .new_validation_issue(
        level = "leakage",
        severity = "warning",
        code = "missing_time_ordering",
        message = "Timepoint-linked samples exist but no explicit time ordering metadata were found."
      )
    }
  }

  batch_edges <- edge_data[edge_data$edge_type == "sample_processed_in_batch", c("edge_id", "from", "to"), drop = FALSE]
  n_samples <- sum(node_data$node_type == "Sample")
  if (nrow(batch_edges) > 0L && n_samples > 0L) {
    batch_counts <- .depgraph_count_unique_by(batch_edges$from, batch_edges$to)
    threshold <- max(3L, ceiling(n_samples * 0.5))
    batch_ids <- batch_counts$by[batch_counts$n >= threshold]
    if (length(batch_ids) > 0L) {
      edges_by_batch <- split(batch_edges$edge_id, batch_edges$to)
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "leakage",
        severity = "advisory",
        code = "heavy_batch_reuse",
        messages = paste0("Batch `", batch_ids, "` is reused across many samples."),
        node_ids = as.list(batch_ids),
        edge_ids = edges_by_batch[batch_ids]
      )
    }
  }

  # FeatureSet provenance contamination. Leak condition: a FeatureSet derived
  # from a source Study/Batch is used both by samples of that source (which
  # are in this dataset and can be evaluated) and by samples outside it. A FeatureSet from a source with no samples here (external
  # knowledge, e.g. a published signature) is not flagged. In-sample use only
  # (derived from and used on the same source) is covered by
  # `shared_featureset_provenance`.
  fs_study_edges <- edge_data[edge_data$edge_type == "featureset_generated_from_study", c("edge_id", "from", "to"), drop = FALSE]
  if (nrow(fs_study_edges) > 0L && nrow(feature_edges) > 0L) {
    sample_study_map <- if (nrow(study_edges) > 0L) {
      split(study_edges$to, study_edges$from)
    } else {
      list()
    }
    study_sample_map <- if (nrow(study_edges) > 0L) {
      split(study_edges$from, study_edges$to)
    } else {
      list()
    }
    for (f_id in unique(fs_study_edges$from)) {
      src_studies <- unique(fs_study_edges$to[fs_study_edges$from == f_id])
      u_samples <- unique(feature_edges$from[feature_edges$to == f_id])
      if (length(u_samples) == 0L) next
      active_src <- src_studies[vapply(src_studies, function(st) {
        length(intersect(u_samples, study_sample_map[[st]] %||% character())) > 0L
      }, logical(1))]
      if (length(active_src) == 0L) next
      # Leak: the FeatureSet features samples of at least one of its source
      # studies (which can be evaluated) and is also used by samples outside
      # that source study (including when pooled across multiple in-data
      # studies). Used only outside its sources, or only within a single source
      # study, is not flagged here.
      outside_samples <- u_samples[vapply(u_samples, function(s) {
        s_st <- sample_study_map[[s]] %||% character()
        any(!active_src %in% s_st)
      }, logical(1))]
      src_samples <- unique(unlist(study_sample_map[active_src], use.names = FALSE))
      if (length(outside_samples) > 0L) {
        aff_nodes <- unique(c(f_id, src_studies, src_samples, u_samples))
        aff_edges <- unique(c(
          fs_study_edges$edge_id[fs_study_edges$from == f_id],
          feature_edges$edge_id[feature_edges$to == f_id & feature_edges$from %in% outside_samples]
        ))
        issues[[length(issues) + 1L]] <- .new_validation_issue(
          level = "leakage",
          severity = "warning",
          code = "cross_study_featureset_contamination",
          message = paste0(
            "FeatureSet `", f_id, "` generated from ",
            paste(src_studies, collapse = ", "),
            " is used by sample(s) outside its source study (",
            paste(outside_samples, collapse = ", "),
            ") while the source study's own samples are also in the data (",
            length(src_samples), " sample(s)); if the FeatureSet was derived using those ",
            "samples, evaluating them is not independent of how features were chosen."
          ),
          node_ids = aff_nodes,
          edge_ids = aff_edges
        )
      }
    }
  }

  fs_batch_edges <- edge_data[edge_data$edge_type == "featureset_generated_from_batch", c("edge_id", "from", "to"), drop = FALSE]
  if (nrow(fs_batch_edges) > 0L && nrow(feature_edges) > 0L) {
    sample_batch_map <- if (nrow(batch_edges) > 0L) {
      split(batch_edges$to, batch_edges$from)
    } else {
      list()
    }
    batch_sample_map <- if (nrow(batch_edges) > 0L) {
      split(batch_edges$from, batch_edges$to)
    } else {
      list()
    }
    for (f_id in unique(fs_batch_edges$from)) {
      src_batches <- unique(fs_batch_edges$to[fs_batch_edges$from == f_id])
      u_samples <- unique(feature_edges$from[feature_edges$to == f_id])
      if (length(u_samples) == 0L) next
      active_src <- src_batches[vapply(src_batches, function(bt) {
        length(intersect(u_samples, batch_sample_map[[bt]] %||% character())) > 0L
      }, logical(1))]
      if (length(active_src) == 0L) next
      # Leak: the FeatureSet features samples of at least one of its source
      # batches (which can be evaluated) and is also used by samples outside
      # that source batch (including when pooled across multiple in-data
      # batches). Used only outside its sources, or only within a single source
      # batch, is not flagged here.
      outside_samples <- u_samples[vapply(u_samples, function(s) {
        s_bt <- sample_batch_map[[s]] %||% character()
        any(!active_src %in% s_bt)
      }, logical(1))]
      src_samples <- unique(unlist(batch_sample_map[active_src], use.names = FALSE))
      if (length(outside_samples) > 0L) {
        aff_nodes <- unique(c(f_id, src_batches, src_samples, u_samples))
        aff_edges <- unique(c(
          fs_batch_edges$edge_id[fs_batch_edges$from == f_id],
          feature_edges$edge_id[feature_edges$to == f_id & feature_edges$from %in% outside_samples]
        ))
        issues[[length(issues) + 1L]] <- .new_validation_issue(
          level = "leakage",
          severity = "warning",
          code = "cross_batch_featureset_contamination",
          message = paste0(
            "FeatureSet `", f_id, "` generated from ",
            paste(src_batches, collapse = ", "),
            " is used by sample(s) outside its source batch (",
            paste(outside_samples, collapse = ", "),
            ") while the source batch's own samples are also in the data (",
            length(src_samples), " sample(s)); if the FeatureSet was derived using those ",
            "samples, evaluating them is not independent of how features were chosen."
          ),
          node_ids = aff_nodes,
          edge_ids = aff_edges
        )
      }
    }
  }

  # Clinical timeline checks against each sample's index date (t0). Leak
  # conditions and limits are documented in ?build_dependency_graph
  # ("Timeline rules"). Timestamps are compared only in the same unit
  # (`.depgraph_temporal_order()`); pairs in different units are reported as
  # `temporal_unit_mismatch` instead of being compared.
  sample_rows <- node_data[node_data$node_type == "Sample", , drop = FALSE]
  if (nrow(sample_rows) > 0L) {
    # First present (non-missing) attribute among `keys`, with its label. An
    # unparseable value counts as present, so it is reported, not skipped.
    first_time <- function(attrs, keys, node_type, node_id = NULL) {
      for (k in keys) {
        # The raw attribute, not .depgraph_extract_attr(): its `[[1L]]` drops
        # the class of a difftime.
        p <- .depgraph_parse_temporal_scalar(attrs[[k]])
        if (!is.na(p$unit)) {
          p$attr <- k
          p$node_type <- node_type
          p$node_id <- node_id
          p$label <- if (is.null(node_id)) {
            paste0("`", k, "`")
          } else {
            paste0("`", k, "` of ", node_type, " `", node_id, "`")
          }
          return(p)
        }
      }
      NULL
    }
    times_by_node <- function(rows, keys, node_type) {
      out <- lapply(seq_len(nrow(rows)), function(i) {
        first_time(rows$attrs[[i]], keys, node_type, rows$node_id[[i]])
      })
      names(out) <- rows$node_id
      Filter(Negate(is.null), out)
    }
    fs_time <- times_by_node(feature_nodes, c("feature_window_end", "collection_time"), "FeatureSet")
    outcome_nodes <- node_data[node_data$node_type == "Outcome", , drop = FALSE]
    out_time <- times_by_node(outcome_nodes, c("outcome_time", "event_time"), "Outcome")

    # Hashed lookups: `named_list[[id]]` scans the names, which made this
    # per-sample loop quadratic on large cohorts.
    as_lookup <- function(x) list2env(as.list(x), parent = emptyenv(), hash = TRUE)
    get_ids <- function(env, ids) {
      unlist(lapply(ids, function(id) get0(id, envir = env, inherits = FALSE)), use.names = FALSE)
    }
    fs_time_env <- as_lookup(fs_time)
    out_time_env <- as_lookup(out_time)
    out_edges <- edge_data[edge_data$edge_type == "sample_has_outcome", c("edge_id", "from", "to"), drop = FALSE]
    subj_out_edges <- edge_data[edge_data$edge_type == "subject_has_outcome", c("edge_id", "from", "to"), drop = FALSE]
    sample_fs_map <- as_lookup(split(feature_edges$to, feature_edges$from))
    sample_out_map <- as_lookup(split(out_edges$to, out_edges$from))
    sample_subj_map <- as_lookup(split(subject_edges$to, subject_edges$from))
    subj_out_map <- as_lookup(split(subj_out_edges$to, subj_out_edges$from))

    # One slot per sample, filled in place: growing these vectors with c()
    # was quadratic in the number of flagged samples.
    n_samples <- nrow(sample_rows)
    postdate_msgs <- rep(NA_character_, n_samples)
    postdate_nodes <- vector("list", n_samples)
    order_msgs <- rep(NA_character_, n_samples)
    order_nodes <- vector("list", n_samples)
    # Pairs not compared exactly (different units, or day resolution). Each is
    # keyed by (status, attribute, unit, attribute, unit); the key's
    # description lives in `pair_meta`, and each sample's hits are collected in
    # per-sample slots and combined once after the loop, so the bookkeeping
    # stays linear in the number of samples.
    pair_meta <- new.env(parent = emptyenv())
    hit_keys <- vector("list", nrow(sample_rows))
    hit_nodes <- vector("list", nrow(sample_rows))
    pair_key <- function(a, b, status, code) {
      if (!status %in% c("mismatch", "day_resolution")) return(NULL)
      ga <- if (is.null(a$node_id)) paste0("Sample `", a$attr, "`") else paste0(a$node_type, " `", a$attr, "`")
      gb <- if (is.null(b$node_id)) paste0("Sample `", b$attr, "`") else paste0(b$node_type, " `", b$attr, "`")
      key <- paste(status, ga, a$unit, gb, b$unit, sep = "\r")
      m <- pair_meta[[key]] %||% list(
        status = status, a = ga, a_unit = a$unit, b = gb, b_unit = b$unit, codes = character()
      )
      if (!code %in% m$codes) {
        m$codes <- c(m$codes, code)
        assign(key, m, envir = pair_meta)
      }
      key
    }

    for (i in seq_len(nrow(sample_rows))) {
      sid <- sample_rows$node_id[[i]]
      s_attrs <- sample_rows$attrs[[i]]
      keys_i <- character()
      nodes_i <- character()
      note_pair <- function(x, y, status, code) {
        k <- pair_key(x, y, status, code)
        if (!is.null(k)) {
          keys_i <<- c(keys_i, k)
          other <- c(x$node_id, y$node_id)
          if (length(other) > 0L) nodes_i <<- c(nodes_i, paste0(k, "\n", other))
        }
      }
      t_idx <- first_time(s_attrs, "index_time", "Sample")
      t_col <- first_time(s_attrs, "collection_time", "Sample")
      linked_fs <- unique(get_ids(sample_fs_map, sid))
      fs_refs <- lapply(linked_fs, function(f) get0(f, envir = fs_time_env, inherits = FALSE))
      fs_refs <- Filter(Negate(is.null), fs_refs)
      predictor_times <- c(if (!is.null(t_col)) list(t_col), fs_refs)

      # Rule 1 (feature_postdates_index_time): a predictor timestamp (sample
      # collection_time, or the window end of a linked FeatureSet) is later
      # than the sample's index_time. Equal timestamps are allowed.
      if (!is.null(t_idx) && length(predictor_times) > 0L) {
        late <- character()
        late_nodes <- character()
        for (r in predictor_times) {
          o <- .depgraph_temporal_order(r, t_idx)
          note_pair(r, t_idx, o$status, "feature_postdates_index_time")
          if (isTRUE(o$sign > 0)) {
            late <- c(late, r$label)
            late_nodes <- c(late_nodes, r$node_id)
          }
        }
        if (length(late) > 0L) {
          postdate_msgs[[i]] <- paste0(
            "Sample `", sid, "`: ", paste(late, collapse = ", "),
            if (length(late) == 1L) " is" else " are",
            " later than `index_time` (predictor data recorded after the index date; look-ahead)."
          )
          postdate_nodes[[i]] <- unique(c(sid, late_nodes))
        }
      }

      # Rule 2 (outcome_precedes_index_time): the outcome timestamp (the
      # sample's own outcome_time / event_time, else those of its linked
      # Outcome nodes) is earlier than the index_time or a predictor timestamp.
      t_own <- first_time(s_attrs, c("outcome_time", "event_time"), "Sample")
      outcome_times <- if (!is.null(t_own)) {
        list(t_own)
      } else if (length(out_time) > 0L) {
        linked_out <- unique(c(
          get_ids(sample_out_map, sid),
          get_ids(subj_out_map, get_ids(sample_subj_map, sid))
        ))
        Filter(Negate(is.null), lapply(linked_out, function(o) get0(o, envir = out_time_env, inherits = FALSE)))
      } else {
        list()
      }
      ref_times <- c(if (!is.null(t_idx)) list(t_idx), predictor_times)
      if (length(outcome_times) > 0L && length(ref_times) > 0L) {
        early <- character()
        early_nodes <- character()
        for (ot in outcome_times) {
          before <- character()
          for (r in ref_times) {
            o <- .depgraph_temporal_order(ot, r)
            note_pair(ot, r, o$status, "outcome_precedes_index_time")
            if (isTRUE(o$sign < 0)) {
              before <- c(before, r$label)
              early_nodes <- c(early_nodes, ot$node_id)
            }
          }
          if (length(before) > 0L) {
            early <- c(early, paste0(ot$label, " is earlier than ", paste(before, collapse = ", ")))
          }
        }
        if (length(early) > 0L) {
          order_msgs[[i]] <- paste0(
            "Sample `", sid, "`: ", paste(early, collapse = "; "),
            " (outcome dated before the index date or the predictor data; timestamp ordering violation)."
          )
          order_nodes[[i]] <- unique(c(sid, early_nodes))
        }
      }
      if (length(keys_i) > 0L) {
        hit_keys[[i]] <- unique(keys_i)
        hit_nodes[[i]] <- unique(nodes_i)
      }
    }

    hit <- !is.na(postdate_msgs)
    if (any(hit)) {
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "leakage",
        severity = "warning",
        code = "feature_postdates_index_time",
        messages = postdate_msgs[hit],
        node_ids = postdate_nodes[hit]
      )
    }
    hit <- !is.na(order_msgs)
    if (any(hit)) {
      issues[[length(issues) + 1L]] <- .new_validation_issues(
        level = "leakage",
        severity = "warning",
        code = "outcome_precedes_index_time",
        messages = order_msgs[hit],
        node_ids = order_nodes[hit]
      )
    }

    all_keys <- unlist(hit_keys, use.names = FALSE)
    if (length(all_keys) > 0L) {
      show_ids <- function(ids) {
        shown <- paste(utils::head(ids, 10L), collapse = ", ")
        if (length(ids) > 10L) paste0(shown, ", and ", length(ids) - 10L, " more") else shown
      }
      key_samples <- split(rep(sample_rows$node_id, lengths(hit_keys)), all_keys)
      extra <- unlist(hit_nodes, use.names = FALSE)
      extra_key <- sub("\n.*$", "", extra)
      extra_node <- sub("^[^\n]*\n", "", extra)
      key_extra <- split(extra_node, factor(extra_key, levels = names(key_samples)))
      unit_pairs <- lapply(sort(names(key_samples)), function(k) {
        e <- pair_meta[[k]]
        e$samples <- unique(key_samples[[k]])
        e$nodes <- unique(c(e$samples, key_extra[[k]]))
        e
      })
      mism <- Filter(function(e) identical(e$status, "mismatch"), unit_pairs)
      coarse <- Filter(function(e) identical(e$status, "day_resolution"), unit_pairs)
      if (length(mism) > 0L) {
        issues[[length(issues) + 1L]] <- .new_validation_issues(
          level = "leakage",
          severity = "warning",
          code = "temporal_unit_mismatch",
          messages = vapply(mism, function(e) {
            paste0(
              "Timestamps in different units were not compared: ", e$a, " (",
              .depgraph_temporal_unit_label(e$a_unit), ") vs ", e$b, " (",
              .depgraph_temporal_unit_label(e$b_unit), ") for ", length(e$samples),
              " sample(s) (", show_ids(e$samples), "), so ",
              paste(e$codes, collapse = " and "), " was not checked for these pairs. ",
              "A bare number has no calendar unit: record both as dates or date-times, ",
              "or both as numbers in the same unit."
            )
          }, character(1)),
          node_ids = lapply(mism, `[[`, "nodes")
        )
      }
      if (length(coarse) > 0L) {
        issues[[length(issues) + 1L]] <- .new_validation_issues(
          level = "leakage",
          severity = "advisory",
          code = "temporal_resolution_mismatch",
          messages = vapply(coarse, function(e) {
            paste0(
              e$a, " (", .depgraph_temporal_unit_label(e$a_unit), ") and ", e$b, " (",
              .depgraph_temporal_unit_label(e$b_unit), ") were compared at day resolution for ",
              length(e$samples), " sample(s) (", show_ids(e$samples), "): values on the same ",
              "calendar day are not counted as earlier or later."
            )
          }, character(1)),
          node_ids = lapply(coarse, `[[`, "nodes")
        )
      }
    }
  }

  .depgraph_bind_issues(issues)
}

#' @rdname build_dependency_graph
#' @export
validate_graph <- function(graph, error_on_fail = FALSE, levels = NULL, severities = NULL, validation_overrides = NULL) {
  .depgraph_assert(inherits(graph, "dependency_graph"), "`graph` must be a `dependency_graph`.")

  if (!is.null(validation_overrides)) {
    .depgraph_assert(
      is.list(validation_overrides) && (length(validation_overrides) == 0L ||
        (!is.null(names(validation_overrides)) &&
           all(nzchar(names(validation_overrides))))),
      "`validation_overrides` must be a named list."
    )
    graph <- .depgraph_with_overrides(graph, validation_overrides)
  }

  selected_levels <- if (is.null(levels)) .depgraph_validation_levels else unique(as.character(levels))
  .depgraph_assert(all(selected_levels %in% .depgraph_validation_levels), "`levels` contains unsupported values.")

  selected_severities <- if (is.null(severities)) .depgraph_validation_severities else unique(as.character(severities))
  .depgraph_assert(all(selected_severities %in% .depgraph_validation_severities), "`severities` contains unsupported values.")

  issue_sets <- list()
  if ("structural" %in% selected_levels) {
    issue_sets[[length(issue_sets) + 1L]] <- .validate_structural(graph)
  }
  if ("semantic" %in% selected_levels) {
    issue_sets[[length(issue_sets) + 1L]] <- .validate_semantic(graph)
  }
  if ("leakage" %in% selected_levels) {
    issue_sets[[length(issue_sets) + 1L]] <- .validate_leakage(graph)
  }

  all_issues <- .depgraph_bind_issues(issue_sets)
  issues <- all_issues
  if (nrow(issues) > 0L) {
    issues <- issues[issues$severity %in% selected_severities, , drop = FALSE]
    row.names(issues) <- NULL
  }

  all_errors <- unique(all_issues$message[all_issues$severity == "error"])
  all_warnings <- unique(all_issues$message[all_issues$severity == "warning"])
  all_advisories <- unique(all_issues$message[all_issues$severity == "advisory"])

  metrics <- list(
    n_nodes = nrow(graph$nodes$data),
    n_edges = nrow(graph$edges$data),
    n_samples = sum(graph$nodes$data$node_type == "Sample"),
    n_issues = nrow(all_issues),
    n_errors = length(all_errors),
    n_warnings = length(all_warnings),
    n_advisories = length(all_advisories)
  )

  report <- depgraph_validation_report(
    graph_name = graph$metadata$graph_name,
    issues = issues,
    metrics = metrics,
    metadata = list(
      levels = selected_levels,
      severities = selected_severities
    ),
    valid = length(all_errors) == 0L,
    errors = all_errors,
    warnings = all_warnings,
    advisories = all_advisories
  )

  if (length(all_errors) > 0L && isTRUE(error_on_fail)) {
    .depgraph_stop(
      paste(c("Graph validation failed.", report$errors), collapse = "\n"),
      class = "splitgraph_validation_error",
      code = "graph_validation_failed"
    )
  }

  report
}
