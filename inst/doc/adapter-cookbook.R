## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = TRUE
)

package_root <- if (file.exists("../DESCRIPTION")) ".." else "."
if (requireNamespace("pkgload", quietly = TRUE) &&
    file.exists(file.path(package_root, "DESCRIPTION"))) {
  pkgload::load_all(package_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else {
  library(splitGraph)
}

## ----build-spec---------------------------------------------------------------
meta <- data.frame(
  sample_id    = c("S1", "S2", "S3", "S4", "S5", "S6"),
  subject_id   = c("P1", "P1", "P2", "P2", "P3", "P3"),
  batch_id     = c("B1", "B2", "B1", "B2", "B1", "B2"),
  timepoint_id = c("T0", "T1", "T0", "T1", "T0", "T1"),
  time_index   = c(0, 1, 0, 1, 0, 1),
  outcome_id   = c("ctrl", "case", "ctrl", "case", "case", "ctrl"),
  stringsAsFactors = FALSE
)

g <- graph_from_metadata(meta, graph_name = "cookbook")
subject_constraint <- derive_split_constraints(g, mode = "subject")
spec <- as_split_spec(subject_constraint, graph = g)
spec

## ----show-sample-data---------------------------------------------------------
as.data.frame(spec)[, c("sample_id", "group_id", "batch_group", "order_rank")]

## ----adapter-base-r-----------------------------------------------------------
logo_folds <- function(spec, observation_data, sample_id_col = "sample_id") {
  stopifnot(inherits(spec, "split_spec"))
  if (!sample_id_col %in% names(observation_data)) {
    stop("`observation_data` must contain a `", sample_id_col, "` column.")
  }

  joined <- merge(
    observation_data,
    spec$sample_data[, c("sample_id", spec$group_var)],
    by.x = sample_id_col, by.y = "sample_id", sort = FALSE
  )
  joined$.row <- seq_len(nrow(joined))
  groups <- split(joined$.row, joined[[spec$group_var]])

  lapply(names(groups), function(g) {
    list(
      group   = g,
      train   = unlist(groups[setdiff(names(groups), g)], use.names = FALSE),
      assess  = groups[[g]]
    )
  })
}

# Pretend we have an observation frame keyed by sample_id.
set.seed(1)
obs <- data.frame(
  sample_id = meta$sample_id,
  x = rnorm(nrow(meta)),
  y = rbinom(nrow(meta), 1, 0.5)
)

folds <- logo_folds(spec, obs)
length(folds)
folds[[1]]

## ----block-vars---------------------------------------------------------------
spec$block_vars
head(spec$sample_data[, c("sample_id", spec$group_var, spec$block_vars)])

## ----block-audit--------------------------------------------------------------
block <- spec$block_vars[[1]]
block_of <- setNames(spec$sample_data[[block]], spec$sample_data$sample_id)

do.call(rbind, lapply(folds, function(f) {
  data.frame(
    held_out_group    = f$group,
    straddling_batches = paste(
      intersect(block_of[obs$sample_id[f$train]],
                block_of[obs$sample_id[f$assess]]),
      collapse = ", "
    )
  )
}))

## ----stratum------------------------------------------------------------------
spec$stratum_var
spec$sample_data[, c("sample_id", spec$group_var, spec$stratum_var)]

## ----stratum-constant---------------------------------------------------------
tapply(spec$sample_data$stratum, spec$sample_data$group_id,
       function(x) length(unique(x)) == 1L)

## ----stratum-rsample, eval = requireNamespace("rsample", quietly = TRUE)------
joined_s <- merge(obs, spec$sample_data[, c("sample_id", "group_id", "stratum")],
                  by = "sample_id", sort = FALSE)

tryCatch(
  rsample::group_vfold_cv(joined_s, group = "group_id", v = 3, strata = "stratum"),
  error = function(e) conditionMessage(e)
)

## ----adapter-rsample-group, eval = requireNamespace("rsample", quietly = TRUE)----
spec_to_group_vfold <- function(spec, observation_data,
                                v = NULL,
                                sample_id_col = "sample_id") {
  stopifnot(inherits(spec, "split_spec"))
  if (!requireNamespace("rsample", quietly = TRUE)) {
    stop("Install rsample to use this adapter.")
  }

  joined <- merge(
    observation_data,
    spec$sample_data[, c("sample_id", spec$group_var)],
    by.x = sample_id_col, by.y = "sample_id", sort = FALSE
  )

  n_groups <- length(unique(joined[[spec$group_var]]))
  if (is.null(v)) v <- n_groups

  rsample::group_vfold_cv(
    data  = joined,
    group = !!spec$group_var,
    v     = v
  )
}

## ----adapter-rsample-group-run, eval = requireNamespace("rsample", quietly = TRUE)----
grouped <- spec_to_group_vfold(spec, obs)
grouped
# Every assessment set is exactly one subject's samples:
vapply(grouped$splits, function(s) {
  paste(sort(unique(rsample::assessment(s)$group_id)), collapse = ", ")
}, character(1))

## ----adapter-rsample-rolling, eval = requireNamespace("rsample", quietly = TRUE)----
spec_to_rolling_origin <- function(spec, observation_data,
                                   sample_id_col = "sample_id",
                                   initial = NULL,
                                   assess = 1L) {
  stopifnot(inherits(spec, "split_spec"))
  if (is.null(spec$time_var)) {
    stop("This split_spec has no `time_var`; ordered evaluation is not available.")
  }
  if (!requireNamespace("rsample", quietly = TRUE)) {
    stop("Install rsample to use this adapter.")
  }

  joined <- merge(
    observation_data,
    spec$sample_data[, c("sample_id", spec$time_var)],
    by.x = sample_id_col, by.y = "sample_id", sort = FALSE
  )
  ordered <- joined[order(joined[[spec$time_var]]), , drop = FALSE]

  if (is.null(initial)) initial <- max(1L, floor(nrow(ordered) * 0.6))
  rsample::rolling_origin(ordered, initial = initial, assess = assess)
}

## ----adapter-rsample-rolling-run, eval = requireNamespace("rsample", quietly = TRUE)----
rolling <- spec_to_rolling_origin(spec, obs, initial = 3, assess = 1)
rolling
# No analysis sample comes after any assessment sample:
vapply(rolling$splits, function(s) {
  max(rsample::analysis(s)$order_rank) <= min(rsample::assessment(s)$order_rank)
}, logical(1))

## ----rolling-ties, eval = requireNamespace("rsample", quietly = TRUE)---------
length(unique(spec$sample_data$order_rank))   # distinct ranks
nrow(spec$sample_data)                        # rows

vapply(rolling$splits, function(s) {
  shared <- intersect(rsample::analysis(s)$order_rank,
                      rsample::assessment(s)$order_rank)
  paste(shared, collapse = ", ")
}, character(1))

## ----sliding-window, eval = requireNamespace("rsample", quietly = TRUE)-------
joined  <- merge(obs, spec$sample_data[, c("sample_id", spec$time_var)],
                 by = "sample_id", sort = FALSE)
ordered <- joined[order(joined[[spec$time_var]]), , drop = FALSE]

sliding <- rsample::sliding_window(
  ordered,
  lookback     = Inf,   # cumulative analysis window, like rolling_origin()
  assess_stop  = 1,
  complete     = FALSE,
  skip         = 2      # start where `initial = 3` did
)

identical(
  lapply(sliding$splits, function(s) rsample::assessment(s)$sample_id),
  lapply(rolling$splits, function(s) rsample::assessment(s)$sample_id)
)

## ----serialize, eval = requireNamespace("jsonlite", quietly = TRUE)-----------
tmp <- tempfile(fileext = ".json")
write_split_spec(spec, tmp)

# The file opens with its $schema reference and schema_version.
cat(readLines(tmp, n = 5), sep = "\n")

# Validate the file against the shipped JSON Schema, then read it back exactly.
validate_split_spec_json(tmp)$valid
spec2 <- read_split_spec(tmp)
identical(spec$sample_data$group_id, spec2$sample_data$group_id)

unlink(tmp)

## ----xlang-pointer, eval = FALSE----------------------------------------------
# vignette("cross-language-handoff", package = "splitGraph")

## ----dispatch-----------------------------------------------------------------
recommend_adapter <- function(spec) {
  switch(
    spec$recommended_resampling,
    grouped_cv          = "group_vfold_cv (group = group_id)",
    blocked_cv          = "group_vfold_cv (group = group_id)",
    custom_grouped_cv   = "group_vfold_cv (group = group_id)",
    leave_one_group_out = "leave-one-group-out over group_id",
    ordered_split       = "rolling_origin (order by order_rank)",
    "group_vfold_cv (default)"
  )
}

# The subject spec recommends grouped CV; a time-mode spec recommends ordering.
recommend_adapter(spec)
time_spec <- as_split_spec(derive_split_constraints(g, mode = "time"), graph = g)
recommend_adapter(time_spec)

## ----dispatch-exhaustive------------------------------------------------------
# One graph carrying every direct relation, so each mode has something to group.
g_all <- graph_from_metadata(transform(
  meta,
  site_id     = c("N", "N", "N", "B", "B", "B"),
  region_id   = "ctx",
  platform_id = "il",
  assay_id    = "rna"
))

modes <- c("subject", "batch", "study", "time",
           "site", "region", "platform", "assay")
vapply(modes, function(m) {
  as_split_spec(derive_split_constraints(g_all, mode = m),
                graph = g_all)$recommended_resampling
}, character(1))

# And the two composite strategies, which supply the fifth value.
c(
  strict = as_split_spec(derive_split_constraints(
    g_all, mode = "composite", strategy = "strict",
    via = c("Subject", "Batch")), graph = g_all)$recommended_resampling,
  rule_based = as_split_spec(derive_split_constraints(
    g_all, mode = "composite", strategy = "rule_based",
    priority = c("subject", "batch")), graph = g_all)$recommended_resampling
)

