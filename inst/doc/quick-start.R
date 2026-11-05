## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----load---------------------------------------------------------------------
library(splitGraph)

## ----meta---------------------------------------------------------------------
meta <- data.frame(
  sample_id    = paste0("S", 1:8),
  subject_id   = c("P1", "P1", "P2", "P2", "P3", "P3", "P4", "P4"),
  batch_id     = c("B1", "B1", "B2", "B2", "B3", "B3", "B3", "B4"),
  site_id      = c("NYC", "NYC", "NYC", "NYC", "BOS", "BOS", "BOS", "BOS"),
  timepoint_id = rep(c("T0", "T1"), 4),
  time_index   = rep(c(0, 1), 4),
  outcome_id   = c("case", "case", "ctrl", "ctrl", "case", "case", "ctrl", "ctrl"),
  stringsAsFactors = FALSE
)

## ----build--------------------------------------------------------------------
g <- graph_from_metadata(meta, graph_name = "quick-start")
g
validate_graph(g)

## ----derive-subject-----------------------------------------------------------
subject_constraint <- derive_split_constraints(g, mode = "subject")
subject_constraint
grouping_vector(subject_constraint)

## ----constraint-table---------------------------------------------------------
head(as.data.frame(subject_constraint)[, c("sample_id", "group_id", "explanation")], 2)

## ----derive-composite---------------------------------------------------------
composite_constraint <- derive_split_constraints(
  g, mode = "composite", via = c("subject", "batch")
)
grouping_vector(composite_constraint)

## ----spec---------------------------------------------------------------------
spec <- as_split_spec(subject_constraint, graph = g)
spec

## ----spec-roles---------------------------------------------------------------
spec$group_var    # the split unit
spec$block_vars   # coarser axes that ideally should not straddle a fold
spec$time_var     # ordering, when the graph carries one
spec$stratum_var  # the outcome level each sample has

## ----spec-table---------------------------------------------------------------
as.data.frame(spec)[, c("sample_id", "group_id", "batch_group", "site_group",
                        "stratum", "order_rank")]

## ----spec-validate------------------------------------------------------------
validate_split_spec(spec)

## ----risks--------------------------------------------------------------------
risks <- summarize_leakage_risks(g, constraint = subject_constraint, split_spec = spec)
risks
unique(as.data.frame(risks)[, c("category", "severity", "severed")])

## ----write, eval = requireNamespace("jsonlite", quietly = TRUE)---------------
path <- tempfile(fileext = ".json")
write_split_spec(spec, path)

validate_split_spec_json(path)$valid

# Reading with validate = TRUE re-checks the file against the schema and runs
# the preflight validator, instead of trusting whatever is on disk.
back <- read_split_spec(path, validate = TRUE)
identical(back$sample_data$group_id, spec$sample_data$group_id)

## ----bioleak, eval = FALSE----------------------------------------------------
# bioLeak::as_leaksplits(spec, data = my_frame, outcome = "y")

## ----bioleak-workaround, eval = FALSE-----------------------------------------
# joined <- merge(my_frame, spec$sample_data[, c("sample_id", "group_id")],
#                 by = "sample_id")
# bioLeak::make_split_plan(joined, outcome = "y",
#                          mode = "subject_grouped", group = "group_id")

## ----cleanup, include = FALSE-------------------------------------------------
if (exists("path")) unlink(path)

