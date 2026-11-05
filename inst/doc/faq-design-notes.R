## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(splitGraph)

## ----overmerge----------------------------------------------------------------
meta <- data.frame(
  sample_id  = paste0("S", 1:6),
  subject_id = c("P1", "P1", "P2", "P2", "P3", "P3"),
  batch_id   = c("B1", "B2", "B2", "B3", "B3", "B1"),
  stringsAsFactors = FALSE
)
g <- graph_from_metadata(meta)
table(grouping_vector(derive_split_constraints(g, "composite", via = c("subject", "batch"))))

## ----remedies-----------------------------------------------------------------
compare_split_constraints(g, k = 2)
find_dependency_bridges(g, via = c("Subject", "Batch"))
grouping_vector(derive_split_constraints(g, "composite", strategy = "rule_based",
                                         via = c("subject", "batch"),
                                         priority = c("subject", "batch")))

## ----threshold----------------------------------------------------------------
pairs <- data.frame(id1 = c("P1", "P2"), id2 = c("P2", "P3"), kinship = c(0.26, 0.13))
meta <- data.frame(sample_id = c("S1", "S2", "S3"), subject_id = c("P1", "P2", "P3"))
build <- function(threshold) {
  g <- build_dependency_graph(
    list(create_nodes(meta, "Sample", "sample_id"), create_nodes(meta, "Subject", "subject_id")),
    list(create_edges(meta, "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject"),
         relatedness_edges_from_kinship(pairs, threshold = threshold))
  )
  grouping_vector(derive_split_constraints(g, "relatedness"))
}
build(0.25)  # only P1~P2 pass: {S1,S2}, {S3}
build(0.10)  # P2~P3 also passes and chains: {S1,S2,S3}

## ----schema-major, eval = requireNamespace("jsonlite", quietly = TRUE)--------
tiny <- data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2"),
                   stringsAsFactors = FALSE)
g_tiny <- graph_from_metadata(tiny)
p <- tempfile(fileext = ".json")
write_split_spec(as_split_spec(derive_split_constraints(g_tiny, "subject"),
                               graph = g_tiny), p)

# Pretend the file was written by a future splitGraph with a different major.
raw <- jsonlite::fromJSON(p, simplifyVector = FALSE)
raw$schema_version <- "1.0.0"
writeLines(jsonlite::toJSON(raw, auto_unbox = TRUE, null = "null"), p)

back <- withCallingHandlers(
  read_split_spec(p),
  warning = function(w) {
    message("warning: ", conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
class(back)
unlink(p)

## ----se, eval = requireNamespace("SummarizedExperiment", quietly = TRUE)------
meta <- data.frame(
  sample_id  = c("S1", "S2", "S3", "S4"),
  subject_id = c("P1", "P1", "P2", "P2"),
  batch_id   = c("B1", "B2", "B1", "B2"),
  stringsAsFactors = FALSE
)
se <- SummarizedExperiment::SummarizedExperiment(
  assays  = list(counts = matrix(0, nrow = 3, ncol = 4,
                                 dimnames = list(NULL, meta$sample_id))),
  colData = meta[, c("subject_id", "batch_id")]
)

g_se <- graph_from_metadata(se, graph_name = "from-se")
grouping_vector(derive_split_constraints(g_se, "subject"))

# identical to building from the data frame directly
identical(
  grouping_vector(derive_split_constraints(g_se, "subject")),
  grouping_vector(derive_split_constraints(graph_from_metadata(meta), "subject"))
)

## ----conditions---------------------------------------------------------------
g <- graph_from_metadata(data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2")))
tryCatch(
  query_neighbors(g, "sample:S9"),
  splitgraph_reference_error = function(e) e$code
)

