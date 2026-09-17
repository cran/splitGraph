## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(splitGraph)

## ----load---------------------------------------------------------------------
path <- system.file("extdata", "GSE60424_samples.csv", package = "splitGraph")
gse <- read.csv(path, stringsAsFactors = FALSE)
str(gse)

## ----facts--------------------------------------------------------------------
# 1. Every donor contributed six or seven samples, one per cell population
#    that was successfully sorted for them.
range(table(gse$subject_id))
table(table(gse$subject_id))
# 2. Every donor was collected on its own date, so collection date (the natural
#    "batch") coincides with donor.
all(tapply(gse$batch_id, gse$subject_id, function(x) length(unique(x))) == 1)
# 3. Every donor has exactly one disease status.
all(tapply(gse$condition, gse$subject_id, function(x) length(unique(x))) == 1)

## ----no-longitudinal----------------------------------------------------------
with(gse[gse$condition == "MS", ], table(subject_id, timepoint_id))

## ----build--------------------------------------------------------------------
meta <- gse[, c("sample_id", "subject_id", "batch_id", "cell_type", "condition", "sex")]
g <- graph_from_metadata(
  meta,
  columns = c(region_id = "cell_type", outcome_id = "condition"),
  graph_name = "GSE60424"
)
g
summary(g)$node_types

## ----ego, fig.width = 7.2, fig.height = 5, dpi = 150, out.width = "100%"------
plot(g, focus = "ego", node = "subject:D20", order = 2,
     legend_position = "bottomleft")

## ----validate-----------------------------------------------------------------
report <- validate_graph(g)
report
summary(report)$by_code

## ----naive--------------------------------------------------------------------
set.seed(1)
fold <- sample(rep(1:5, length.out = nrow(gse)))
straddling <- tapply(fold, gse$subject_id, function(f) length(unique(f)) > 1)
sum(straddling)

## ----derive-------------------------------------------------------------------
by_subject <- derive_split_constraints(g, mode = "subject")
by_batch   <- derive_split_constraints(g, mode = "batch")
by_region  <- derive_split_constraints(g, mode = "region")
c(subject = by_subject$metadata$n_groups,
  batch   = by_batch$metadata$n_groups,
  region  = by_region$metadata$n_groups)

## ----same-partition-----------------------------------------------------------
canon <- function(x) as.integer(match(x, unique(x)))
identical(canon(grouping_vector(by_subject)), canon(grouping_vector(by_batch)))

## ----region-wrong-------------------------------------------------------------
region_groups <- grouping_vector(by_region)
donor_spread <- tapply(region_groups[gse$sample_id], gse$subject_id,
                       function(x) length(unique(x)))
range(donor_spread)
sum(donor_spread > 1)   # donors split across more than one region group

## ----composite----------------------------------------------------------------
strict <- derive_split_constraints(g, mode = "composite", via = c("subject", "region"))
strict$metadata$n_groups
strict$metadata$warnings

## ----components---------------------------------------------------------------
comps <- detect_dependency_components(g, via = c("Subject", "Region"))
table(as.data.frame(comps)$component_size)

## ----rule-based---------------------------------------------------------------
ruled <- derive_split_constraints(
  g, mode = "composite", strategy = "rule_based",
  via = c("subject", "region"), priority = c("subject", "region")
)
ruled$metadata$n_groups
head(as.data.frame(ruled)[, c("sample_id", "group_id", "constraint_type")], 3)

## ----spec---------------------------------------------------------------------
spec <- as_split_spec(by_subject, graph = g)
spec
spec$block_vars
spec$stratum_var
head(as.data.frame(spec)[, c("sample_id", "group_id", "batch_group", "region_group", "stratum")], 7)
validate_split_spec(spec)

## ----risks--------------------------------------------------------------------
risks <- summarize_leakage_risks(g, constraint = by_subject, split_spec = spec)
unique(as.data.frame(risks)[, c("category", "severity", "severed")])

## ----subset-------------------------------------------------------------------
keep <- gse$sample_id[gse$condition %in% c("Healthy Control", "MS")]
g_sub <- subset_graph(g, samples = keep, graph_name = "GSE60424: HC vs MS")
summary(g_sub)$node_types
spec_sub <- as_split_spec(derive_split_constraints(g_sub, "subject"), graph = g_sub)
table(spec_sub$sample_data$stratum)

## ----write, eval = requireNamespace("jsonlite", quietly = TRUE)---------------
out <- tempfile(fileext = ".json")
write_split_spec(spec, out)
validate_split_spec_json(out)$valid
unlink(out)

