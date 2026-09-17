## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----derive-------------------------------------------------------------------
library(splitGraph)

meta <- data.frame(
  sample_id    = c("S1", "S2", "S3", "S4", "S5"),
  subject_id   = c("P1", "P1", "P2", "P3", "P3"),
  timepoint_id = c("T0", "T1", "T0", "T2", "T0"),
  time_index   = c(0, 1, 0, 2, 0),
  outcome_id   = c("case", "case", "ctrl", "ctrl", "ctrl"),
  stringsAsFactors = FALSE
)

g <- graph_from_metadata(meta, graph_name = "handoff-demo")

# Group so that repeated measures of the same subject never straddle a split.
constraint <- derive_split_constraints(g, mode = "subject")
spec <- as_split_spec(constraint, graph = g)

path <- tempfile(fileext = ".json")
write_split_spec(spec, path)

## ----validate-----------------------------------------------------------------
report <- validate_split_spec_json(path)
report$valid

# The R-side grouping we expect Python to reproduce:
grouping_vector(constraint)

# The outcome travels with the spec as a stratum annotation, so a consumer can
# stratify without touching the graph. splitGraph never balances folds itself.
spec$stratum_var
spec$sample_data$stratum

## ----json-shape, eval = requireNamespace("jsonlite", quietly = TRUE)----------
on_disk <- jsonlite::fromJSON(path, simplifyVector = FALSE)
names(on_disk)

# One sample row. Every declared role above names a column in here.
str(on_disk$sample_data[[1]])

## ----pypath-------------------------------------------------------------------
system.file("python", package = "splitGraph")

## ----find-python--------------------------------------------------------------
find_python <- function() {
  for (name in c("python3", "python")) {
    candidate <- Sys.which(name)
    if (!nzchar(candidate)) next
    probe <- tryCatch(
      suppressWarnings(system2(
        candidate, c("-c", shQuote("import sys; print(sys.version_info[0])")),
        stdout = TRUE, stderr = TRUE
      )),
      error = function(e) character()
    )
    if (is.null(attr(probe, "status")) && any(trimws(probe) == "3")) return(candidate)
  }
  ""
}

python <- find_python()
nzchar(python)

## ----conformance, eval = requireNamespace("jsonlite", quietly = TRUE)---------
if (!nzchar(python)) {
  cat("No usable Python 3 found; skipping the round-trip check.\n")
} else {
  script   <- system.file("python", "conformance.py", package = "splitGraph")
  out_path <- tempfile(fileext = ".json")

  # Run the Python reader on our JSON file; it writes back what it recovered.
  status <- suppressWarnings(system2(
    python, c("-B", shQuote(script), shQuote(path), shQuote(out_path)),
    stdout = FALSE, stderr = FALSE
  ))

  if (!identical(status, 0L) || !file.exists(out_path)) {
    cat("The Python reader could not be run (exit status ", status, ").\n", sep = "")
  } else {
    recovered  <- jsonlite::fromJSON(out_path)
    r_grouping <- grouping_vector(constraint)
    ids        <- spec$sample_data$sample_id

    # Grouping recovered by Python:
    print(unlist(recovered$grouping))

    # Identical to the grouping R produced?
    cat("grouping matches:",
        identical(unlist(recovered$grouping)[names(r_grouping)],
                  r_grouping[names(r_grouping)]), "\n")

    # The script returns the ordering and the stratum annotation too.
    cat("order_rank matches:",
        identical(as.integer(unlist(recovered$order_ranks)[ids]),
                  as.integer(spec$sample_data$order_rank)), "\n")
    cat("stratum matches:",
        identical(unname(unlist(recovered$strata)[ids]),
                  spec$sample_data$stratum), "\n")
    unlink(out_path)
  }
}

## ----cleanup, include = FALSE-------------------------------------------------
unlink(path)

