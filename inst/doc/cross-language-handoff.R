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

## ----pypath-------------------------------------------------------------------
system.file("python", package = "splitGraph")

## ----conformance, eval = nzchar(Sys.which("python3")) && requireNamespace("jsonlite", quietly = TRUE)----
script   <- system.file("python", "conformance.py", package = "splitGraph")
out_path <- tempfile(fileext = ".json")

# Run the Python reader on our JSON file; it writes back what it recovered.
status <- system2(
  "python3", c("-B", shQuote(script), shQuote(path), shQuote(out_path)),
  stdout = FALSE, stderr = FALSE
)

if (status == 0 && file.exists(out_path)) {
  recovered <- jsonlite::fromJSON(out_path)

  # Grouping recovered by Python:
  print(unlist(recovered$grouping))

  # Identical to the grouping R produced?
  r_grouping <- grouping_vector(constraint)
  cat("Python matches R exactly:",
      identical(unlist(recovered$grouping)[names(r_grouping)],
                r_grouping[names(r_grouping)]), "\n")
}

