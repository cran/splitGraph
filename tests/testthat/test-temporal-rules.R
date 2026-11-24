# Clinical timeline rules (feature_postdates_index_time,
# outcome_precedes_index_time) and the unit discipline behind them (SG-3, SG-5).

issue_rows <- function(g, code) {
  iss <- validate_graph(g)$issues
  iss[iss$code == code, , drop = FALSE]
}
flagged_nodes <- function(g, code) unique(unlist(issue_rows(g, code)$node_ids))

test_that("SG-3: a bare number is never compared with a Date (seconds vs days)", {
  # Days since epoch: 18000 = 2019-04-15, 18500 = 2020-08-26. Under the old
  # parser the Date became seconds and the numbers stayed days, so S2's real
  # violation was missed and every outcome looked earlier than index_time.
  meta <- data.frame(
    sample_id = c("S1", "S2"), subject_id = c("P1", "P2"),
    index_time = as.Date(c("2020-01-01", "2020-01-01")),
    collection_time = c(18000, 18500),
    outcome_time = c(18600, 18600),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)
  expect_length(flagged_nodes(g, "feature_postdates_index_time"), 0L)
  expect_length(flagged_nodes(g, "outcome_precedes_index_time"), 0L)

  mm <- issue_rows(g, "temporal_unit_mismatch")
  expect_gte(nrow(mm), 1L)
  expect_true(all(mm$severity == "warning"))
  expect_true(all(mm$level == "leakage"))
  expect_setequal(unique(unlist(mm$node_ids)), c("sample:S1", "sample:S2"))
  msg <- paste(mm$message, collapse = " ")
  expect_match(msg, "index_time", fixed = TRUE)
  expect_match(msg, "collection_time", fixed = TRUE)
  expect_match(msg, "outcome_time", fixed = TRUE)
  expect_match(msg, "not compared", fixed = TRUE)
  expect_match(msg, "feature_postdates_index_time", fixed = TRUE)
  expect_match(msg, "outcome_precedes_index_time", fixed = TRUE)
})

test_that("SG-3: same-unit timelines are compared and raise no unit issue", {
  # All bare numbers (one unit): S2 collection after index, S3 outcome before.
  num <- data.frame(
    sample_id = c("S1", "S2", "S3"), subject_id = c("P1", "P2", "P3"),
    index_time = c(10, 10, 10), collection_time = c(8, 12, 9),
    outcome_time = c(20, 20, 5), stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(num)
  expect_identical(flagged_nodes(g, "feature_postdates_index_time"), "sample:S2")
  expect_identical(flagged_nodes(g, "outcome_precedes_index_time"), "sample:S3")
  expect_equal(nrow(issue_rows(g, "temporal_unit_mismatch")), 0L)
  expect_equal(nrow(issue_rows(g, "temporal_resolution_mismatch")), 0L)

  # Numeric strings are bare numbers too, so they compare with numbers.
  num_chr <- num
  num_chr$index_time <- c("10", "10", "10")
  g2 <- graph_from_metadata(num_chr)
  expect_identical(flagged_nodes(g2, "feature_postdates_index_time"), "sample:S2")
  expect_equal(nrow(issue_rows(g2, "temporal_unit_mismatch")), 0L)

  # All Dates: same verdicts, no unit issue.
  dts <- data.frame(
    sample_id = c("S1", "S2", "S3"), subject_id = c("P1", "P2", "P3"),
    index_time = as.Date("2020-01-10"),
    collection_time = as.Date(c("2020-01-08", "2020-01-12", "2020-01-09")),
    outcome_time = as.Date(c("2020-01-20", "2020-01-20", "2020-01-05")),
    stringsAsFactors = FALSE
  )
  g3 <- graph_from_metadata(dts)
  expect_identical(flagged_nodes(g3, "feature_postdates_index_time"), "sample:S2")
  expect_identical(flagged_nodes(g3, "outcome_precedes_index_time"), "sample:S3")
  expect_equal(nrow(issue_rows(g3, "temporal_unit_mismatch")), 0L)
  expect_equal(nrow(issue_rows(g3, "temporal_resolution_mismatch")), 0L)
})

test_that("SG-3: Date vs date-time is compared on the calendar day, with an advisory", {
  s <- create_nodes(
    data.frame(
      sample_id = c("same_day", "next_day", "before"),
      index_time = as.Date("2020-01-01"),
      collection_time = as.POSIXct(c("2020-01-01 10:00", "2020-01-02 10:00", "2019-12-31 23:00"), tz = "UTC"),
      stringsAsFactors = FALSE
    ),
    type = "Sample", id_col = "sample_id"
  )
  g <- build_dependency_graph(list(s), list(graph_edge_set()))
  # Same calendar day is not "later"; the next day is.
  expect_identical(flagged_nodes(g, "feature_postdates_index_time"), "sample:next_day")
  expect_equal(nrow(issue_rows(g, "temporal_unit_mismatch")), 0L)
  adv <- issue_rows(g, "temporal_resolution_mismatch")
  expect_equal(nrow(adv), 1L)
  expect_identical(adv$severity, "advisory")
  expect_setequal(unlist(adv$node_ids), c("sample:same_day", "sample:next_day", "sample:before"))
  expect_match(adv$message, "day resolution", fixed = TRUE)

  # The calendar day is the one written in the value's own time zone, not UTC:
  # 22:00 on 1 Jan in New York is 03:00 on 2 Jan in UTC, still the index day.
  s_tz <- create_nodes(
    data.frame(
      sample_id = "ny", index_time = as.Date("2020-01-01"),
      collection_time = as.POSIXct("2020-01-01 22:00", tz = "America/New_York"),
      stringsAsFactors = FALSE
    ),
    type = "Sample", id_col = "sample_id"
  )
  g_tz <- build_dependency_graph(list(s_tz), list(graph_edge_set()))
  expect_length(flagged_nodes(g_tz, "feature_postdates_index_time"), 0L)

  # ISO strings: a date-only string vs a date-time with an offset.
  s_iso <- create_nodes(
    data.frame(
      sample_id = c("iso_ok", "iso_late"),
      index_time = "2022-06-01",
      collection_time = c("2022-06-01T23:30:00+05:00", "2022-06-02T00:30:00+05:00"),
      stringsAsFactors = FALSE
    ),
    type = "Sample", id_col = "sample_id"
  )
  g_iso <- build_dependency_graph(list(s_iso), list(graph_edge_set()))
  expect_identical(flagged_nodes(g_iso, "feature_postdates_index_time"), "sample:iso_late")
  expect_equal(nrow(issue_rows(g_iso, "temporal_resolution_mismatch")), 1L)
})

test_that("SG-3: units are checked across FeatureSet and Outcome nodes too", {
  # graph_from_metadata() copies a sample-level event_time onto the Sample, so
  # the Outcome-node path is built by hand: the Sample has no outcome time of
  # its own and inherits the linked Outcome's.
  build <- function(window_end, event_time) {
    s <- create_nodes(
      data.frame(sample_id = c("S1", "S2"), index_time = as.Date("2020-01-01"),
                 stringsAsFactors = FALSE),
      type = "Sample", id_col = "sample_id"
    )
    fs <- create_nodes(data.frame(featureset_id = "FS1", feature_window_end = window_end,
                                  stringsAsFactors = FALSE),
                       type = "FeatureSet", id_col = "featureset_id")
    out <- create_nodes(data.frame(outcome_id = "O1", event_time = event_time,
                                   stringsAsFactors = FALSE),
                        type = "Outcome", id_col = "outcome_id")
    links <- data.frame(sample_id = c("S1", "S2"), featureset_id = "FS1", outcome_id = "O1",
                        stringsAsFactors = FALSE)
    build_dependency_graph(
      list(s, fs, out),
      list(
        create_edges(links, "sample_id", "featureset_id", "Sample", "FeatureSet", "sample_uses_featureset"),
        create_edges(links, "sample_id", "outcome_id", "Sample", "Outcome", "sample_has_outcome")
      )
    )
  }

  g <- build(18500, 18000)
  expect_length(flagged_nodes(g, "feature_postdates_index_time"), 0L)
  # event_time 18000 < window 18500 is a same-unit (bare number) comparison and
  # is a real ordering violation; only the comparisons with index_time are skipped.
  expect_setequal(flagged_nodes(g, "outcome_precedes_index_time"),
                  c("sample:S1", "sample:S2", "outcome:O1"))
  mm <- issue_rows(g, "temporal_unit_mismatch")
  msg <- paste(mm$message, collapse = " ")
  expect_match(msg, "FeatureSet `feature_window_end`", fixed = TRUE)
  expect_match(msg, "Outcome `event_time`", fixed = TRUE)
  expect_true(all(c("sample:S1", "sample:S2", "featureset:FS1", "outcome:O1") %in% unlist(mm$node_ids)))

  # Everything as dates: compared, S1/S2 flagged for the late window, no unit issue.
  g_ok <- build(as.Date("2020-02-01"), as.Date("2020-06-01"))
  expect_setequal(flagged_nodes(g_ok, "feature_postdates_index_time"),
                  c("sample:S1", "sample:S2", "featureset:FS1"))
  expect_length(flagged_nodes(g_ok, "outcome_precedes_index_time"), 0L)
  expect_equal(nrow(issue_rows(g_ok, "temporal_unit_mismatch")), 0L)
})

test_that("SG-3 edge cases: NA sides, factors, no samples, JSON round trip", {
  # NA on one side: nothing to compare, nothing reported.
  na_meta <- data.frame(
    sample_id = c("S1", "S2"), subject_id = c("P1", "P2"),
    index_time = as.Date(c("2020-01-01", NA)),
    collection_time = c(NA, 18500), stringsAsFactors = FALSE
  )
  g_na <- graph_from_metadata(na_meta)
  expect_equal(nrow(issue_rows(g_na, "temporal_unit_mismatch")), 0L)
  expect_length(flagged_nodes(g_na, "feature_postdates_index_time"), 0L)

  # A factor value is read by its label, not its integer code.
  s_f <- create_nodes(
    data.frame(sample_id = "F1", index_time = factor("2020-01-01"),
               collection_time = factor("2020-03-01")),
    type = "Sample", id_col = "sample_id"
  )
  g_f <- build_dependency_graph(list(s_f), list(graph_edge_set()))
  expect_identical(flagged_nodes(g_f, "feature_postdates_index_time"), "sample:F1")
  expect_equal(nrow(issue_rows(g_f, "temporal_unit_mismatch")), 0L)

  # A single sample without temporal attributes: no temporal issues at all.
  one <- create_nodes(data.frame(sample_id = "S1"), type = "Sample", id_col = "sample_id")
  g_one <- build_dependency_graph(list(one), list(graph_edge_set()))
  expect_false(any(grepl("^temporal_|_index_time$", validate_graph(g_one)$issues$code)))

  # SG-10: text that is not a date no longer stops graph construction; the
  # comparison is reported as not made.
  txt <- data.frame(sample_id = c("S1", "S2"), subject_id = c("P1", "P2"),
                    index_time = "2020-01-01", collection_time = c("baseline", "2020-03-01"),
                    stringsAsFactors = FALSE)
  g_txt <- graph_from_metadata(txt)
  expect_identical(flagged_nodes(g_txt, "feature_postdates_index_time"), "sample:S2")
  mm_txt <- issue_rows(g_txt, "temporal_unit_mismatch")
  expect_equal(nrow(mm_txt), 1L)
  expect_identical(unlist(mm_txt$node_ids), "sample:S1")
  expect_match(mm_txt$message, "text that is not a date", fixed = TRUE)

  # Units survive JSON: a Date written as "2020-01-01" is read back as a date,
  # so the round-tripped graph gives the same verdicts.
  skip_if_not_installed("jsonlite")
  mixed <- data.frame(
    sample_id = c("S1", "S2"), subject_id = c("P1", "P2"),
    index_time = as.Date(c("2020-01-01", "2020-01-01")),
    collection_time = as.Date(c("2019-12-01", "2020-02-01")),
    outcome_time = c(18600, 18600), stringsAsFactors = FALSE
  )
  g_m <- graph_from_metadata(mixed)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_dependency_graph(g_m, tmp)
  g_rt <- read_dependency_graph(tmp)
  for (code in c("feature_postdates_index_time", "outcome_precedes_index_time",
                 "temporal_unit_mismatch")) {
    expect_identical(flagged_nodes(g_rt, code), flagged_nodes(g_m, code), info = code)
  }
  expect_identical(flagged_nodes(g_m, "feature_postdates_index_time"), "sample:S2")
})

test_that("SG-3: unit issues have a deliberate severed value", {
  meta <- data.frame(
    sample_id = c("S1", "S2", "S3"), subject_id = c("P1", "P2", "P3"),
    index_time = as.Date("2020-01-01"),
    collection_time = c(18000, 18500, 18000),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)
  for (mode in c("subject", "study")) {
    con <- suppressWarnings(derive_split_constraints(g, mode = mode))
    d <- summarize_leakage_risks(g, constraint = con)$diagnostics
    row <- d[d$category == "temporal_unit_mismatch", , drop = FALSE]
    expect_equal(nrow(row), 1L, info = mode)
    # A split cannot settle an ordering that was never checked.
    expect_identical(row$severed, NA, info = mode)
  }
  expect_true(is.na(splitGraph:::.leakage_severed_by_constraint("temporal_unit_mismatch", mode = "time")))
  expect_true(is.na(splitGraph:::.leakage_severed_by_constraint("temporal_resolution_mismatch", mode = "subject")))
})

test_that("SG-5: the outcome ordering message names only the ordering it checks", {
  meta <- data.frame(
    sample_id = c("S1", "S2"), subject_id = c("P1", "P2"),
    index_time = c(10, 10), collection_time = c(8, 12),
    outcome_time = c(20, 5), stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)
  iss <- validate_graph(g)$issues
  out_msg <- iss$message[iss$code == "outcome_precedes_index_time"]
  expect_length(out_msg, 1L)
  expect_false(grepl("immortal|reverse[- ]causal", out_msg, ignore.case = TRUE))
  expect_match(out_msg, "sample:S2", fixed = TRUE)
  expect_match(out_msg, "`outcome_time` is earlier than `index_time`", fixed = TRUE)
  expect_match(out_msg, "timestamp ordering", fixed = TRUE)

  post_msg <- iss$message[iss$code == "feature_postdates_index_time"]
  expect_length(post_msg, 1L)
  expect_match(post_msg, "`collection_time` is later than `index_time`", fixed = TRUE)
  expect_false(grepl("immortal", post_msg, ignore.case = TRUE))
})

test_that("SG-11: date-time attributes keep their instant through JSON", {
  skip_if_not_installed("jsonlite")
  # 06:00 / 08:00 in New York are 11:00Z / 13:00Z: only B is after a 12:00Z index.
  s <- create_nodes(
    data.frame(sample_id = c("A", "B"), index_time = "2020-01-01T12:00:00Z",
               collection_time = as.POSIXct(c("2020-01-01 06:00", "2020-01-01 08:00"),
                                            tz = "America/New_York"),
               stringsAsFactors = FALSE),
    type = "Sample", id_col = "sample_id"
  )
  g <- build_dependency_graph(list(s), list(graph_edge_set()))
  expect_identical(flagged_nodes(g, "feature_postdates_index_time"), "sample:B")

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_dependency_graph(g, tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  written <- vapply(raw$nodes, function(n) n$attrs$collection_time, character(1))
  expect_identical(written, c("2020-01-01T06:00:00-0500", "2020-01-01T08:00:00-0500"))
  g_rt <- read_dependency_graph(tmp)
  expect_identical(flagged_nodes(g_rt, "feature_postdates_index_time"), "sample:B")
  expect_true(validate_graph_json(tmp)$valid)

  # A Date stays a plain date; fractional seconds survive.
  s2 <- create_nodes(
    data.frame(sample_id = "C", index_time = as.Date("2020-01-01"),
               collection_time = as.POSIXct("2020-01-01 10:00:00.25", tz = "UTC"),
               stringsAsFactors = FALSE),
    type = "Sample", id_col = "sample_id"
  )
  write_dependency_graph(build_dependency_graph(list(s2), list(graph_edge_set())), tmp)
  raw2 <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)$nodes[[1]]$attrs
  expect_identical(raw2$index_time, "2020-01-01")
  expect_identical(raw2$collection_time, "2020-01-01T10:00:00.250000+0000")
})

test_that("R4 review: difftime, fractional Dates and non-finite values", {
  mk <- function(df) {
    build_dependency_graph(list(create_nodes(df, type = "Sample", id_col = "sample_id")),
                           list(graph_edge_set()))
  }
  # difftime is a time span: compared in seconds with another span (2 h < 1 d),
  # never with a bare number.
  df <- data.frame(sample_id = c("S1", "S2"))
  df$index_time <- as.difftime(c(1, 1), units = "days")
  df$collection_time <- as.difftime(c(2, 30), units = "hours")
  g <- mk(df)
  expect_identical(flagged_nodes(g, "feature_postdates_index_time"), "sample:S2")
  expect_equal(nrow(issue_rows(g, "temporal_unit_mismatch")), 0L)
  df_num <- data.frame(sample_id = "S1", collection_time = 2)
  df_num$index_time <- as.difftime(1, units = "days")
  mm <- issue_rows(mk(df_num), "temporal_unit_mismatch")
  expect_equal(nrow(mm), 1L)
  expect_match(mm$message, "a time span", fixed = TRUE)

  # A Date with a fractional day is the same calendar date as its print form.
  df <- data.frame(sample_id = "S1")
  df$index_time <- mean(as.Date(c("2020-01-01", "2020-01-04")))  # 2020-01-02.5
  df$collection_time <- as.Date("2020-01-02")
  df$outcome_time <- as.Date("2020-01-02")
  g <- mk(df)
  expect_length(flagged_nodes(g, "outcome_precedes_index_time"), 0L)
  expect_length(flagged_nodes(g, "feature_postdates_index_time"), 0L)

  # Non-finite values are missing (JSON writes them as null), so the verdict
  # is the same before and after a round trip.
  g_inf <- mk(data.frame(sample_id = "S1", index_time = 10, collection_time = Inf))
  expect_equal(nrow(issue_rows(g_inf, "temporal_unit_mismatch")), 0L)
  df_d <- data.frame(sample_id = "S1", collection_time = as.Date("2020-01-01"))
  df_d$index_time <- as.Date(Inf)
  g_dinf <- mk(df_d)
  expect_equal(nrow(issue_rows(g_dinf, "temporal_unit_mismatch")), 0L)
  skip_if_not_installed("jsonlite")
  for (gg in list(g_inf, g_dinf)) {
    tmp <- tempfile(fileext = ".json")
    write_dependency_graph(gg, tmp)
    expect_equal(nrow(issue_rows(read_dependency_graph(tmp), "temporal_unit_mismatch")), 0L)
    unlink(tmp)
  }
})

test_that("SG-12: difftime attributes serialize as ISO 8601 durations and keep verdicts through JSON", {
  skip_if_not_installed("jsonlite")
  mk <- function(df) {
    build_dependency_graph(list(create_nodes(df, type = "Sample", id_col = "sample_id")),
                           list(graph_edge_set()))
  }

  # Same-unit difftime comparison across units (days vs hours/mins/negative/zero/fractional)
  # plus NA/Inf handling: verdicts must match before and after JSON round-trip.
  df <- data.frame(sample_id = c("S1", "S2", "S3", "S4", "S5"), stringsAsFactors = FALSE)
  df$index_time <- as.difftime(c(1, 1, 0, -2, 1), units = "days")
  df$collection_time <- as.difftime(c(2, 30, 0, -30, NA_real_), units = "hours")
  df$outcome_time <- as.difftime(c(3, 3, -0.5, -1, Inf), units = "days")
  g <- mk(df)
  expect_identical(flagged_nodes(g, "feature_postdates_index_time"), c("sample:S2", "sample:S4"))
  expect_identical(flagged_nodes(g, "outcome_precedes_index_time"), "sample:S3")
  expect_equal(nrow(issue_rows(g, "temporal_unit_mismatch")), 0L)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_dependency_graph(g, tmp)
  expect_true(validate_graph_json(tmp)$valid)

  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  written_idx <- lapply(raw$nodes, function(n) n$attrs$index_time)
  written_col <- lapply(raw$nodes, function(n) n$attrs$collection_time)
  written_out <- lapply(raw$nodes, function(n) n$attrs$outcome_time)
  expect_identical(written_idx, list("P1D", "P1D", "PT0S", "-P2D", "P1D"))
  expect_identical(written_col, list("PT2H", "P1DT6H", "PT0S", "-P1DT6H", NULL))
  expect_identical(written_out, list("P3D", "P3D", "-PT12H", "-P1D", NULL))

  g_rt <- read_dependency_graph(tmp)
  for (code in c("feature_postdates_index_time", "outcome_precedes_index_time",
                 "temporal_unit_mismatch", "temporal_resolution_mismatch")) {
    expect_identical(flagged_nodes(g_rt, code), flagged_nodes(g, code), info = code)
  }

  # Unit discipline survives round-trip: a difftime compared against a bare number
  # or a Date must STILL report temporal_unit_mismatch after writing and reading.
  df_mix <- data.frame(
    sample_id = c("M1", "M2"),
    collection_time = c(2, 100),
    stringsAsFactors = FALSE
  )
  df_mix$index_time <- as.difftime(c(1, 1), units = "days")
  g_mix <- mk(df_mix)
  expect_equal(nrow(issue_rows(g_mix, "temporal_unit_mismatch")), 1L)
  expect_length(flagged_nodes(g_mix, "feature_postdates_index_time"), 0L)

  write_dependency_graph(g_mix, tmp)
  g_mix_rt <- read_dependency_graph(tmp)
  expect_equal(nrow(issue_rows(g_mix_rt, "temporal_unit_mismatch")), 1L)
  expect_length(flagged_nodes(g_mix_rt, "feature_postdates_index_time"), 0L)

  # Direct ISO 8601 duration strings (weeks, fractional seconds, signs) and
  # ambiguous/malformed durations (years, months, bare P/PT).
  df_str <- data.frame(
    sample_id = c("W1", "F1", "BAD1", "BAD2", "BAD3"),
    index_time = c("P1W", "PT10.25S", "P1Y", "P1D", "P1D"),
    collection_time = c("P8D", "PT10.5S", "P1D", "PT", "P1DT"),
    stringsAsFactors = FALSE
  )
  g_str <- mk(df_str)
  expect_identical(flagged_nodes(g_str, "feature_postdates_index_time"), c("sample:W1", "sample:F1"))
  mm_str <- issue_rows(g_str, "temporal_unit_mismatch")
  expect_setequal(unique(unlist(mm_str$node_ids)), c("sample:BAD1", "sample:BAD2", "sample:BAD3"))
})

test_that("SG-15: non-finite POSIXct and Date attributes serialize as null and keep verdicts", {
  skip_if_not_installed("jsonlite")
  mk <- function(df) {
    build_dependency_graph(list(create_nodes(df, type = "Sample", id_col = "sample_id")),
                           list(graph_edge_set()))
  }
  df_p <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    index_time = as.POSIXct(c("2024-01-01 00:00:00", "2024-01-02 00:00:00", "2024-01-03 00:00:00"), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  df_p$collection_time <- as.POSIXct(c(Inf, -Inf, NaN), origin = "1970-01-01", tz = "UTC")
  g_p <- mk(df_p)
  expect_equal(nrow(validate_graph(g_p)$issues), 0L)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(write_dependency_graph(g_p, tmp))
  raw_p <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_identical(lapply(raw_p$nodes, function(n) n$attrs$collection_time), list(NULL, NULL, NULL))
  expect_equal(nrow(validate_graph(read_dependency_graph(tmp))$issues), 0L)

  df_d <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    index_time = as.Date("2024-01-01"),
    stringsAsFactors = FALSE
  )
  df_d$collection_time <- structure(c(NaN, Inf, -Inf), class = "Date")
  g_d <- mk(df_d)
  expect_equal(nrow(validate_graph(g_d)$issues), 0L)
  write_dependency_graph(g_d, tmp)
  raw_d <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_identical(lapply(raw_d$nodes, function(n) n$attrs$collection_time), list(NULL, NULL, NULL))
  expect_equal(nrow(validate_graph(read_dependency_graph(tmp))$issues), 0L)
})

test_that("SG-17: untimed sample_has_outcome does not mask timed subject_has_outcome", {
  build_both <- function(relapse_date) {
    build_dependency_graph(
      nodes = list(
        create_nodes(data.frame(sample_id = "S1", index_time = "2024-06-01", stringsAsFactors = FALSE),
                     "Sample", "sample_id", attr_cols = "index_time"),
        create_nodes(data.frame(subject_id = "P1", stringsAsFactors = FALSE), "Subject", "subject_id"),
        create_nodes(data.frame(outcome_id = c("cls_case", "relapse"),
                                event_time = c(NA_character_, relapse_date),
                                stringsAsFactors = FALSE),
                     "Outcome", "outcome_id", attr_cols = "event_time")
      ),
      edges = list(
        create_edges(data.frame(sample_id = "S1", subject_id = "P1", stringsAsFactors = FALSE),
                     "sample_id", "subject_id", "Sample", "Subject", "sample_belongs_to_subject"),
        create_edges(data.frame(sample_id = "S1", outcome_id = "cls_case", stringsAsFactors = FALSE),
                     "sample_id", "outcome_id", "Sample", "Outcome", "sample_has_outcome"),
        create_edges(data.frame(subject_id = "P1", outcome_id = "relapse", stringsAsFactors = FALSE),
                     "subject_id", "outcome_id", "Subject", "Outcome", "subject_has_outcome")
      )
    )
  }
  # Leaky: subject relapse precedes sample index_time
  g_bad <- build_both("2024-01-01")
  expect_setequal(flagged_nodes(g_bad, "outcome_precedes_index_time"), c("sample:S1", "outcome:relapse"))
  # Benign look-alike: subject relapse is after sample index_time
  g_ok <- build_both("2024-09-01")
  expect_length(flagged_nodes(g_ok, "outcome_precedes_index_time"), 0L)
})

test_that("SG-21: malformed ISO datetimes (bad seconds, hour 24, out-of-range offset) are unparseable", {
  meta <- data.frame(
    sample_id       = c("S1", "S2", "S3", "S4"),
    index_time      = c("2024-01-10 12:30:10", "2024-01-10 12:30:10Z",
                        "2024-01-10 12:30:10Z", "2024-01-10 12:30:10Z"),
    collection_time = c("2024-01-10 12:30:65", "2024-01-10T12:30:10-25:00",
                        "2024-01-10T12:30:10+05:99", "2024-01-10T24:00:00Z"),
    stringsAsFactors = FALSE
  )
  g <- graph_from_metadata(meta)
  expect_length(flagged_nodes(g, "feature_postdates_index_time"), 0L)
  mm <- issue_rows(g, "temporal_unit_mismatch")
  expect_gte(nrow(mm), 1L)
  expect_setequal(unique(unlist(mm$node_ids)), c("sample:S1", "sample:S2", "sample:S3", "sample:S4"))
})
