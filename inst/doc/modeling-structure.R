## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)
library(splitGraph)

## ----cluster------------------------------------------------------------------
meta <- data.frame(
  sample_id   = paste0("S", 1:6),
  subject_id  = c("P1", "P1", "P2", "P2", "P3", "P3"),
  site_id     = c("NYC", "NYC", "BOS", "BOS", "NYC", "BOS"),
  region_id   = c("cortex", "cortex", "cortex",
                  "hippocampus", "hippocampus", "hippocampus"),
  platform_id = c("illumina", "illumina", "nanopore",
                  "nanopore", "illumina", "nanopore"),
  assay_id    = c("rnaseq", "rnaseq", "rnaseq", "wgs", "wgs", "wgs"),
  stringsAsFactors = FALSE
)

g <- graph_from_metadata(meta, graph_name = "structure-demo")

cluster_modes <- c("site", "region", "platform", "assay")
do.call(cbind, lapply(
  stats::setNames(cluster_modes, cluster_modes),
  function(m) grouping_vector(derive_split_constraints(g, mode = m))
))

## ----cross-site---------------------------------------------------------------
report <- validate_graph(g)
report$issues[report$issues$code == "subject_cross_site_overlap",
              c("severity", "message")]

# Which constraint modes actually sever that path:
risks <- summarize_leakage_risks(g, constraint = derive_split_constraints(g, "subject"))
as.data.frame(risks)[as.data.frame(risks)$category == "subject_cross_site_overlap",
                     c("category", "severed")]

## ----block-annotations--------------------------------------------------------
spec <- as_split_spec(derive_split_constraints(g, mode = "subject"), graph = g)
spec$block_vars
head(spec$sample_data[, c("sample_id", "group_id", "site_group",
                          "region_group", "platform_group", "assay_group")])

## ----composite----------------------------------------------------------------
constraint <- derive_split_constraints(
  g, mode = "composite", strategy = "strict",
  via = c("Site", "Platform")
)
grouping_vector(constraint)

## ----composite-collapse-------------------------------------------------------
collapsed <- derive_split_constraints(
  g, mode = "composite", strategy = "strict",
  via = c("Site", "Platform", "Subject")
)
collapsed$metadata$n_groups
grouping_vector(collapsed)

## ----relatedness--------------------------------------------------------------
# A kinship table over subject pairs (one sample per subject here for clarity).
# P1-P2 and P2-P3 clear the threshold and chain together; P5-P6 form a second
# related pair; P1-P4 is too weak to count.
kin <- data.frame(
  id1     = c("P1", "P2", "P1", "P5"),
  id2     = c("P2", "P3", "P4", "P6"),
  kinship = c(0.25, 0.20, 0.02, 0.30),
  stringsAsFactors = FALSE
)
rel_edges <- relatedness_edges_from_kinship(kin, threshold = 0.1)

meta_r <- data.frame(
  sample_id  = paste0("S", 1:6),
  subject_id = paste0("P", 1:6),
  stringsAsFactors = FALSE
)
samples  <- create_nodes(meta_r, "Sample", "sample_id")
subjects <- create_nodes(meta_r, "Subject", "subject_id")
belongs  <- create_edges(meta_r, "sample_id", "subject_id",
                         "Sample", "Subject", "sample_belongs_to_subject")

g_rel <- build_dependency_graph(list(samples, subjects), list(belongs, rel_edges))

rel_groups <- grouping_vector(derive_split_constraints(g_rel, mode = "relatedness"))
rel_groups

## ----kinship-columns----------------------------------------------------------
king <- data.frame(
  ID1     = c("P1", "P2", "P1", "P5"),
  ID2     = c("P2", "P3", "P4", "P6"),
  Kinship = c(0.25, 0.20, 0.02, 0.30),
  stringsAsFactors = FALSE
)
king_edges <- relatedness_edges_from_kinship(
  king, threshold = 0.1, id1 = "ID1", id2 = "ID2", kinship = "Kinship"
)
as.data.frame(king_edges)[, c("from", "to")]

## ----kinship-matrix-----------------------------------------------------------
grm <- matrix(
  c(0.50, 0.25, 0.02, 0.00,
    0.25, 0.50, 0.20, 0.00,
    0.02, 0.20, 0.50, 0.00,
    0.00, 0.00, 0.00, 0.50),
  nrow = 4, byrow = TRUE,
  dimnames = list(paste0("P", 1:4), paste0("P", 1:4))
)

as.data.frame(relatedness_edges_from_kinship(grm, threshold = 0.1))[
  , c("from", "to")
]

## ----kinship-attrs------------------------------------------------------------
kept <- as.data.frame(query_edge_type(g_rel, "subject_related_to"))
data.frame(
  from    = kept$from,
  to      = kept$to,
  kinship = vapply(kept$attrs, function(a) a$kinship, numeric(1))
)

## ----rel-plot, fig.width = 6.5, fig.height = 4.5------------------------------
subject_group <- setNames(rel_groups[meta_r$sample_id], meta_r$subject_id)
kept_pairs <- kin[kin$kinship >= 0.1, c("id1", "id2")]
rel_net <- igraph::graph_from_data_frame(
  kept_pairs, directed = FALSE,
  vertices = data.frame(name = meta_r$subject_id)
)

palette_rel <- c("#4C78A8", "#F58518", "#54A24B", "#B279A2")
set.seed(1)
plot(rel_net,
     vertex.color       = palette_rel[as.integer(factor(subject_group[igraph::V(rel_net)$name]))],
     vertex.size        = 34,
     vertex.label.color = "white",
     vertex.label.font  = 2,
     edge.color         = "grey60",
     edge.width         = 2,
     main               = "Relatedness clusters (kinship >= 0.1)")

## ----rel-threshold------------------------------------------------------------
rel_strict <- relatedness_edges_from_kinship(kin, threshold = 0.22)
g_rel_strict <- build_dependency_graph(list(samples, subjects), list(belongs, rel_strict))

grouping_vector(derive_split_constraints(g_rel_strict, mode = "relatedness"))

## ----rel-sparse---------------------------------------------------------------
sparse <- derive_split_constraints(
  build_dependency_graph(
    list(samples, subjects),
    list(belongs, relatedness_edges_from_kinship(kin, threshold = 0.28))
  ),
  mode = "relatedness"
)
sparse$metadata$n_groups
sparse$metadata$warnings

## ----spatial------------------------------------------------------------------
# Two spatial clusters. Cluster 1 (S1-S3) is a chain: neighbouring pairs are
# within the radius, but the endpoints are not.
coords <- data.frame(
  sample_id = paste0("S", 1:6),
  x = c(0, 1, 2,  6.0, 6.9, 6.2),
  y = c(0, 1, 0,  6.0, 6.6, 5.3),
  stringsAsFactors = FALSE
)
adj_edges <- spatial_edges_from_coords(coords, radius = 1.5)

meta_s <- data.frame(
  sample_id  = paste0("S", 1:6),
  subject_id = paste0("P", 1:6),
  stringsAsFactors = FALSE
)
samples_s  <- create_nodes(meta_s, "Sample", "sample_id")
subjects_s <- create_nodes(meta_s, "Subject", "subject_id")
belongs_s  <- create_edges(meta_s, "sample_id", "subject_id",
                           "Sample", "Subject", "sample_belongs_to_subject")

g_sp <- build_dependency_graph(list(samples_s, subjects_s), list(belongs_s, adj_edges))

sp_groups <- grouping_vector(derive_split_constraints(g_sp, mode = "spatial"))
sp_groups

## ----sp-plot, fig.width = 6.5, fig.height = 5---------------------------------
sp_grp <- factor(sp_groups[coords$sample_id])
row_of <- setNames(seq_len(nrow(coords)), coords$sample_id)
from_i <- row_of[sub("^sample:", "", adj_edges$data$from)]
to_i   <- row_of[sub("^sample:", "", adj_edges$data$to)]
palette_sp <- c("#4C78A8", "#F58518")

plot(coords$x, coords$y, type = "n", asp = 1, xlab = "x", ylab = "y",
     main = "Spatial groups (radius = 1.5)")
segments(coords$x[from_i], coords$y[from_i],
         coords$x[to_i],   coords$y[to_i], col = "grey60", lwd = 2)
points(coords$x, coords$y, pch = 19, cex = 3.5, col = palette_sp[as.integer(sp_grp)])
text(coords$x, coords$y, labels = coords$sample_id, col = "white", cex = 0.8, font = 2)
legend("topleft", legend = levels(sp_grp), pch = 19,
       col = palette_sp[seq_along(levels(sp_grp))], title = "Spatial group", bty = "n")

## ----coord-cols---------------------------------------------------------------
coords_qc <- cbind(coords, reads_millions = c(31, 44, 12, 38, 27, 51))

# `reads_millions` is picked up as a third dimension: nothing is within radius.
nrow(as.data.frame(spatial_edges_from_coords(coords_qc, radius = 1.5)))

# Naming the coordinate columns restores the five adjacency edges.
nrow(as.data.frame(
  spatial_edges_from_coords(coords_qc, radius = 1.5, coord_cols = c("x", "y"))
))

## ----subset-scoping-----------------------------------------------------------
grouping_vector(
  derive_split_constraints(g_sp, mode = "spatial", samples = c("S1", "S3"))
)

## ----subset-graph-------------------------------------------------------------
g_two <- subset_graph(g_sp, samples = c("S1", "S3"), graph_name = "S1 + S3")
grouping_vector(derive_split_constraints(g_two, mode = "spatial"))

## ----composite-pairwise-------------------------------------------------------
meta_c <- data.frame(
  sample_id  = paste0("S", 1:4),
  subject_id = paste0("P", 1:4),
  batch_id   = c("B1", "B1", "B2", "B3"),
  stringsAsFactors = FALSE
)
kin_c <- data.frame(id1 = "P2", id2 = "P3", kinship = 0.25, stringsAsFactors = FALSE)

g_mixed <- add_edges(
  graph_from_metadata(meta_c, graph_name = "mixed-structure"),
  relatedness_edges_from_kinship(kin_c, threshold = 0.1)
)

# batch alone: {S1,S2} {S3} {S4}   relatedness alone: {S2,S3}
grouping_vector(derive_split_constraints(g_mixed, "batch"))
grouping_vector(derive_split_constraints(g_mixed, "relatedness"))

# combined, the chain S1-S2 (batch) - S3 (kinship) becomes one group:
mixed <- derive_split_constraints(g_mixed, "composite", via = c("batch", "relatedness"))
grouping_vector(mixed)
mixed$metadata$via

## ----threshold-provenance-----------------------------------------------------
g_mixed$metadata$edge_sources$subject_related_to[c("threshold", "metric")]

## ----threshold-in-spec--------------------------------------------------------
as_split_spec(derive_split_constraints(g_mixed, "relatedness"),
              graph = g_mixed)$metadata[c("threshold", "threshold_metric")]

