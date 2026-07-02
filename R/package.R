#' retractionpollution
#'
#' Trace citation pollution from retracted and concerning scientific papers.
#'
#' @importFrom duckdb duckdb
#' @keywords internal
"_PACKAGE"

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".data", "cited_seed_count", "depth", "depth2_citer_count",
    "depth2_descendants", "direct_citers", "n", "node_count",
    "notice_date", "notice_type", "openalex_id",
    "post_notice_depth2_descendants", "post_notice_direct_citers",
    "publication_date", "publication_year", "record_id",
    "seed_record_count", "seed_record_ids", "source_id", "target_id",
    "title", "topic_domain", "topic_name", "total_depth2_reach", "year",
    # manuscript.R figure/aggregation variables
    "offset_year", "edge_count", "distinct_sources", "distinct_targets",
    "surv", "reach", "L", "p", "count", "layer", "short", "period",
    "value", "source_api", "nodes", "pubdate_null", "label", "x", "y", "lab",
    # control.R matched-comparison variables
    "share", "group", "share_diff", "control_share", "seed_share",
    "control_id", "control_post_in", "control_indeg_dated", "pair_id", "indeg",
    # network figure variables
    "xend", "yend", "Depth"
  ))
}
