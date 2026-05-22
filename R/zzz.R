#' Package startup banner and shared UI helpers
#'
#' This file contains the package lifecycle hook that prints a banner on
#' load, and a small set of internal helpers that standardize the look of
#' log messages emitted during processing.
#'
#' @keywords internal
#' @noRd
NULL


# -------------------------------------------------------------------------
# Shared constants
# -------------------------------------------------------------------------
# Pre-wrap target width for sub-line and info messages. Long messages get
# broken at this width so continuation lines align under the message
# column instead of starting at the left margin.
.lfs_max_width <- 80L

# The closing rule shown at the end of a run. Same character (═) and width
# as the banner's rule.
.lfs_rule_char <- "\u2550"  # ═ double horizontal
.lfs_rule_len  <- 57L


# -------------------------------------------------------------------------
# .onAttach — prints banner once per session when library(lfsinsights) runs
# -------------------------------------------------------------------------
.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)

  ghost  <- cli::make_ansi_style("#aaaaaa", bg = FALSE)
  purple <- cli::make_ansi_style("#b8a4d4", bg = FALSE)

  packageStartupMessage("")
  packageStartupMessage(
    cli::col_blue(cli::style_bold("  LFS Insights  ")),
    ghost(paste0("v", version))
  )
  packageStartupMessage(cli::col_blue(paste0("  ", strrep(.lfs_rule_char, .lfs_rule_len))))
  packageStartupMessage(purple("  Labour Force Survey data processing & estimation engine"))
  packageStartupMessage(cli::col_silver("  Type ?lfsinsights for help"))
  packageStartupMessage("")
}


# -------------------------------------------------------------------------
# Internal cli helpers used throughout the package
# -------------------------------------------------------------------------
# Design:
#  - .lfs_info(msg)    — top-level event:     "   HH:MM:SS ℹ  <msg>"
#  - .lfs_sub(msg)     — sub-step (grey):     "   HH:MM:SS ⤷  <msg>"
#  - .lfs_phase(title) — phase header:        "❯  <title>" + thin underline
#  - .lfs_success(msg) — final completion:    "✔ <msg>" + closing rule
# -------------------------------------------------------------------------

# Layout shared by .lfs_info and .lfs_sub:
#   3 indent + 8 timestamp + 1 space + 1 glyph + 2 gap = 15 cols of prefix
.lfs_prefix_width <- 15L


#' Format the current time as HH:MM:SS
#' @keywords internal
#' @noRd
.lfs_now <- function() format(Sys.time(), "%H:%M:%S")


#' Emit a top-level informational event with a timestamp
#'
#' Renders as: "   HH:MM:SS ℹ  <msg>"
#' Long messages are pre-wrapped at .lfs_max_width with continuation lines
#' indented to align under the message column.
#'
#' @keywords internal
#' @noRd
.lfs_info <- function(msg) {
  msg <- gsub("\\{", "{{", msg, fixed = FALSE)
  msg <- gsub("\\}", "}}", msg, fixed = FALSE)

  available <- .lfs_max_width - .lfs_prefix_width
  wrapped <- strwrap(msg, width = available)

  first <- paste0(
    "   ", .lfs_now(), " ", cli::col_blue("\u2139"), "  ", wrapped[1L]
  )
  cli::cli_verbatim(first)

  if (length(wrapped) > 1L) {
    indent <- strrep(" ", .lfs_prefix_width)
    for (line in wrapped[-1L]) {
      cli::cli_verbatim(paste0(indent, line))
    }
  }
}


#' Emit an indented sub-line with a timestamp
#'
#' Renders as: "   HH:MM:SS ⤷  <msg>" in light grey.
#' Long messages are pre-wrapped at .lfs_max_width with continuation lines
#' indented to align under the message column.
#'
#' @keywords internal
#' @noRd
.lfs_sub <- function(msg) {
  msg <- gsub("\\{", "{{", msg, fixed = FALSE)
  msg <- gsub("\\}", "}}", msg, fixed = FALSE)

  available <- .lfs_max_width - .lfs_prefix_width
  wrapped <- strwrap(msg, width = available)

  first <- paste0(
    "   ", cli::col_silver(paste0(.lfs_now(), " \u2937  ", wrapped[1L]))
  )
  cli::cli_verbatim(first)

  if (length(wrapped) > 1L) {
    indent <- strrep(" ", .lfs_prefix_width)
    for (line in wrapped[-1L]) {
      cli::cli_verbatim(cli::col_silver(paste0(indent, line)))
    }
  }
}


#' Emit a phase marker
#'
#' Renders as a chevron-prefixed bold title with a thin underline that
#' matches the title's length:
#'
#'   ❯  Loading microdata
#'      ─────────────────
#'
#' The underline length adjusts to the title length so it sits exactly
#' under the title text. Adds a blank line before for breathing room.
#'
#' @keywords internal
#' @noRd
.lfs_phase <- function(title) {
  cli::cli_verbatim("")
  purple <- cli::make_ansi_style("#b8a4d4", bg = FALSE)

  marker <- cli::col_blue("\u276f")             # ❯
  text   <- cli::col_blue(title)
  cli::cli_verbatim(paste0(marker, "  ", text))

  # Thin underline beneath the title, aligned under the title text.
  # Three-space indent to clear the chevron + two-space gap.
  #underline <- strrep("\u2500", nchar(title))   # ─ × len(title)
  underline <- strrep("\u2500", 25)   # ─ × len(title)
  cli::cli_verbatim(paste0("   ", purple(underline)))
}


#' Emit the final completion line
#'
#' Renders as: "✔ <msg>" with a blank line before and a matching closing
#' rule below. Marks the end of a run.
#'
#' @keywords internal
#' @noRd
.lfs_success <- function(msg) {
  cli::cli_verbatim("")
  msg <- gsub("\\{", "{{", msg, fixed = FALSE)
  msg <- gsub("\\}", "}}", msg, fixed = FALSE)

  marker <- cli::col_green("\u2714")
  cli::cli_verbatim(paste0(marker, "  ", cli::col_blue(msg)))

  underline <- strrep("\u2500", 25)   # ─ × len(title)
    purple <- cli::make_ansi_style("#b8a4d4", bg = FALSE)
  cli::cli_verbatim(paste0("   ", purple(underline)))
  cli::cli_verbatim(" ")
}