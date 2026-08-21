#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(file_arg) != 1L) {
  stop("Could not determine the script directory.", call. = FALSE)
}
script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg)))
source(file.path(script_dir, "chooser.R"))

usage <- function() {
  paste(
    "Usage:",
    "  Rscript choose_presenters.R <count>",
    "  Rscript choose_presenters.R <count> --replace <absent-name>",
    sep = "\n"
  )
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L && !(length(args) == 3L && args[2] == "--replace")) {
  stop(usage(), call. = FALSE)
}
count <- suppressWarnings(as.integer(args[1]))
if (is.na(count) || as.character(count) != args[1]) {
  stop("Count must be a positive whole number.\n\n", usage(), call. = FALSE)
}
absent_name <- if (length(args) == 3L) args[3] else NULL

roster_path <- file.path(script_dir, "roster.csv")
history_path <- file.path(script_dir, "presentation_history.csv")
today <- Sys.Date()
today_text <- format(today, "%Y-%m-%d")
roster <- read_roster(roster_path)
count <- validate_count(count, length(roster))
history <- read_history(history_path)
saved <- presenters_for_date(history, today)

if (is.null(absent_name)) {
  if (length(saved) > 0L) {
    cat(sprintf("Presenters already selected for %s:\n", today_text))
    cat(paste0("  - ", saved), sep = "\n")
    cat(sprintf("\nReused saved result from %s\n", history_path))
  } else {
    presenters <- draw_presenters(roster, history, count, today)
    history <- record_draw(history, presenters, today)
    write_history(history, history_path)
    cat(sprintf("Presenters for %s:\n", today_text))
    cat(paste0("  - ", presenters), sep = "\n")
    cat(sprintf("\nSaved result to %s\n", history_path))
  }
} else {
  absent_name <- trimws(absent_name)
  if (!(absent_name %in% saved)) {
    stop(sprintf("'%s' is not a saved presenter for %s.", absent_name, today_text), call. = FALSE)
  }
  # Do not count the absent student as having presented; remove only today's row.
  history <- history[!(history$date == today_text & history$name == absent_name), , drop = FALSE]
  replacement <- draw_presenters(roster, history, 1L, today, exclude = c(saved, absent_name))
  history <- record_draw(history, replacement, today)
  write_history(history, history_path)
  cat(sprintf("Replacement for %s on %s:\n  - %s\n", absent_name, today_text, replacement))
  cat(sprintf("Updated result saved to %s\n", history_path))
}
