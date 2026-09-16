file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(file_arg) != 1L) stop("Could not determine the test directory.", call. = FALSE)
test_dir <- dirname(normalizePath(sub("^--file=", "", file_arg)))
source(file.path(test_dir, "..", "chooser.R"))

expect_error <- function(expr, pattern) {
  message <- tryCatch({ force(expr); NULL }, error = function(e) e$message)
  stopifnot(!is.null(message), grepl(pattern, message, fixed = TRUE))
}

roster <- c("A", "B", "C", "D")
empty_history <- data.frame(date = character(), name = character(), stringsAsFactors = FALSE)

# First run: every weight is exactly uniform and a draw contains no duplicates.
stopifnot(identical(unname(selection_weights(roster, empty_history, as.Date("2026-01-01"))), rep(1, 4)))
set.seed(42)
first_draw <- draw_presenters(roster, empty_history, 3L, as.Date("2026-01-01"))
stopifnot(length(first_draw) == 3L, length(unique(first_draw)) == 3L)

# Waiting longer raises a student's weight by 10% per missed presentation day.
history <- data.frame(
  date = c("2026-01-01", "2026-01-08"),
  name = c("A", "B"),
  stringsAsFactors = FALSE
)
weights <- selection_weights(roster, history, as.Date("2026-01-15"))
stopifnot(weights[["A"]] == 1.1, weights[["B"]] == 1, weights[["C"]] == 1.2)

# Input validation and replacement eligibility.
expect_error(validate_count(0L, 4L), "positive whole")
expect_error(validate_count(5L, 4L), "cannot exceed")
expect_error(draw_presenters(roster, empty_history, 2L, as.Date("2026-01-01"), exclude = c("A", "B", "C")), "Not enough")

tmp <- tempfile(fileext = ".csv")
writeLines(c("student", "A"), tmp)
expect_error(read_roster(tmp), "exactly one column")
unlink(tmp)

# Exercise the actual CLI against temporary files, leaving class history untouched.
test_same_day_expansion <- function() {
  sandbox <- tempfile("presentation-chooser-")
  dir.create(sandbox)
  on.exit(unlink(sandbox, recursive = TRUE), add = TRUE)
  scripts <- file.path(test_dir, "..", c("chooser.R", "choose_presenters.R"))
  stopifnot(all(file.copy(scripts, sandbox)))
  test_roster <- LETTERS[1:6]
  utils::write.csv(data.frame(name = test_roster), file.path(sandbox, "roster.csv"), row.names = FALSE)
  history_path <- file.path(sandbox, "presentation_history.csv")
  today <- Sys.Date()
  prior <- record_draw(empty_history, "A", today - 1L)
  write_history(prior, history_path)

  run_chooser <- function(count) {
    output <- system2(
      file.path(R.home("bin"), "Rscript"),
      c(shQuote(file.path(sandbox, "choose_presenters.R")), as.character(count)),
      stdout = TRUE, stderr = TRUE
    )
    stopifnot(is.null(attr(output, "status")))
    output
  }

  run_chooser(3L)
  original_history <- read_history(history_path)
  original <- presenters_for_date(original_history, today)
  stopifnot(length(original) == 3L, !anyDuplicated(original))

  output <- run_chooser(4L)
  expanded_history <- read_history(history_path)
  expanded <- presenters_for_date(expanded_history, today)
  stopifnot(
    length(expanded) == 4L,
    !anyDuplicated(expanded),
    identical(expanded[1:3], original),
    identical(expanded_history[seq_len(nrow(original_history)), ], original_history),
    identical(output[startsWith(output, "  - ")], paste0("  - ", expanded))
  )

  # Repeating or reducing the requested total must not redraw or rewrite history.
  saved_file <- readLines(history_path)
  for (count in c(4L, 2L)) {
    output <- run_chooser(count)
    stopifnot(
      identical(readLines(history_path), saved_file),
      identical(output[startsWith(output, "  - ")], paste0("  - ", expanded))
    )
  }

  # A larger increase fills all remaining slots without duplicating saved names.
  run_chooser(6L)
  full <- presenters_for_date(read_history(history_path), today)
  stopifnot(length(full) == 6L, setequal(full, test_roster), identical(full[1:4], expanded))
}
test_same_day_expansion()

cat("All presentation chooser tests passed.\n")
