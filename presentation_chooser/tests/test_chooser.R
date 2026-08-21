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

cat("All presentation chooser tests passed.\n")
