# Functions for selecting class presenters. Uses only base R.

read_roster <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Roster file not found: %s", path), call. = FALSE)
  }

  roster <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop(sprintf("Could not read roster CSV: %s", e$message), call. = FALSE)
  )

  if (!identical(names(roster), "name")) {
    stop("Roster CSV must contain exactly one column named 'name'.", call. = FALSE)
  }
  roster$name <- trimws(roster$name)
  if (nrow(roster) == 0L || anyNA(roster$name) || any(roster$name == "")) {
    stop("Roster must contain at least one non-empty name.", call. = FALSE)
  }
  if (anyDuplicated(roster$name)) {
    stop("Roster names must be unique.", call. = FALSE)
  }
  roster$name
}

read_history <- function(path) {
  if (!file.exists(path)) {
    return(data.frame(date = character(), name = character(), stringsAsFactors = FALSE))
  }
  history <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop(sprintf("Could not read history CSV: %s", e$message), call. = FALSE)
  )
  if (!identical(names(history), c("date", "name"))) {
    stop("History CSV must contain columns named 'date' and 'name'.", call. = FALSE)
  }
  history$date <- as.character(history$date)
  history$name <- as.character(history$name)
  if (anyNA(history$date) || anyNA(history$name) || any(history$date == "") || any(history$name == "")) {
    stop("History CSV contains empty dates or names.", call. = FALSE)
  }
  parsed_dates <- as.Date(history$date)
  if (anyNA(parsed_dates)) {
    stop("History CSV contains an invalid date; use YYYY-MM-DD.", call. = FALSE)
  }
  history$date <- format(parsed_dates, "%Y-%m-%d")
  history
}

write_history <- function(history, path) {
  utils::write.csv(history, path, row.names = FALSE, quote = TRUE)
}

validate_count <- function(count, roster_size) {
  if (length(count) != 1L || is.na(count) || count < 1L || count != as.integer(count)) {
    stop("Count must be a positive whole number.", call. = FALSE)
  }
  if (count > roster_size) {
    stop(sprintf("Count (%d) cannot exceed the roster size (%d).", count, roster_size), call. = FALSE)
  }
  as.integer(count)
}

presenters_for_date <- function(history, date) {
  history$name[history$date == format(as.Date(date), "%Y-%m-%d")]
}

selection_weights <- function(roster, history, date) {
  date <- as.Date(date)
  prior <- history[as.Date(history$date) < date, , drop = FALSE]
  prior_dates <- sort(unique(as.Date(prior$date)))
  weights <- vapply(roster, function(student) {
    selected_dates <- as.Date(prior$date[prior$name == student])
    if (length(selected_dates) == 0L) {
      missed_days <- length(prior_dates)
    } else {
      missed_days <- sum(prior_dates > max(selected_dates))
    }
    1 + 0.10 * missed_days
  }, numeric(1))
  stats::setNames(weights, roster)
}

draw_presenters <- function(roster, history, count, date, exclude = character()) {
  count <- validate_count(count, length(roster))
  candidates <- setdiff(roster, exclude)
  if (count > length(candidates)) {
    stop("Not enough eligible students available for this draw.", call. = FALSE)
  }
  weights <- selection_weights(roster, history, date)[candidates]
  sample(candidates, size = count, replace = FALSE, prob = weights)
}

record_draw <- function(history, presenters, date) {
  rbind(
    history,
    data.frame(date = format(as.Date(date), "%Y-%m-%d"), name = presenters, stringsAsFactors = FALSE)
  )
}
