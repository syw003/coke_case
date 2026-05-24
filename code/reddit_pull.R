# ============================================================
#  Reddit PullPush pull — Coke Zero vs Pepsi Zero/Max
#  Difference-in-differences: 2017 rebrand & 2021 reformulation
# ============================================================

needed <- c("httr2", "jsonlite", "dplyr", "readr")
to_install <- needed[!vapply(needed, requireNamespace, logical(1L), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)

output_path <- "/Users/selinawu/Desktop/mgt159/coke_case/data/coke_pepsi_reddit_all.csv"

# ── Search terms ─────────────────────────────────────────────
treatment_terms <- c("coke zero", "cokezero", "coca cola zero")
control_terms   <- c("pepsi zero", "pepsizero", "pepsi max", "pepsimax")

# ── Time windows (Unix timestamps) ──────────────────────────
windows <- list(
  "2017_rebrand"      = list(after = as.integer(as.POSIXct("2017-01-01", tz = "UTC")),
                              before = as.integer(as.POSIXct("2018-01-01", tz = "UTC"))),
  "2021_reformulation" = list(after = as.integer(as.POSIXct("2021-01-01", tz = "UTC")),
                               before = as.integer(as.POSIXct("2022-01-01", tz = "UTC")))
)

# ── Constants ────────────────────────────────────────────────
BASE_URL      <- "https://api.pullpush.io/reddit/search"
PAGE_SIZE     <- 100L
SLEEP_BETWEEN <- 1.5        # seconds between pages
SLEEP_WINDOWS <- 60         # seconds between the two windows
REQUEST_TIMEOUT <- 30       # seconds per request

# Backoff schedule for transient errors (seconds)
BACKOFF_SECS  <- c(30, 60, 120, 240, 480, 600)
MAX_RETRIES   <- length(BACKOFF_SECS)

# HTTP status codes treated as transient (retry)
TRANSIENT_CODES <- c(429L, 500L, 502L, 503L, 504L)

# ── Helper: single page request with retry ──────────────────
fetch_page <- function(endpoint, q, after, before) {
  url <- paste0(BASE_URL, "/", endpoint, "/")

  for (attempt in seq_len(MAX_RETRIES + 1L)) {
    resp <- tryCatch({
      request(url) |>
        req_url_query(q          = q,
                      after      = after,
                      before     = before,
                      size       = PAGE_SIZE,
                      sort       = "asc",
                      sort_type  = "created_utc") |>
        req_timeout(REQUEST_TIMEOUT) |>
        req_perform()
    }, error = function(e) {
      # Network-level error (timeout, connection refused, etc.)
      structure(list(message = conditionMessage(e)), class = "fetch_error")
    })

    # ── Network error ────────────────────────────────────────
    if (inherits(resp, "fetch_error")) {
      if (attempt > MAX_RETRIES) {
        message(sprintf("  [SKIP] Network error after %d attempts: %s",
                        MAX_RETRIES, resp$message))
        return(NULL)
      }
      sleep_s <- BACKOFF_SECS[min(attempt, length(BACKOFF_SECS))]
      message(sprintf("  [RETRY %d/%d] Network error — sleeping %ds: %s",
                      attempt, MAX_RETRIES, sleep_s, resp$message))
      Sys.sleep(sleep_s)
      next
    }

    status <- resp_status(resp)

    # ── Transient HTTP error ─────────────────────────────────
    if (status %in% TRANSIENT_CODES) {
      if (attempt > MAX_RETRIES) {
        message(sprintf("  [SKIP] HTTP %d after %d retries — giving up on this page.",
                        status, MAX_RETRIES))
        return(NULL)
      }
      sleep_s <- BACKOFF_SECS[min(attempt, length(BACKOFF_SECS))]
      message(sprintf("  [RETRY %d/%d] HTTP %d — sleeping %ds before retry.",
                      attempt, MAX_RETRIES, status, sleep_s))
      Sys.sleep(sleep_s)
      next
    }

    # ── Non-transient HTTP error ─────────────────────────────
    if (status >= 400L) {
      message(sprintf("  [SKIP] Non-transient HTTP %d — skipping page.", status))
      return(NULL)
    }

    # ── Success ──────────────────────────────────────────────
    body <- tryCatch(resp_body_string(resp), error = function(e) NULL)
    if (is.null(body)) {
      message("  [SKIP] Could not read response body.")
      return(NULL)
    }

    parsed <- tryCatch(fromJSON(body, simplifyVector = FALSE),
                       error = function(e) NULL)
    if (is.null(parsed)) {
      message("  [SKIP] JSON parse error.")
      return(NULL)
    }

    items <- parsed$data
    if (is.null(items)) items <- list()
    return(items)
  }
  NULL
}

# ── Helper: parse one item into a named list ─────────────────
parse_item <- function(item, endpoint, term, group, win_name) {
  is_post <- (endpoint == "submission")
  list(
    id          = as.character(item$id %||% NA_character_),
    type        = if (is_post) "post" else "comment",
    subreddit   = as.character(item$subreddit %||% NA_character_),
    author      = as.character(item$author %||% NA_character_),
    title       = if (is_post) as.character(item$title %||% "") else "",
    text        = as.character(if (is_post) (item$selftext %||% "") else (item$body %||% "")),
    score       = as.integer(item$score %||% NA_integer_),
    comments    = as.integer(if (is_post) (item$num_comments %||% 0L) else 0L),
    created_utc = as.integer(item$created_utc %||% NA_integer_),
    term        = term,
    group       = group,
    window      = win_name
  )
}

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Main pull ────────────────────────────────────────────────
all_rows  <- list()
wall_start <- proc.time()[["elapsed"]]

win_names <- names(windows)

for (w_idx in seq_along(win_names)) {
  win_name <- win_names[[w_idx]]
  win      <- windows[[win_name]]
  message(sprintf("\n====== Window: %s ======", win_name))

  term_list <- c(
    setNames(as.list(treatment_terms), rep("treatment", length(treatment_terms))),
    setNames(as.list(control_terms),   rep("control",   length(control_terms)))
  )
  groups <- c(rep("treatment", length(treatment_terms)),
              rep("control",   length(control_terms)))
  terms  <- c(treatment_terms, control_terms)

  for (t_idx in seq_along(terms)) {
    term  <- terms[[t_idx]]
    group <- groups[[t_idx]]

    for (endpoint in c("submission", "comment")) {
      message(sprintf("\n  [%s] term='%s' endpoint=%s", win_name, term, endpoint))

      after_cursor <- win$after
      page_num     <- 0L
      term_count   <- 0L

      repeat {
        page_num <- page_num + 1L
        items    <- fetch_page(endpoint, term, after_cursor, win$before)

        if (is.null(items) || length(items) == 0L) {
          message(sprintf("    page %d → 0 items — done.", page_num))
          break
        }

        message(sprintf("    page %d → %d items", page_num, length(items)))
        term_count <- term_count + length(items)

        for (item in items) {
          all_rows[[length(all_rows) + 1L]] <- parse_item(item, endpoint, term, group, win_name)
        }

        # Advance cursor to the last item's created_utc
        last_utc <- as.integer(items[[length(items)]]$created_utc %||% NA_integer_)
        if (is.na(last_utc) || last_utc <= after_cursor) {
          message("    cursor did not advance — stopping pagination.")
          break
        }
        after_cursor <- last_utc

        Sys.sleep(SLEEP_BETWEEN)
      }

      message(sprintf("  => Total for (term='%s', endpoint=%s): %d items", term, endpoint, term_count))
    }
  }

  # Pause between windows to let the API breathe
  if (w_idx < length(win_names)) {
    message(sprintf("\n  Pausing %ds between windows ...", SLEEP_WINDOWS))
    Sys.sleep(SLEEP_WINDOWS)
  }
}

# ── Assemble tibble ──────────────────────────────────────────
message("\nAssembling tibble ...")

if (length(all_rows) == 0L) {
  results <- tibble(
    id = character(), type = character(), subreddit = character(),
    author = character(), title = character(), text = character(),
    score = integer(), comments = integer(), created_utc = integer(),
    term = character(), group = character(), window = character()
  )
} else {
  results <- bind_rows(lapply(all_rows, as_tibble))
}

# ── Deduplicate on (id, type) ────────────────────────────────
before_dedup <- nrow(results)
results <- results |>
  group_by(id, type) |>
  slice(1L) |>
  ungroup()
after_dedup <- nrow(results)
message(sprintf("Deduplication: %d → %d rows (removed %d duplicates)",
                before_dedup, after_dedup, before_dedup - after_dedup))

# ── Write output ─────────────────────────────────────────────
write_csv(results, output_path)
message(sprintf("\nWrote %d rows to:\n  %s", nrow(results), output_path))

# ── Summary ─────────────────────────────────────────────────
message("\n── Count summary (window × group × type) ──")
summary_tbl <- results |>
  count(window, group, type, name = "n") |>
  arrange(window, group, type)
print(as.data.frame(summary_tbl), row.names = FALSE)

wall_elapsed <- proc.time()[["elapsed"]] - wall_start
message(sprintf("\nTotal wall-clock time: %.1f minutes (%.0f seconds)",
                wall_elapsed / 60, wall_elapsed))
