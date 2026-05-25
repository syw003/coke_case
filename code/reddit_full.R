library(tidyverse)
library(httr2)
library(jsonlite)

readRenviron("~/.Renviron")

api_key <- Sys.getenv("OPENAI_API_KEY")

input_path <- "/Users/codyzheng_1/Desktop/my-r-project/data/coke_pepsi_reddit_trimmed.csv"
output_path <- "/Users/codyzheng_1/Desktop/my-r-project/data/coke_pepsi_reddit_extracted.csv"
checkpoint_path <- "/Users/codyzheng_1/Desktop/my-r-project/data/checkpoint_extracted.rds"

reddit <- read_csv(input_path, show_col_types = FALSE)

todo <- reddit

cat("Rows to classify:", nrow(todo), "\n")

classify_row <- function(text, focal_beverage, retries = 5) {
  prompt <- paste0(
    "Is this Reddit text a complaint about ", focal_beverage, "?\n\n",
    "A complaint means negative sentiment about the focal beverage only.\n",
    "Examples: taste, formula, price, health, availability.\n",
    "Neutral mentions are NOT complaints.\n",
    "Praise of other brands is NOT a complaint.\n",
    "Complaints about a different brand are NOT complaints.\n\n",
    "If complaint, extract a verbatim 3-8 word phrase from the text naming the complaint subject.\n",
    "If not complaint, use empty string for complaint_subject.\n\n",
    "Return ONLY valid JSON in this format:\n",
    "{\"is_complaint\": true, \"complaint_subject\": \"example phrase\"}\n\n",
    "Text:\n",
    text
  )

  body <- list(
    model = "gpt-5-mini",
    messages = list(
      list(role = "user", content = prompt)
    )
  )

  wait <- 10

  for (attempt in seq_len(retries)) {
    result <- tryCatch({
      resp <- request("https://api.openai.com/v1/chat/completions") |>
        req_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ) |>
        req_body_json(body) |>
        req_timeout(90) |>
        req_perform()

      content <- resp |>
        resp_body_json() |>
        _$choices[[1]]$message$content

      parsed <- fromJSON(content)

      tibble(
        is_complaint = parsed$is_complaint,
        complaint_subject = parsed$complaint_subject
      )

    }, error = function(e) {
      cat(sprintf(
        "Attempt %d failed: %s — sleeping %ds\n",
        attempt, conditionMessage(e), wait
      ))
      Sys.sleep(wait)
      wait <<- min(wait * 2, 120)
      NULL
    })

    if (!is.null(result)) return(result)
  }

  tibble(
    is_complaint = NA,
    complaint_subject = NA_character_
  )
}

if (file.exists(checkpoint_path)) {
  results_list <- readRDS(checkpoint_path)
  cat("Loaded checkpoint\n")
} else {
  results_list <- vector("list", nrow(todo))
}

start_i <- which(map_lgl(results_list, is.null))[1]

if (is.na(start_i)) {
  cat("All rows already classified in checkpoint.\n")
} else {
  for (i in start_i:nrow(todo)) {
    row <- todo[i, ]

    focal <- if (row$group == "treatment") {
      "Coke Zero"
    } else {
      "Pepsi Zero/Pepsi Max"
    }

    text_val <- ifelse(
      is.na(row$text) || nchar(trimws(row$text)) == 0,
      "[no text]",
      row$text
    )

    results_list[[i]] <- classify_row(text_val, focal)

    if (i %% 25 == 0) {
      saveRDS(results_list, checkpoint_path)
      cat(sprintf("[%d / %d] checkpoint saved\n", i, nrow(todo)))
    }
  }
}

saveRDS(results_list, checkpoint_path)

results <- bind_rows(results_list)

reddit_out <- bind_cols(
  todo,
  results
)

write_csv(reddit_out, output_path)

cat(sprintf(
  "\n=== Summary ===
Total rows        : %d
Complaints        : %d (%.1f%%)
Failed / NA       : %d
Non-empty subject : %d\n",
  nrow(reddit_out),
  sum(reddit_out$is_complaint == TRUE, na.rm = TRUE),
  mean(reddit_out$is_complaint == TRUE, na.rm = TRUE) * 100,
  sum(is.na(reddit_out$is_complaint)),
  sum(nchar(reddit_out$complaint_subject) > 0, na.rm = TRUE)
))