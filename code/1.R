library(tidyverse)
library(httr2)
library(jsonlite)

api_key     <- Sys.getenv("OPENAI_API_KEY")
input_path  <- "/Users/codyzheng_1/Desktop/my-r-project/data/coke_pepsi_reddit_trimmed.csv"
output_path <- "/Users/codyzheng_1/Desktop/my-r-project/data/coke_pepsi_reddit_extracted.csv"
checkpoint  <- "/Users/codyzheng_1/Desktop/my-r-project/data/checkpoint_extracted.rds"

reddit <- read_csv(input_path, show_col_types = FALSE)

# Resume from checkpoint
if (file.exists(checkpoint)) {
  done <- readRDS(checkpoint)
  cat(sprintf("Resuming: %d rows already done.\n", nrow(done)))
  todo <- reddit |> filter(!id %in% done$id)
} else {
  done <- tibble()
  todo <- reddit
}

cat(sprintf("Rows to classify: %d\n", nrow(todo)))

classify_row <- function(text, focal_beverage, retries = 5) {
  prompt <- sprintf(
    'Is the following Reddit post or comment a complaint specifically about %s?

A complaint = negative sentiment directed at %s itself (taste, formula, availability, health, price, etc).
Neutral mentions, recipes, questions, or praise of other brands do NOT count.

If it IS a complaint, extract a verbatim phrase of exactly 3-8 words from the text that names the complaint subject.
If NOT a complaint, return empty string for complaint_subject.

Text:
"""%s"""',
    focal_beverage, focal_beverage, text
  )

  schema <- list(
    type = "json_schema",
    json_schema = list(
      name   = "complaint_result",
      strict = TRUE,
      schema = list(
        type = "object",
        properties = list(
          is_complaint      = list(type = "boolean"),
          complaint_subject = list(type = "string")
        ),
        required             = list("is_complaint", "complaint_subject"),
        additionalProperties = FALSE
      )
    )
  )

  body <- list(
    model            = "gpt-4o-mini",
    reasoning_effort = "low",
    response_format  = schema,
    messages = list(list(role = "user", content = prompt))
  )

  wait <- 30
  for (attempt in seq_len(retries)) {
    result <- tryCatch({
      resp <- request("https://api.openai.com/v1/chat/completions") |>
        req_headers(Authorization = paste("Bearer", api_key),
                    `Content-Type` = "application/json") |>
        req_body_json(body) |>
        req_timeout(60) |>
        req_perform()
      raw    <- resp |> resp_body_json()
      parsed <- fromJSON(raw$choices[[1]]$message$content)
      list(is_complaint = parsed$is_complaint,
           complaint_subject = parsed$complaint_subject)
    }, error = function(e) {
      cat(sprintf("  Attempt %d failed: %s — sleeping %ds\n", attempt, conditionMessage(e), wait))
      Sys.sleep(wait)
      wait <<- min(wait * 2, 600)
      NULL
    })
    if (!is.null(result)) return(result)
  }
  list(is_complaint = NA, complaint_subject = NA_character_)
}

rows_list <- vector("list", nrow(todo))

for (i in seq_len(nrow(todo))) {
  row      <- todo[i, ]
  focal    <- if (row$group == "treatment") "Coke Zero" else "Pepsi Zero/Pepsi Max"
  text_val <- if (is.na(row$text) || nchar(trimws(row$text)) == 0) "[no text]" else row$text

  rows_list[[i]] <- bind_cols(
    tibble(id = row$id),
    as_tibble(classify_row(text_val, focal))
  )

  if (i %% 100 == 0) cat(sprintf("[%d / %d]\n", i, nrow(todo)))

  if (i %% 500 == 0 || i == nrow(todo)) {
    partial  <- bind_rows(rows_list[1:i])
    combined <- bind_rows(done, left_join(todo[1:i,], partial, by = "id"))
    saveRDS(combined, checkpoint)
    cat(sprintf("Checkpoint saved: %d rows total\n", nrow(combined)))
  }
}

results    <- bind_rows(rows_list)
reddit_out <- left_join(todo, results, by = "id") |> bind_rows(done)
write_csv(reddit_out, output_path)

cat(sprintf(
"\n=== Summary ===
Total rows        : %d
Complaints        : %d (%.1f%%)
Non-empty subject : %d\n",
  nrow(reddit_out),
  sum(reddit_out$is_complaint == TRUE, na.rm = TRUE),
  mean(reddit_out$is_complaint == TRUE, na.rm = TRUE) * 100,
  sum(nchar(reddit_out$complaint_subject) > 0, na.rm = TRUE)
))