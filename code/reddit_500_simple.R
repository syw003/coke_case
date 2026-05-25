library(tidyverse)
library(httr2)
library(jsonlite)

readRenviron("~/.Renviron")

api_key <- Sys.getenv("OPENAI_API_KEY")

input_path <- "/Users/codyzheng_1/Desktop/my-r-project/data/coke_pepsi_reddit_trimmed.csv"

output_path <- "/Users/codyzheng_1/Desktop/my-r-project/data/coke_pepsi_reddit_extracted_500.csv"

reddit <- read_csv(input_path, show_col_types = FALSE)

todo <- reddit |>
  slice_sample(n = 500)

cat("Rows to classify:", nrow(todo), "\n")

classify_row <- function(text, focal_beverage) {

  prompt <- paste0(
    "Is this Reddit text a complaint about ",
    focal_beverage,
    "?\n\n",
    
    "A complaint means negative sentiment about the beverage.\n",
    "Examples: taste, formula, price, health, availability.\n\n",
    
    "Neutral mentions are NOT complaints.\n",
    "Praise of other brands is NOT a complaint.\n\n",
    
    "Return ONLY valid JSON.\n\n",
    
    "Format:\n",
    "{\"is_complaint\": true, \"complaint_subject\": \"example phrase\"}\n\n",
    
    "If not complaint, use empty string for complaint_subject.\n\n",
    
    "Text:\n",
    text
  )

  body <- list(
    model = "gpt-5-mini",
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    ),
    temperature = 0
  )

  tryCatch({

    resp <- request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ) |>
      req_body_json(body) |>
      req_timeout(60) |>
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

    cat("ERROR:\n")
    print(e)

    tibble(
      is_complaint = NA,
      complaint_subject = NA_character_
    )
  })
}

results_list <- vector("list", nrow(todo))

for (i in seq_len(nrow(todo))) {

  row <- todo[i, ]

  focal <- if (row$group == "treatment") {
    "Coke Zero"
  } else {
    "Pepsi Zero/Pepsi Max"
  }

  text_val <- ifelse(
    is.na(row$text),
    "",
    row$text
  )

  results_list[[i]] <- classify_row(
    text_val,
    focal
  )

  if (i %% 25 == 0) {
    cat(i, "/", nrow(todo), "\n")
  }
}

results <- bind_rows(results_list)

reddit_out <- bind_cols(
  todo,
  results
)

cat("FINAL ROWS:", nrow(reddit_out), "\n")

write_csv(
  reddit_out,
  output_path
)

cat("DONE\n")