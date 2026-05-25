library(tidyverse)
library(httr2)
library(jsonlite)

readRenviron("~/.Renviron")
api_key <- Sys.getenv("OPENAI_API_KEY")

data_path <- "/Users/codyzheng_1/Desktop/my-r-project/data/coke_pepsi_reddit_extracted_42000.csv"
meta_path <- "/Users/codyzheng_1/Desktop/my-r-project/data/coke_pepsi_reddit_buckets_meta.json"
out_path  <- "/Users/codyzheng_1/Desktop/my-r-project/data/coke_pepsi_reddit_bucketed.csv"

if (api_key == "") {
  stop("OPENAI_API_KEY is empty. Check ~/.Renviron and restart R.")
}

extracted <- read_csv(data_path, show_col_types = FALSE)

phrases <- extracted |>
  filter(
    group == "treatment",
    is_complaint == TRUE,
    !is.na(complaint_subject),
    nchar(complaint_subject) > 0
  ) |>
  distinct(complaint_subject) |>
  pull(complaint_subject)

cat(sprintf("Unique complaint phrases: %d\n", length(phrases)))

if (file.exists(meta_path)) {
  buckets <- fromJSON(meta_path)
  bucket_names <- buckets$name
  cat(sprintf("Loaded existing bucket schema (%d buckets).\n", length(bucket_names)))
  print(bucket_names)
} else {
  phrases_text <- paste(phrases, collapse = "\n")

  prompt_discover <- paste0(
    "You are an expert qualitative researcher. Below are ~", length(phrases),
    " complaint phrases extracted from Reddit posts about Coke Zero.\n\n",
    "Propose exactly 16 mutually exclusive bucket categories that cover these complaints.\n\n",
    "Guidelines:\n",
    "- Each bucket should substantively and thematically group multiple raw terms.\n",
    "- Avoid vague catch-all buckets like 'miscellaneous' or 'other'.\n",
    "- Strongly prefer descriptive, specific, and directional bucket terms.\n",
    "- Mutual exclusivity: each bucket must be clearly distinct from others.\n\n",
    "Return ONLY valid JSON in this exact format:\n",
    "[{\"name\": \"bucket name\", \"definition\": \"short definition\"}, ...]\n\n",
    "Phrases:\n", phrases_text
  )

  cat("Running Stage 1: bucket discovery with gpt-5.5...\n")

  resp1 <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(
      model = "gpt-5.5",
      reasoning_effort = "high",
      messages = list(
        list(role = "user", content = prompt_discover)
      )
    )) |>
    req_timeout(300) |>
    req_perform() |>
    resp_body_json()

  buckets_raw <- resp1$choices[[1]]$message$content
  buckets_clean <- gsub("```json|```", "", buckets_raw)
  buckets <- fromJSON(buckets_clean)
  bucket_names <- buckets$name

  cat(sprintf("Discovered %d buckets:\n", length(bucket_names)))
  print(bucket_names)

  write_json(buckets, meta_path, pretty = TRUE, auto_unbox = TRUE)
  cat(sprintf("Saved bucket schema to %s\n", meta_path))
}

assign_bucket_batch <- function(phrases_batch, bucket_names, api_key, retries = 3) {
  phrase_ids <- seq_along(phrases_batch)

  prompt <- paste0(
    "Assign each complaint phrase to exactly one of the allowed buckets.\n\n",
    "Allowed buckets:\n",
    paste(bucket_names, collapse = "\n"),
    "\n\nReturn ONLY valid JSON as an array. Each item must have:\n",
    "- id: the numeric id I provided\n",
    "- complaint_bucket: one exact bucket name from the allowed bucket list\n\n",
    "Important rules:\n",
    "- Return one item for every phrase.\n",
    "- Do not skip any id.\n",
    "- Do not invent bucket names.\n",
    "- Use the exact bucket spelling.\n\n",
    "Phrases:\n",
    paste(sprintf("%s. %s", phrase_ids, phrases_batch), collapse = "\n")
  )

  wait <- 5

  for (attempt in seq_len(retries)) {
    result <- tryCatch({
      resp <- request("https://api.openai.com/v1/chat/completions") |>
        req_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ) |>
        req_body_json(list(
          model = "gpt-5.4-mini",
          reasoning_effort = "low",
          messages = list(
            list(role = "user", content = prompt)
          )
        )) |>
        req_timeout(180) |>
        req_perform() |>
        resp_body_json()

      content <- resp$choices[[1]]$message$content
      content <- gsub("```json|```", "", content)
      parsed <- fromJSON(content)

      parsed_tbl <- as_tibble(parsed) |>
        mutate(id = as.integer(id)) |>
        filter(id %in% phrase_ids) |>
        distinct(id, .keep_all = TRUE) |>
        select(id, complaint_bucket)

      full_tbl <- tibble(
        id = phrase_ids,
        complaint_subject = phrases_batch
      ) |>
        left_join(parsed_tbl, by = "id")

      bad_bucket <- !is.na(full_tbl$complaint_bucket) &
        !(full_tbl$complaint_bucket %in% bucket_names)

      full_tbl$complaint_bucket[bad_bucket] <- NA_character_

      full_tbl

    }, error = function(e) {
      message(sprintf("Batch attempt %d failed: %s", attempt, e$message))
      Sys.sleep(wait)
      wait <<- min(wait * 2, 60)
      NULL
    })

    if (!is.null(result)) {
      return(result)
    }
  }

  tibble(
    id = phrase_ids,
    complaint_subject = phrases_batch,
    complaint_bucket = NA_character_
  )
}

cat(sprintf("Running FAST batch Stage 2: assigning %d phrases...\n", length(phrases)))

batch_size <- 100
phrase_batches <- split(phrases, ceiling(seq_along(phrases) / batch_size))

all_results <- list()

for (i in seq_along(phrase_batches)) {
  cat(sprintf("\nRunning batch %d / %d\n", i, length(phrase_batches)))

  batch_result <- assign_bucket_batch(
    phrases_batch = phrase_batches[[i]],
    bucket_names = bucket_names,
    api_key = api_key
  )

  all_results[[i]] <- batch_result

  temp_phrase_buckets <- bind_rows(all_results) |>
    select(complaint_subject, complaint_bucket)

  temp_reddit_bucketed <- extracted |>
    left_join(temp_phrase_buckets, by = "complaint_subject")

  write_csv(temp_reddit_bucketed, out_path)

  cat(sprintf("Saved progress after batch %d / %d\n", i, length(phrase_batches)))
}

phrase_buckets <- bind_rows(all_results) |>
  select(complaint_subject, complaint_bucket)

missing_count <- sum(is.na(phrase_buckets$complaint_bucket))

cat(sprintf("\nInitial batch assignment complete. Missing buckets: %d\n", missing_count))

if (missing_count > 0) {
  cat("Retrying missing phrases one more time in smaller batches...\n")

  missing_phrases <- phrase_buckets |>
    filter(is.na(complaint_bucket)) |>
    pull(complaint_subject)

  retry_batches <- split(missing_phrases, ceiling(seq_along(missing_phrases) / 25))
  retry_results <- list()

  for (j in seq_along(retry_batches)) {
    cat(sprintf("Retry batch %d / %d\n", j, length(retry_batches)))

    retry_results[[j]] <- assign_bucket_batch(
      phrases_batch = retry_batches[[j]],
      bucket_names = bucket_names,
      api_key = api_key
    )
  }

  retry_phrase_buckets <- bind_rows(retry_results) |>
    select(complaint_subject, complaint_bucket) |>
    filter(!is.na(complaint_bucket))

  phrase_buckets <- phrase_buckets |>
    select(complaint_subject, complaint_bucket) |>
    rows_update(
      retry_phrase_buckets,
      by = "complaint_subject",
      unmatched = "ignore"
    )
}

reddit_bucketed <- extracted |>
  left_join(phrase_buckets, by = "complaint_subject")

write_csv(reddit_bucketed, out_path)

cat(sprintf("\nDone. Wrote %d rows to %s\n", nrow(reddit_bucketed), out_path))

cat("\nBucket counts (Coke Zero complaints):\n")

reddit_bucketed |>
  filter(group == "treatment", is_complaint == TRUE) |>
  count(complaint_bucket, sort = TRUE) |>
  print(n = 20)