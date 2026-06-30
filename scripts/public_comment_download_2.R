# Set rec.gov API Key in your console or .Renviron file before running
# Sys.setenv(REGULATIONS_API_KEY = "Your API Key Here")

# ============================================================
# Download all public comments for docket BOEM-2025-0318
# Regulations.gov API v4
# ============================================================

# Install if needed:
# install.packages(c(
#   "httr2", "jsonlite", "dplyr", "purrr",
#   "tibble", "readr", "stringr"
# ))

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)

# ----------------------------
# USER SETTINGS
# ----------------------------

API_KEY   <- Sys.getenv("REGULATIONS_API_KEY")  # set this in .Renviron or manually
DOCKET_ID <- "BOEM-2025-0318"
BASE_URL  <- "https://api.regulations.gov/v4"

# IMPORTANT:
# The docs identify 429 as the rate-limit error. The API also has request limits,
# so this script goes slowly and checkpoints progress.

REQUEST_PAUSE <- 4.0

if (API_KEY == "") {
  stop("No API key found. Run Sys.setenv(REGULATIONS_API_KEY = 'your_key_here') first.")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ----------------------------
# Safe API request helper
# ----------------------------

reg_get <- function(path, query = list(), pause = REQUEST_PAUSE, max_tries = 6) {
  url <- paste0(BASE_URL, path)
  
  for (try_i in seq_len(max_tries)) {
    Sys.sleep(pause)
    
    req <- request(url) |>
      req_headers(`X-Api-Key` = API_KEY) |>
      req_url_query(!!!query) |>
      req_error(is_error = function(resp) FALSE)
    
    resp <- tryCatch(req_perform(req), error = function(e) e)
    
    if (inherits(resp, "error")) {
      message("Request error on try ", try_i, ": ", resp$message)
      Sys.sleep(10 * try_i)
      next
    }
    
    status <- resp_status(resp)
    
    if (status == 200) {
      txt <- resp_body_string(resp)
      return(list(
        ok = TRUE,
        status = status,
        json = fromJSON(txt, simplifyVector = FALSE),
        body = txt
      ))
    }
    
    body_txt <- tryCatch(resp_body_string(resp), error = function(e) NA_character_)
    
    if (status == 429) {
      message("Rate limited: 429. Waiting before retry...")
      Sys.sleep(60 * try_i)
      next
    }
    
    if (status >= 500) {
      message("Server error ", status, ". Retrying...")
      Sys.sleep(20 * try_i)
      next
    }
    
    return(list(
      ok = FALSE,
      status = status,
      json = NULL,
      body = body_txt
    ))
  }
  
  list(
    ok = FALSE,
    status = NA_integer_,
    json = NULL,
    body = "Max retries exceeded"
  )
}

# ----------------------------
# Convert list of API data records into tibble
# ----------------------------

records_to_tibble <- function(records) {
  if (is.null(records) || length(records) == 0) {
    return(tibble())
  }
  
  map_dfr(records, function(x) {
    attrs <- x$attributes
    
    tibble(
      id                 = x$id %||% NA_character_,
      type               = x$type %||% NA_character_,
      title              = attrs$title %||% NA_character_,
      docket_id          = attrs$docketId %||% NA_character_,
      document_id        = attrs$documentId %||% NA_character_,
      comment_on_id      = attrs$commentOnId %||% NA_character_,
      object_id          = attrs$objectId %||% NA_character_,
      document_type      = attrs$documentType %||% NA_character_,
      posted_date        = attrs$postedDate %||% NA_character_,
      receive_date       = attrs$receiveDate %||% NA_character_,
      last_modified_date = attrs$lastModifiedDate %||% NA_character_,
      tracking_nbr       = attrs$trackingNbr %||% NA_character_,
      comment_text       = attrs$comment %||% NA_character_,
      withdrawn          = attrs$withdrawn %||% NA
    )
  })
}

# ----------------------------
# Step 1: get documents for docket
# ----------------------------

get_docket_documents <- function(docket_id) {
  message("Getting documents for docket: ", docket_id)
  
  res <- reg_get(
    "/documents",
    query = list(
      `filter[docketId]` = docket_id,
      `page[size]` = 250
    )
  )
  
  if (!res$ok) stop("Document request failed: ", res$status, " | ", res$body)
  
  docs <- records_to_tibble(res$json$data)
  
  docs |>
    distinct(id, .keep_all = TRUE)
}

# ----------------------------
# Step 2: get all comment IDs for each document objectId
# ----------------------------

get_comments_for_object <- function(comment_on_id) {
  out <- list()
  
  for (page_num in 1:20) {
    message("  Comment list page ", page_num, " for objectId: ", comment_on_id)
    
    res <- reg_get(
      "/comments",
      query = list(
        `filter[commentOnId]` = comment_on_id,
        `page[size]` = 250,
        `page[number]` = page_num,
        sort = "lastModifiedDate,documentId"
      )
    )
    
    if (!res$ok) {
      warning("Failed comment list request for objectId ", comment_on_id, ": ", res$status)
      break
    }
    
    page_tbl <- records_to_tibble(res$json$data)
    
    if (nrow(page_tbl) == 0) break
    
    out[[page_num]] <- page_tbl
    
    if (nrow(page_tbl) < 250) break
  }
  
  bind_rows(out) |>
    distinct(id, .keep_all = TRUE)
}

# ----------------------------
# Step 3: get detailed comment text + attachment info
# ----------------------------

safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.list(x)) return(paste(unlist(x), collapse = " | "))
  as.character(x)[1]
}

safe_num <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x)[1])
}

parse_comment_detail <- function(comment_id, res) {
  if (!res$ok) {
    return(list(
      comment = tibble(
        comment_id = comment_id,
        request_ok = FALSE,
        http_status = res$status,
        comment_text = NA_character_
      ),
      attachments = tibble(
        comment_id = character(),
        attachment_id = character(),
        attachment_title = character(),
        file_format = character(),
        mime_type = character(),
        size = double(),
        url = character(),
        download_url = character(),
        view_url = character()
      ),
      failure = tibble(
        comment_id = comment_id,
        status = res$status,
        body = res$body %||% NA_character_
      )
    ))
  }
  
  dat <- res$json$data
  attrs <- dat$attributes
  
  comment_row <- tibble(
    comment_id         = safe_chr(dat$id %||% comment_id),
    request_ok         = TRUE,
    http_status        = 200L,
    docket_id          = safe_chr(attrs$docketId),
    document_id        = safe_chr(attrs$documentId),
    comment_on_id      = safe_chr(attrs$commentOnId),
    title              = safe_chr(attrs$title),
    comment_text       = safe_chr(attrs$comment),
    posted_date        = safe_chr(attrs$postedDate),
    receive_date       = safe_chr(attrs$receiveDate),
    last_modified_date = safe_chr(attrs$lastModifiedDate),
    tracking_nbr       = safe_chr(attrs$trackingNbr),
    withdrawn          = attrs$withdrawn %||% NA,
    organization       = safe_chr(attrs$organization),
    first_name         = safe_chr(attrs$firstName),
    last_name          = safe_chr(attrs$lastName),
    city               = safe_chr(attrs$city),
    state              = safe_chr(attrs$stateProvinceRegion),
    country            = safe_chr(attrs$country)
  )
  
  attachment_rows <- tibble(
    comment_id = character(),
    attachment_id = character(),
    attachment_title = character(),
    file_format = character(),
    mime_type = character(),
    size = double(),
    url = character(),
    download_url = character(),
    view_url = character()
  )
  
  if (!is.null(res$json$included) && length(res$json$included) > 0) {
    attachment_rows <- map_dfr(res$json$included, function(x) {
      a <- x$attributes
      
      tibble(
        comment_id       = safe_chr(dat$id %||% comment_id),
        attachment_id    = safe_chr(x$id),
        attachment_title = safe_chr(a$title),
        file_format      = safe_chr(a$fileFormat),
        mime_type        = safe_chr(a$mimeType),
        size             = safe_num(a$size),
        url              = safe_chr(a$url),
        download_url     = safe_chr(a$downloadUrl),
        view_url         = safe_chr(a$viewUrl)
      )
    })
  }
  
  list(
    comment = comment_row,
    attachments = attachment_rows,
    failure = tibble()
  )
}

get_comment_detail <- function(comment_id) {
  res <- reg_get(
    path = paste0("/comments/", comment_id),
    query = list(include = "attachments")
  )
  
  parse_comment_detail(comment_id, res)
}

# ----------------------------
# Step 4: run full workflow
# ----------------------------

documents <- get_docket_documents(DOCKET_ID)

write_csv(documents, paste0(DOCKET_ID, "_documents.csv"))

documents_to_query <- documents |>
  filter(!is.na(object_id), object_id != "")

comment_list <- map_dfr(documents_to_query$object_id, function(obj_id) {
  message("Getting comments for document objectId: ", obj_id)
  get_comments_for_object(obj_id)
})

comment_list <- comment_list |>
  distinct(id, .keep_all = TRUE)

write_csv(comment_list, paste0(DOCKET_ID, "_comment_list.csv"))

message("Total unique comments found: ", nrow(comment_list))

# ----------------------------
# Step 5: resume from checkpoint if present
# ----------------------------

checkpoint_comments_file <- paste0(DOCKET_ID, "_checkpoint_comment_details.csv")
checkpoint_attachments_file <- paste0(DOCKET_ID, "_checkpoint_attachment_details.csv")
checkpoint_failures_file <- paste0(DOCKET_ID, "_checkpoint_failures.csv")

if (file.exists(checkpoint_comments_file)) {
  existing_details <- read_csv(
    checkpoint_comments_file,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  ) |>
    mutate(
      request_ok = as.logical(request_ok),
      http_status = as.integer(http_status),
      withdrawn = as.logical(withdrawn)
    )
  
  completed_ids <- existing_details$comment_id |> unique()
  message("Found checkpoint with ", length(completed_ids), " completed comment IDs.")
} else {
  existing_details <- tibble()
  completed_ids <- character()
}

comment_ids <- setdiff(unique(comment_list$id), completed_ids)

message("Remaining comments to detail: ", length(comment_ids))

all_comment_rows <- list()
all_attachment_rows <- list()
all_failure_rows <- list()

for (i in seq_along(comment_ids)) {
  id <- comment_ids[i]
  
  message("Detail request ", i, " / ", length(comment_ids), ": ", id)
  
  parsed <- get_comment_detail(id)
  
  all_comment_rows[[i]] <- parsed$comment
  all_attachment_rows[[i]] <- parsed$attachments
  all_failure_rows[[i]] <- parsed$failure
  
  if (i %% 25 == 0) {
    new_details <- bind_rows(all_comment_rows)
    new_attachments <- bind_rows(all_attachment_rows)
    new_failures <- bind_rows(all_failure_rows)
    
    combined_details <- bind_rows(existing_details, new_details) |>
      distinct(comment_id, .keep_all = TRUE)
    
    write_csv(combined_details, checkpoint_comments_file)
    write_csv(new_attachments, checkpoint_attachments_file)
    write_csv(new_failures, checkpoint_failures_file)
    
    message("Checkpoint saved at ", i, " detailed requests.")
  }
}

# ----------------------------
# Step 6: final outputs
# ----------------------------

new_details <- bind_rows(all_comment_rows)
new_attachments <- bind_rows(all_attachment_rows)
new_failures <- bind_rows(all_failure_rows)

comment_details_full <- bind_rows(
  existing_details |> mutate(across(everything(), as.character)),
  new_details |> mutate(across(everything(), as.character))
) |>
  distinct(comment_id, .keep_all = TRUE)

# Read existing checkpoint attachments if they exist
if (file.exists(checkpoint_attachments_file)) {
  existing_attachments <- read_csv(
    checkpoint_attachments_file,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  )
} else {
  existing_attachments <- tibble()
}

# Combine checkpoint attachments + new attachments
attachment_details_full <- bind_rows(
  existing_attachments |> mutate(across(everything(), as.character)),
  new_attachments |> mutate(across(everything(), as.character))
) |>
  distinct()

# Clean attachment links 
attachments_clean <- attachment_details_full |>
  mutate(
    attachment_link = str_extract(
      file_format,
      "https?://[^[:space:]|]+"
    )
  ) |>
  filter(!is.na(attachment_link), attachment_link != "") |>
  distinct(comment_id, attachment_id, attachment_link, .keep_all = TRUE)

# Summarize attachments by comment_id
attachment_summary <- attachments_clean |>
  group_by(comment_id) |>
  summarise(
    has_attachments = TRUE,
    n_attachments = n(),
    attachment_links = paste(unique(attachment_link), collapse = " | "),
    .groups = "drop"
  )

# Join attachment summary back to comments
comments_final <- comment_details_full |>
  left_join(attachment_summary, by = "comment_id") |>
  mutate(
    has_comment_text = !is.na(comment_text) & str_trim(comment_text) != "",
    has_attachments = ifelse(is.na(has_attachments), FALSE, has_attachments),
    n_attachments = ifelse(is.na(n_attachments), 0, n_attachments)
  )

write_csv(comments_final, paste0(DOCKET_ID, "_comments_full_text_and_links.csv"))
write_csv(attachment_details_full, paste0(DOCKET_ID, "_attachments_full.csv"))
write_csv(attachments_clean, paste0(DOCKET_ID, "_attachments_clean.csv"))
write_csv(failures_full, paste0(DOCKET_ID, "_detail_failures.csv"))

message("Done.")
message("Comment list count: ", nrow(comment_list))
message("Detailed comment count: ", nrow(comment_details_full))
message("Attachment row count: ", nrow(attachment_details_full))
message("Failures: ", nrow(failures_full))
