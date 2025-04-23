#Author: Finn Dobkin
#Date: 4/23/2025


#Install and load packages
required_packages <- c("rvest", "dplyr", "stringr", "progress", "readr", "httr", "curl")
installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}
lapply(required_packages, library, character.only = TRUE)

# -------------------------------------------
# Helper Function to Pad Vectors
# -------------------------------------------

lengthen <- function(x, n) {
  length(x) <- n
  return(x)
}

# -------------------------------------------
# Scrape Setup
# -------------------------------------------

base_url <- "https://scholar.google.com/scholar?"

query <- list(
  q = "National Environmental Policy Act",
  hl = "en",
  as_sdt = "3"  # Restrict to federal court cases only
)

num_pages <- 3  # Modify as needed

results_all <- tibble()

pb <- progress::progress_bar$new(
  total = num_pages,
  format = "Scraping [:bar] :current/:total (:percent) ETA: :eta",
  clear = FALSE, width = 60
)

# -------------------------------------------
# Scraping Loop
# -------------------------------------------

for (i in 0:(num_pages - 1)) {
  
  start_val <- i * 10
  
  query_url <- paste0(
    base_url,
    "start=", start_val, "&",
    "q=", URLencode(query$q), "&",
    "hl=", query$hl, "&",
    "as_sdt=", query$as_sdt
  )
  
  page <- tryCatch({
    resp <- httr::GET(
      query_url,
      httr::add_headers('User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36')
    )
    read_html(resp)
  }, error = function(e) {
    message("Error on search page ", i, ": ", e)
    return(NULL)
  })
  
  if (is.null(page)) next
  
  titles <- page %>%
    html_nodes(".gs_rt") %>%
    html_text(trim = TRUE)
  
  link_nodes <- page %>% html_nodes(".gs_rt")
  links <- link_nodes %>%
    html_node("a") %>%
    html_attr("href")
  
  snippets <- page %>%
    html_nodes(".gs_rs") %>%
    html_text(trim = TRUE)
  
  meta_info <- page %>%
    html_nodes(".gs_a") %>%
    html_text(trim = TRUE)
  
  # Ensure equal lengths
  max_len <- length(titles)
  links <- lengthen(links, max_len)
  snippets <- lengthen(snippets, max_len)
  meta_info <- lengthen(meta_info, max_len)
  
  results_page <- tibble(
    Title = titles,
    Link = links,
    Snippet = snippets,
    Citation = meta_info
  )
  
  results_all <- bind_rows(results_all, results_page)
  
  pb$tick()
  
  gc()
  closeAllConnections()
  
  Sys.sleep(runif(1, 15, 30))
}

# -------------------------------------------
# Clean Citation and Extract Court Name and Year
# -------------------------------------------

# Step 1: Remove "Google Scholar" from the Citation
results_all <- results_all %>%
  mutate(Citation_Clean = str_replace(Citation, " - Google Scholar", ""))

# Step 2: Extract the year from Citation_Clean (after "Google Scholar" is removed)
results_all <- results_all %>%
  mutate(Year = str_extract(Citation_Clean, "\\d{4}$"))

# Step 3: Remove the year (in parentheses) from Citation_Clean
results_all <- results_all %>%
  mutate(Citation_Clean = str_replace(Citation_Clean, "\\(\\d{4}\\)", ""))

# Step 4: Extract the court name (everything after the citation until the year)
results_all <- results_all %>%
  mutate(Court_Name = str_extract(Citation_Clean, "(?<=, ).*$"))

# Step 5: Remove the year from Court_Name
results_all <- results_all %>%
  mutate(Court_Name = str_replace(Court_Name, "\\s\\d{4}$", ""))

# Step 6: Remove unnecessary comma after Circuit (if present)
results_all <- results_all %>%
  mutate(Court_Name = str_replace(Court_Name, ",$", ""))

# Step 7: Remove unnecessary text for Supreme Court
results_all <- results_all %>%
  mutate(Court_Name = ifelse(str_detect(Court_Name, "Supreme Court"), "Supreme Court", Court_Name))

# -------------------------------------------
# Remove " - Court of Appeals" from the end of Court_Citation
# -------------------------------------------

results_all <- results_all %>%
  mutate(Court_Citation = str_extract(Citation_Clean, "^[^,]+"))

# Step 8: If Court_Citation ends with " - Court of Appeals", drop that text
results_all$Court_Citation <- gsub("- Court of Appeals", "", results_all$Court_Citation)

# -------------------------------------------
# Drop Citation (but keep Citation_Clean)
# -------------------------------------------

results_all <- results_all %>%
  select(-Citation, Snippet)

# -------------------------------------------
# Output and Inspection
# -------------------------------------------

print(head(results_all, 10))
