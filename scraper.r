# -------------------------------------------
# Load Packages
# -------------------------------------------

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

results_all <- tibble()
i <- 0

# -------------------------------------------
# Scraping Loop (auto-paging, 2000-result cap)
# -------------------------------------------

repeat {
  if (i >= 200) break  # Stop after 2000 results max (200 pages x 10 per page)
  
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
  
  if (is.null(page)) break
  
  titles <- page %>%
    html_nodes(".gs_rt") %>%
    html_text(trim = TRUE)
  
  if (length(titles) == 0) break
  
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
  
  # Stop if no "Next" button
  next_button <- page %>% html_nodes(".gs_ico_nav_next")
  if (length(next_button) == 0) break
  
  i <- i + 1
  Sys.sleep(runif(1, 15, 30))  # polite delay
  gc()
  closeAllConnections()
}

#Extract Information from Citation Variable
results_all <- results_all %>%
  mutate(
    Citation_Clean = str_replace(Citation, " - Google Scholar", ""),
    Year = str_extract(Citation_Clean, "\\d{4}$"),  # <--- Get last 4-digit year
    Citation_Clean = str_replace(Citation_Clean, "\\(\\d{4}\\)", ""),
    Court_Name = str_extract(Citation_Clean, "(?<=, ).*$"),
    Court_Name = str_replace(Court_Name, "\\s\\d{4}$", ""),
    Court_Name = str_replace(Court_Name, ",$", ""),
    Court_Name = ifelse(str_detect(Court_Name, "Supreme Court"), "Supreme Court", Court_Name),
    Court_Citation = str_extract(Citation_Clean, "^[^,]+"),
    Court_Citation = gsub("- Court of Appeals", "", Court_Citation),
    Court_Citation = gsub("- Dist. Court", "", Court_Citation)
  ) %>%
  select(-Citation) %>%
  rename(Citation = Citation_Clean)
 
#Check Output
print(head(results_all, 10))
cat("Script ran successfully!")
