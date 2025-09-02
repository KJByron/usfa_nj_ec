library(rvest)
library(dplyr)
library(stringr)

# url from recent
nj_tourn_recent <- read_html(
  "https://www.askfred.net/results?weapon=&gender=&age=&name=&date_by=on&date=&entries_count=&division_id=e86bb0a7-d4f6-40e6-ba7d-4462cf582f77&location=&radius=&authority=")

# nj_node <- nj_tourn_recent |> 
#   html_elements("h3") |> 
#   html_table(fill = TRUE)
nj_tourn_recent |> 
  html_elements("table") |> 
  html_table(fill = TRUE)
nj_tourn_recent |> 
  html_elements("table") |> 
  html_elements("a")


event_name <- nj_tourn_recent |>
  html_elements("table") |>
  html_elements("a")
  

result_link <- nj_tourn_recent |> 
  html_elements("table") |>
  html_elements("a") |> 
  html_attr("href") |>
  grepv(pattern = "results")
result_link[1]
# [1]                    "/tournaments/7a8938a9-b469-4bcb-a20d-a3f6bda5a10a/results"
# full link 1 is 
# "https://www.askfred.net/tournaments/7a8938a9-b469-4bcb-a20d-a3f6bda5a10a/results"

nj_table <- nj_tourn_recent |> 
  html_elements("table") |> 
  html_table(fill = TRUE)

page <- read_html("http://www.yelp.com/search?find_loc=New+York,+NY,+USA")
page %>% html_nodes(".biz-name") %>% html_attr('href')

## initial code from copilot
# Define a vector of URLs for NJ tournaments (manually curated or scraped from a search page)
tournament_urls <- c(
  "https://www.askfred.net/tournaments/6fbd4576-6d9d-4a7f-85f4-5fd7fc445369/results",
  "https://www.askfred.net/tournaments/e3c2c830-5284-4095-a068-7cb1700c5d23/results",
  "https://www.askfred.net/tournaments/cbea9976-ce40-4f62-8b7f-e6279269f745/results"
)

# Function to scrape results from a single tournament page
scrape_tournament_results <- function(url) {
  page <- read_html(url)
  
  # Extract event names
  event_names <- page %>%
    html_nodes("h3") %>%
    html_text(trim = TRUE)
  
  # Extract tables of results
  result_tables <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Combine event names with results
  results <- tibble()
  for (i in seq_along(result_tables)) {
    event <- event_names[i]
    table <- result_tables[[i]]
    table$event <- event
    results <- bind_rows(results, table)
  }
  
  results$url <- url
  return(results)
}

# Scrape all tournaments and combine results
nj_results <- lapply(tournament_urls, scrape_tournament_results) 
  # bind_rows()

# View the first few rows
print(head(nj_results))
# 