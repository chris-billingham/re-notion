library(tidyverse)
library(httr2)

# get the token from .Renviron
token <- Sys.getenv("NOTION_TOKEN")

extract_database_from_url <- function(url) {
  # check for string
  if(!is.character(url)){stop("url needs to be a string")}
  
  # strip everything out we don't need
  get_long_id <- str_remove(url, "https://www.notion.so/") %>%
    str_extract("/([^//?]*)\\?") %>%
    str_sub(2, nchar(.) - 1)
  
  # split it up into the sections we need and build the database_id for the api
  database_id <- str_c(
    str_sub(get_long_id, 1, 8),
    str_sub(get_long_id, 9, 12),
    str_sub(get_long_id, 13, 16),
    str_sub(get_long_id, 17, 20),
    str_sub(get_long_id, 21, nchar(get_long_id)),
    sep = "-"
  )
   
  # return the database_id
  return(database_id)
}

db_id <- Sys.getenv("NOTION_DEMO_DB")

base_url <- "https://api.notion.com/v1/"

get_database <- function(db_id, token = Sys.getenv("NOTION_TOKEN")) {
  
  req <- request(glue(base_url, "databases/", db_id)) %>%
    req_auth_bearer_token(token) %>%
    req_headers(`Notion-Versions` = '2022-06-28')
  
  db_info <- req %>%
    req_perform() %>%
    resp_body_json()
  
  return(db_info)
}
