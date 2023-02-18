library(tidyverse)
library(httr2)
library(glue)
library(lubridate)

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

# function
get_database <- function(db_id, token = Sys.getenv("NOTION_TOKEN")) {
  
  req <- request(glue(base_url, "databases/", db_id)) %>%
    req_auth_bearer_token(token) %>%
    req_headers(`Notion-Version` = '2022-06-28')
  
  db_info <- req %>%
    req_perform() %>%
    resp_body_json()
  
  return(db_info)
}

get_database_all_pages <- function(db_id, token = Sys.getenv("NOTION_TOKEN")) {
  # check for string
  if(!is.character(db_id)){stop("db_id needs to be a string")}
  # check db_id is in the right format
  if(!str_detect(db_id, "[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}")){stop("db_id is in the wrong format. why not try using the extract_database_from_url function")}
  
  # build the request to query the database
  req <- request(glue(base_url, "databases/", db_id, "/query")) %>%
    req_method("POST") %>%
    req_auth_bearer_token(token) %>%
    req_headers(`Notion-Version` = '2022-06-28',
                `Content-Type` = 'application/json')
  
  # perform request and turn the body into json
  db_pages <- req %>%
    req_perform() %>%
    resp_body_json()
  
  # extract all the page ids 
  all_pages <- db_pages %>%
    # get the results element
    pluck("results") %>%
    # get all the page_ids
    map_chr("id")
  
  # check if we've got any more than the max pages, if so loop whilst this is true
  while(db_pages$has_more) {
    # modify the request for a new start_cursor
    req <- req %>%
      req_body_json(list(
        start_cursor = db_pages$next_cursor
      ))
    
    # perform the request and return as json
    db_pages <- req %>%
      req_perform() %>%
      resp_body_json()
    
    # get the new pages
    new_pages <- db_pages %>%
      # get the results element
      pluck("results") %>%
      # get all the page_ids
      map_chr("id")
    
    # add to all the pages
    all_pages <- c(all_pages, new_pages)
  }
  
  # return all the pages
  return(all_pages)
}

get_page <- function(page_id, token = Sys.getenv("NOTION_TOKEN")) {
  
  # create the page request
  req <- request(glue(base_url, "pages/", page_id)) %>%
    req_auth_bearer_token(token) %>%
    req_headers(`Notion-Version` = '2022-06-28')
  
  # perform it and return json
  page_info <- req %>%
    req_perform() %>%
    resp_body_json()
  
  return(page_info)
}

delete_page <- function(page_id, token = Sys.getenv("NOTION_TOKEN")) {
  # build the request to query the database
  req <- request(glue(base_url, "pages/", page_id)) %>%
    req_method("PATCH") %>%
    req_auth_bearer_token(token) %>%
    req_headers(`Notion-Version` = '2022-06-28',
                `Content-Type` = 'application/json') %>%
    req_body_raw('{"archived": true}', 'application/x-www-form-urlencoded')
  
  # delete the page
  page_info <- req %>%
    req_perform()
}

restore_page <- function(page_id, token = Sys.getenv("NOTION_TOKEN")) {
  # build the request to query the database
  req <- request(glue(base_url, "pages/", page_id)) %>%
    req_method("PATCH") %>%
    req_auth_bearer_token(token) %>%
    req_headers(`Notion-Version` = '2022-06-28',
                `Content-Type` = 'application/json') %>%
    req_body_raw('{"archived": false}', 'application/x-www-form-urlencoded')
  
  # delete the page
  page_info <- req %>%
    req_perform()
}

page_data_to_df <- function(page_list) {
  # extract properties
  list_properties <- page_list$properties
  
  # extract the names of the variables
  list_col_names <- names(list_properties)
  
  # extract the variable types
  list_types <- map_chr(list_properties, "type")
  
  # create a blank list
  row_result <- list()
  
  # lets do a loop, i'm sure i could use purrr
  for(x in 1:length(list_properties)) {
    # get the data out
    data <- pluck(list_properties, list_col_names[x])
    
    # apply the right transformation based on the vriable type, there is OBVIOUSLY
    # a better way of doing this but i am lazy. probs using switch
    if(list_types[x] == "rich_text"){result <- extract_rich_text(data)}
    if(list_types[x] == "title"){result <- extract_title(data)}
    if(list_types[x] == "date"){result <- extract_date(data)}
    
    # create a list of result and name it appropriately
    var_result <- list(result)
    names(var_result)[1] <- list_col_names[x]
    
    # add to the main list
    row_result <- c(row_result, var_result)
  }
  
  # cast it as a tibble
  row_tibble <- as_tibble(row_result)
  
  # return the result
  return(row_tibble)
  
}

extract_rich_text <- function(data_list) {
  if(length(data_list$rich_text) == 0) {
    text = ""
  } else {
    text <-  data_list$rich_text[[1]]$plain_text
  }
  return(text)
}

extract_date <- function(data_list) {
  if(is.null(data_list$date)) {
    date <- NA_POSIXct_
  } else {
    date <- ymd_hms(data_list$date$start)
  }
  return(date)
}

extract_title <- function(data_list) {
  if(length(data_list$title) == 0) {
    text = ""
  } else {
    text <-  data_list$title[[1]]$plain_text
  }
  return(text)
}

all_pages <- get_database_all_pages(db_id) 

get_page_df <- function(page_id) {
  page_list <- get_page(page_id)
  page_df <- page_data_to_df(page_list)
  return(page_df)
}

get_database_df <- function(db_id) {
  all_pages <- get_database_all_pages(db_id)
  
  db_df <- map_dfr(all_pages, get_page_df)
  
}


