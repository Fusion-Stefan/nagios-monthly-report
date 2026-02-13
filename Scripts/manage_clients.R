# Manage Client Information for reporting purposes
# Library
library(tidyverse)
library(jsonlite)

# Sources
source("./Scripts/Constants.R")

# Functions
# Split client info into sections
get_client_info_1 <- function(txt) {
  client_txt <- txt
  client_txt <- client_txt[-length(client_txt)]
  
  # Get headings
  headings_ind <- grep(":", client_txt)
  headings <- str_remove(client_txt[headings_ind], ":\\s*")
  
  # Split info
  entry_count <- diff(c(headings_ind, length(client_txt) +1)) - 1
  client_info <- split(client_txt[-headings_ind], rep(1:length(headings), entry_count))
  names(client_info) <- headings
  
  return(client_info)
}

# Read client information from client list text file
get_client_info_from_text <- function(path) {

  # Read Text file
  text <- readLines(path)
  
  # Split into list of clients
  client_names <- text[which(text == "") + 1]
  text <- text[-(which(text == "") + 1)]
  stop <- c(which(text == ""), length(text))
  parts <- rep(1:(sum(text == "") + 1), diff(c(0,stop)))  
  client_list <- split(text, parts)[-1]
  names(client_list) <- client_names
  
  # Retrieve info for each client
  return(lapply(client_list, get_client_info_1)) 
  
} 

# Write Client List to JSON
client_list_to_json <- function(cl, path = CLIENT_LIST_PATH_JSON) write_json(cl, path)
# Read Client List from JSON
client_list_from_json <- function(path = CLIENT_LIST_PATH_JSON) read_json(path, simplifyVector = TRUE)

# Examples
#temp <- get_client_info_from_text(CLIENT_LIST_PATH_TXT)
#client_list_to_json(temp)  
#temp <- client_list_from_json(CLIENT_LIST_PATH_JSON)
      
  