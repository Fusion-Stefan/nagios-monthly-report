# Libraries
library(tidyverse) # A combination of packages that provide useful functions and workflow operations
library(officer) # Allows me to read from .docx

# Read the requirements document
# doc: str - file path to requirements word document
read_req_doc <- function(doc) {
  return(docx_summary(read_docx(doc)))
}

# Find all clients from the requirements document
# doc: docx summar - object as returned by read_req_doc
find_clients <- function(doc) {
  doc[["text"]][grep("heading 2", doc[["style_name"]])] |> 
    str_squish()
}

# Find all requirements for a client's report
# client: str - name of the client
# doc: docx summary - object as returned by read_req_doc
find_info <- function(client, doc) {
  # Get text from document
  text <- str_trim(doc[["text"]])
  
  # Find all info about client
  index_head <- which(grepl("heading [1|2]", doc[["style_name"]]))
  index_client <- which(text == client)[1]
  index_next <- index_head[(which(index_head %in% index_client) + 1)]
  info <- text[(index_client + 1):(index_next - 1)]
  
  # Separate out named info from requirements
  index_named <- grep(":", info)
  named_info <- str_split(info[index_named], ":") |> 
    lapply(str_trim)
  names(info)[index_named] <- sapply(named_info, first)
  info[index_named] <- sapply(named_info, function(x) x[-1])
  info <- c(info[index_named], str_flatten(info[-index_named], collapse = "\n "))
  names(info)[length(info)] <- "Jobs"
  
  # Return list with info
  info |> 
    str_split("\n ") |> 
    sapply(str_squish)
}
  
  
# Testing
#req_doc <- read_req_doc("report_checklist.docx")
#find_clients(req_doc)
#find_info("ACTO", req_doc)





