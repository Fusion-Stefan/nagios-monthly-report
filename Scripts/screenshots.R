
# Function
get_last_screenshot <- function(folder) {
  files <- list.files(folder)
  file_info <- file.info(paste0(folder, files))
  return(rownames(file_info[which.max(file_info$mtime),]))
}
