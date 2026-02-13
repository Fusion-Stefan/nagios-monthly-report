# Libraries
library(tidyverse) # A combination of packages that provide useful functions and workflow operations
library(officer) # Allows me to read and edit .docx documents
library(flextable) # Allows for table customization

# Source
source("./Scripts/Constants.R")

# Functions
# Create the basic document form a template and add some basic information
create_report <- function(
    client,  # str: name of the client to be displayed on title page
    reporter,  # str: name of the person making the report
    report_date, # date: first day of the period covered by the report
    date = Sys.Date(), # date: when report was done
    template = TEMPLATE_PATH # str: file path to a suitable template
) {
  
  # Load document from template
  doc <- read_docx(template)
  
  # replace some easy info
  body_replace_all_text(doc, CLIENT_LABEL, client) # Client name
  body_replace_all_text(doc, MONTHYEAR_LABEL, format.Date(report_date, "%B %Y")) # Month and year on title page
  body_replace_all_text(doc, REPORTER_LABEL, reporter) # Change name of reporter
  body_replace_all_text(doc, END_DATE_LABEL, format.Date(date, "%d-%m-%Y"))
  
  return(doc)
}

# Add text with multiple lines
add_ml_text <- function(report, text) {
  # Split text into lines
  text <- str_split_1(text, "\n")
  # Add each line
  for (line in text) {
    report <- body_add_par(report, line)
  }
  return(report)
}


# Add plot in report
report_add_plot <- function(report, plot, scale_width = 1, scale_height = 1) {
  report <- body_add_gg(report,
                        value = plot,
                        width = PLOT_WIDTH_GENERAL * scale_width,
                        height = PLOT_HEIGHT_GENERAL * scale_height,
                        scale = PLOT_GG_SCALE)
  
  return(report)
}

# Adds a section to the report with screenshots and text
report_add_section <- function(report, label, servers, images, text, scale_width = 1, scale_height = 1) {
  
  if (cursor_reach_test(report, label)) {
    report <- cursor_reach(report, label)
    
    for (i in seq_along(servers)){
      
      report <- body_add_fpar(report, 
                    fpar(
                      ftext(
                        servers[i],
                        fp_text(bold = TRUE)
                      )
                    )
      )
      
      
      if (length(images) > 0 && 
          !is.null(images[[servers[i]]]) && 
          images[[servers[i]]] != "" &&
          file.exists(images[[servers[i]]])) {
        report <- body_add_img(report,
                   src = images[[servers[i]]],
                   width = IMAGE_WIDTH * scale_width,
                   height = IMAGE_HEIGHT * scale_height)
      }
      
      report <- body_add_par(report, "")
    }
    
    report <- body_add_fpar(report,
      fpar(
        ftext(
          "Notes:",
          fp_text(underlined = TRUE)
         )
       )
     ) |> 
      add_ml_text(text) |> 
      body_add_par("")
    
  }
  
  # Remove labels 
  report <- cursor_reach(report, keyword = label) |> 
    body_remove()
  
  return(report)
}

# Add text to the report at a label
report_add_text <- function(report, label, text) {
  if (cursor_reach_test(report, label)) {
    report <- cursor_reach(report, label)
    
    report <- add_ml_text(report, text) |> 
      body_add_par("")
  }
  
  # Remove labels 
  report <- cursor_reach(report, label) |> 
   body_remove()
  
  return(report)
}

# Build server status table
create_server_status_table <- function(servers, status_list, notes) {
  # Turn list into df
  list_to_df <- function(lst, server_filter) {
    # Get all outer names (column names)
    outer_names <- names(lst)
    
    # Initialize matrix with "NA"
    mat <- matrix("NA", 
                  nrow = length(server_filter), 
                  ncol = length(outer_names),
                  dimnames = list(server_filter, outer_names))
    
    # Fill in the values only for filtered servers
    for (outer_name in outer_names) {
      inner_list <- lst[[outer_name]]
      for (server_name in server_filter) {
        if (!is.null(inner_list[[server_name]])) {
          mat[server_name, outer_name] <- inner_list[[server_name]]
        }
      }
    }
    
    # Convert to data frame
    as.data.frame(mat)
  }
  
  status_df <- list_to_df(status_list, servers)
  
  # Combine all and return
  df <- cbind(
    Server = servers,
    status_df,
    Notes = notes
  )
  
  # Change to full column names
  long_names <- list('AV' = 'Anti-Virus',
                     'BRT' = 'Backup Recovery Testing',
                     'SU' = 'Server Updates')
  col_names <- colnames(df)
  col_names[col_names %in% names(long_names)] <- unlist(long_names[col_names])
  colnames(df) <- col_names
  
  
  # Create a flextable from the data frame
  ft <- flextable(df)
  
  # Function to determine color based on text
  get_colour <- function(text) {
    text_lower <- tolower(text)
    colours <- ifelse(grepl("critical", text_lower), "red",
                      ifelse(grepl("warning", text_lower), "#f7af28",
                             ifelse(grepl("good", text_lower), "green",
                                    ifelse(grepl("na", text_lower),"darkgrey",
                                           "black"))))
    return(colours)
  }
  
  # Apply conditional formatting
  # Server column remains black as it serves as row headings
  for (col in colnames(df)) {
    if (col %in% c("Server", "Note")) next()
    ft <- color(ft, j = col, color = get_colour(df[,col]))
    ft <- width(ft, j = col, width = 0.85)
    ft <- fontsize(ft, j = col, size = 8, part = "body")
    ft <- align(ft, j = col, align = "center", part = "all")
  }
  
  # Make Server column formatting
  ft <- bold(ft, j = "Server", bold = TRUE)
  ft <- color(ft, j = "Server", color = "black")
  ft <- width(ft, j = "Server", width = 1.2)
  ft <- fontsize(ft, j = "Server", size = 9, part = "body")
  
  
  # Make Notes column formatting
  ft <- color(ft, j = "Notes", color = "black")
  ft <- width(ft, j = "Notes", width = 1.8)
  ft <- fontsize(ft, j = "Notes", size = 8, part = "body")
  
  # Enhanced formatting for better appearance
  # Header formatting - black with red accent
  ft <- bold(ft, part = "header")
  ft <- bg(ft, part = "header", bg = "#1a1a1a")  # Dark black header
  ft <- color(ft, part = "header", color = "white")
  ft <- fontsize(ft, part = "header", size = 9)
  
  # Alignment
  ft <- valign(ft, valign = "center", part = "all")
  
  # Add borders - red outer border, subtle gray inner borders
  ft <- border_outer(ft, border = fp_border(color = "#e02424", width = 2))
  ft <- border_inner_h(ft, border = fp_border(color = "#cccccc", width = 1))
  ft <- border_inner_v(ft, border = fp_border(color = "#cccccc", width = 1))
  
  # Add alternating row colors for better readability - very subtle
  ft <- bg(ft, i = seq(1, nrow(df), 2), bg = "#f9f9f9", part = "body")
  
  # Add padding for better spacing
  ft <- padding(ft, padding = 6, part = "all")
  
  return(ft)
}

# Add Server Roles table
report_add_table <- function(report, label, ft) {
  
  # Add to report
  if (cursor_reach_test(report, label)) {
    report <- cursor_reach(report, label)
    
    report <- body_add_flextable(report, ft) |> 
      body_add_par("")
  }
  
  # Remove labels 
  report <- cursor_reach(report, label) |> 
    body_remove()
  
  return(report)
}








