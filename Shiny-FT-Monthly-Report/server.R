#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Set working directory to parent directory
#setwd("..")

# Source
source("./Scripts/Constants.R")
source("./Scripts/general_reporting.R")
source("./Scripts/cpu_report.R")
source("./Scripts/memory_report.R")
source("./Scripts/disk_report.R")
source("./Scripts/build-report.R")
source("./Scripts/manage_clients.R")
source("./Scripts/screenshots.R")

# Server variables
client_list <- client_list_from_json(CLIENT_LIST_PATH_JSON)
server_choices <- list()
servers <- character()
AV_img_path <- list()
BRT_img_path <- list()
SU_img_path <- list()
RAID_img_path <- list()
status_list <- list("AV" = list(), "BRT" = list(), "SU" = list(), "RAID" = list())
global_environment <- environment()

# Dynamic Server Screenshot System
# Make UI component for a server
render_reporting_ui <- function(i, type, servers) {
  # Toggle this to enable/disable screenshot buttons
  ENABLE_SCREENSHOT_BUTTONS <- FALSE
  
  card(
    card_header(servers[i]),
    card_body(
      # Main layout: content on left, preview on right
      layout_column_wrap(
        width = NULL,
        style = css(grid_template_columns = "1fr auto"),
        # Left side: radio buttons and file controls
        div(
          # Radio buttons for status
          radioButtons(
            inputId = paste0("status_", type, "_", i),
            label = "Status:",
            choices = c("Good", "Warning", "Critical"),
            selected = "Good",
            inline = TRUE
          ),
          # Button and file input
          layout_column_wrap(
            width = NULL,
            style = css(grid_template_columns = if(ENABLE_SCREENSHOT_BUTTONS) "auto 1fr" else "1fr"),
            if(ENABLE_SCREENSHOT_BUTTONS) {
              actionButton(
                inputId = paste0("btn_", type, "_", i),
                label = "Screenshot",
                class = "btn-primary"
              )
            },
            fileInput(
              inputId = paste0("file_", type, "_", i),
              label = NULL,
              buttonLabel = "Browse",
              placeholder = "No file selected"
            )
          )
        ),
        # Right side: preview image
        div(
          style = css(
            width = "200px",
            border = "1px solid #ddd",
            padding = "5px",
            display = "flex",
            align_items = "center",
            justify_content = "center",
            background_color = "#f8f9fa",
            overflow = "hidden"
          ),
          imageOutput(
            outputId = paste0("img_", type, "_", i),
            height = "100%"
          )
        )
      )
    )
  )
}

# Press screenshot button
create_screenshot_observer <- function(input, output, i, type, servers, img_path_list, env) {
  btn_id <- paste0("btn_", type, "_", i)
  file_id <- paste0("file_", type, "_", i)
  status_id <- paste0("status_", type, "_", i)
  image_id <- paste0("img_", type, "_", i)
  image_path_var_name <- paste0(type, "_img_path")
  
  obs1 <- observeEvent(input[[btn_id]], {
    img <- get_last_screenshot(SCREENSHOT_DIR)
    
    # Get the current list from the environment
    img_path_list <- get(image_path_var_name, envir = env)
    
    # Get the last screenshot
    img_path_list[[servers[i]]] <- img
    
    # Assign back to the environment
    assign(image_path_var_name, img_path_list, envir = env)
    
    # Show image preview
    output[[image_id]] <- output[[image_id]] <- renderImage(
      list(
        src = img,
        width = "auto",
        height = "140px",
        style = "max-width: 100%; max-height: 100%; object-fit: contain;"
      ), 
      deleteFile = FALSE
    )
    
    showNotification(
      paste("Server:", servers[i], " | Screenshot Updated: ", format.Date(Sys.time(), "%H:%M:%S")),
      type = "message"
    )
    
  }, ignoreInit = TRUE)
  
  obs2 <- observeEvent(input[[file_id]], {
    file_info <- input[[file_id]]
    
    # Get the current list from the environment
    img_path_list <- get(image_path_var_name, envir = env)
    
    # Get the last screenshot
    img_path_list[[servers[i]]] <- file_info$datapath
    
    # Assign back to the environment
    assign(image_path_var_name, img_path_list, envir = env)
    
    # Show image preview
    output[[image_id]] <- output[[image_id]] <- renderImage(
      list(
        src = file_info$datapath,
        width = "auto",
        height = "140px",
        style = "max-width: 100%; max-height: 100%; object-fit: contain;"
      ), 
      deleteFile = FALSE
    )
    
    showNotification(
      paste("Server:", servers[i], " | Screenshot Updated: ", format.Date(Sys.time(), "%H:%M:%S")),
      type = "message"
    )
    
  }, ignoreInit = TRUE)
  
  obs3 <- observeEvent(input[[status_id]], {
    # Get current list
    status_ls <- get("status_list", envir = env)
    
    # Update with new info
    status_ls[[type]][[servers[i]]] <- input[[status_id]]
    
    # Assign back to environment
    assign("status_list", status_ls, envir = env)
    
  }, ignoreInit = TRUE)
  
  # Return observers
  return(list(obs1, obs2, obs3))
}


# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Populate client select
  updateSelectInput(session, "sel_client", 
                    choices = names(client_list),
                    selected = "")
  
  # Populate Server Checkbox Group based on client
  observeEvent(input$sel_client, {
    if (input$sel_client == "") {
      server_choices <<- list()
    } else {
      server_choices <<- client_list[[input$sel_client]]$Servers
    }
    # Inner checkbox options (fixed)
    inner_options <- c("Anti-Virus", "Backup Recovery Testing", 
                       "Server Updates", "RAID Drives")
    
    # Generate nested checkboxes when client is selected
    output$nested_server_checkboxes <- renderUI({
      req(input$sel_client)
      
      if (input$sel_client == "" || length(server_choices) == 0) {
        return(NULL)
      }
      
      # Create nested checkbox structure
      div(
        h4("Select Servers and Reports to Include"),
        lapply(seq_along(server_choices), function(i) {
          server_name <- server_choices[i]
          
          tagList(
            # Outer checkbox for server
            checkboxInput(
              inputId = paste0("outer_server_", i),
              label = strong(server_name),
              value = TRUE  # Default to checked
            ),
            
            # Inner checkbox group (indented) - always visible
            conditionalPanel(
              condition = paste0("input.outer_server_", i),
              div(
                style = "margin-left: 30px; margin-bottom: 15px;",
                checkboxGroupInput(
                  inputId = paste0("inner_server_", i),
                  label = NULL,
                  choices = inner_options,
                  inline = FALSE
                )
              )
            )
          )
        })
      )
    })
  })
  
  # Create a reactive to get selected servers and their report types
  selected_servers_reports <- reactive({
    req(input$sel_client)
    
    if (length(server_choices) == 0) {
      return(list(servers = character(), reports = list()))
    }
    
    selected_servers <- character()
    server_reports <- list()
    
    for (i in seq_along(server_choices)) {
      outer_checked <- input[[paste0("outer_server_", i)]]
      inner_selected <- input[[paste0("inner_server_", i)]]
      
      if (!is.null(outer_checked) && outer_checked) {
        server_name <- server_choices[i]
        selected_servers <- c(selected_servers, server_name)
        server_reports[[server_name]] <- inner_selected
      }
    }
    
    list(servers = selected_servers, reports = server_reports)
  })
  
  # Screenshot Reports for each server
  observeEvent(selected_servers_reports(), {
    
    # Update server selection
    server_data <- selected_servers_reports()
    servers <<- server_data$servers
    
    # Reports part for each server
    report_types <- c("Anti-Virus", "Backup Recovery Testing", "Server Updates", "RAID Drives")
    report_parts <- list()
    for (type in report_types) {
      report_parts[[type]] <- names(server_data$reports)[sapply(server_data$reports, function(x) type %in% x)]
    }
    
    # Anti virus
    output$anti_virus_ss <- renderUI({
      # Add UI components for each server
      cards_AV <- lapply(seq_along(report_parts[["Anti-Virus"]]), 
                         render_reporting_ui, 
                         type = "AV", 
                         servers = report_parts[["Anti-Virus"]])

      # Return all UI components
      tagList(cards_AV)
    })
    
    # Backup Recovery Testing
    output$backup_ss <- renderUI({
      # Add UI components for each server
      cards_BRT <- lapply(seq_along(report_parts[["Backup Recovery Testing"]]),
                          render_reporting_ui, 
                          type = "BRT",
                          servers = report_parts[["Backup Recovery Testing"]])
      
      # Return all UI components
      tagList(cards_BRT)
    })
    
    # Server Updates
    output$updates_ss <- renderUI({
      # Add UI components for each server
      cards_SU <- lapply(seq_along(report_parts[["Server Updates"]]), 
                         render_reporting_ui, 
                         type = "SU",
                         servers = report_parts[["Server Updates"]])
      
      # Return all UI components
      tagList(cards_SU)
    })
    
    # RAID Drives
    output$RAID_ss <- renderUI({
      # Add UI components for each server
      cards_RAID <- lapply(seq_along(report_parts[["RAID Drives"]]), 
                           render_reporting_ui, 
                           type = "RAID",
                           report_parts[["RAID Drives"]])
      
      # Return all UI components
      tagList(cards_RAID)
    })
    
    output$server_notes <- renderUI({
      cards <- lapply(seq_along(servers), function(i) {
        card(
          card_body(
            textAreaInput(
              inputId = paste0("txt", servers[i]),
              label = servers[i],
              width = "100%"
            )
          )
        )
      })
      tagList(cards)
    })
    
    # Force outputs to render even when hidden - ADD THIS HERE
    outputOptions(output, "anti_virus_ss", suspendWhenHidden = FALSE)
    outputOptions(output, "backup_ss", suspendWhenHidden = FALSE)
    outputOptions(output, "updates_ss", suspendWhenHidden = FALSE)
    outputOptions(output, "RAID_ss", suspendWhenHidden = FALSE)
    outputOptions(output, "server_notes", suspendWhenHidden = FALSE)
    
  })
  
 
  
  # Create a reactiveVal to store observer handles
  observers <- reactiveVal(list())
  
  observeEvent(selected_servers_reports(), {
    # Destroy old observers first
    old_observers <- observers()
    if (length(old_observers) > 0) {
      lapply(old_observers, function(obs) {
        if (is.list(obs)) {
          lapply(obs, function(o) o$destroy())
        } else {
          obs$destroy()
        }
      })
    }
    
    # Create new observers for each server's button
    new_observers <- list()
    
    # Get server data
    server_data <- selected_servers_reports()
    
    # Get servers for each report type
    report_types <- c("Anti-Virus", "Backup Recovery Testing", "Server Updates", "RAID Drives")
    report_parts <- list()
    for (type in report_types) {
      report_parts[[type]] <- names(server_data$reports)[sapply(server_data$reports, function(x) type %in% x)]
    }
    
    # Create observers for each report type with their specific servers
    # Anti-virus
    av_servers <- report_parts[["Anti-Virus"]]
    for (i in seq_along(av_servers)) {
      obs <- create_screenshot_observer(input, output, i, "AV", av_servers, AV_img_path, global_environment)
      new_observers <- c(new_observers, list(obs))
    }
    
    # Backup Recovery Testing
    brt_servers <- report_parts[["Backup Recovery Testing"]]
    for (i in seq_along(brt_servers)) {
      obs <- create_screenshot_observer(input, output, i, "BRT", brt_servers, BRT_img_path, global_environment)
      new_observers <- c(new_observers, list(obs))
    }
    
    # Server Updates
    su_servers <- report_parts[["Server Updates"]]
    for (i in seq_along(su_servers)) {
      obs <- create_screenshot_observer(input, output, i, "SU", su_servers, SU_img_path, global_environment)
      new_observers <- c(new_observers, list(obs))
    }
    
    # RAID Drives
    raid_servers <- report_parts[["RAID Drives"]]
    for (i in seq_along(raid_servers)) {
      obs <- create_screenshot_observer(input, output, i, "RAID", raid_servers, RAID_img_path, global_environment)
      new_observers <- c(new_observers, list(obs))
    }
    
    # Store the new observers
    observers(new_observers)
    
  })
  
  # Generate report button press event
  observeEvent(input$btn_generate_report, {
    # When generate report button in pushed
    
    client <- input$sel_client
    reporter <- input$txt_reporter
    date <- as.POSIXct(as.character(input$date_on_report), tz = TIME_ZONE)
    
    # Get dates for report
    report_period <- str_split_1(input$sel_month, " ")
    start <- as.POSIXct(paste(
      MONTH_START,
      which(month.name == report_period[1]),
      report_period[2],
      sep = "-"
    ),
    format = "%d-%m-%Y",
    tz = TIME_ZONE)
    end <- min(as.POSIXct(paste(
        MONTH_START,
        which(month.name == report_period[1]) + 1,
        report_period[2],
        sep = "-"
      ),
      format = "%d-%m-%Y",
      tz = TIME_ZONE),
      Sys.time())
    
    # Get server data
    server_data <- selected_servers_reports()
    
    # Get servers for each report type
    report_types <- c("Anti-Virus", "Backup Recovery Testing", "Server Updates", "RAID Drives")
    report_parts <- list()
    for (type in report_types) {
      report_parts[[type]] <- names(server_data$reports)[sapply(server_data$reports, function(x) type %in% x)]
    }
    
    # Make server status table
    server_notes <- {
      notes <- character(0)
      for (server in servers) notes <- c(notes, input[[paste0("txt", server)]])
      notes
    }
    
    status_df <- create_server_status_table(servers = servers, 
                                            status_list = status_list, 
                                            notes = server_notes)
    
    # Input Validation
    if (start >= end) {showNotification("Invalid Report Period", type = "error"); return()}
    if (reporter == "") {showNotification("Must include reporter name", type = "error"); return()}
    if (is.null(servers)) {showNotification("Must select at least one server", type = "error"); return()}
    
    report <- generate_report(servers = servers,
                    client = client,
                    reporter = reporter,
                    start = start,
                    end = end,
                    date = date) |> 
      # Add anti-virus section
      report_add_section(label = ANTI_VIRUS_LABEL,
                        servers = report_parts[["Anti-Virus"]],
                        images = AV_img_path,
                        text = input$txtAV) |> 
      # Add backup recovery testing section 
      report_add_section(label = BACKUP_LABEL,
                         servers = report_parts[["Backup Recovery Testing"]],
                         images = BRT_img_path,
                         text = input$txtBRT) |> 
      # Add server updates section
      report_add_section(label = SERVER_UPDATE_LABEL,
                         servers = report_parts[["Server Updates"]],
                         images = SU_img_path,
                         text = input$txtSU) |> 
      # Add RAID section
      report_add_section(label = RAID_LABEL,
                         servers = report_parts[["RAID Drives"]],
                         images = RAID_img_path,
                         text = input$txtRAID) |> 
      # Add Additional Information
      report_add_text(label = ADDINFO_LABEL,
                     text = input$txt_add_info) |> 
      # Add Server Roles
      report_add_table(label = SERVERROLES_LABEL,
                       ft = status_df)
    
    showNotification("Report successfully created. Press 'Download Report' to download",
                     type = "message")
    
    output$dlReport <- downloadHandler(
      filename = function() paste0(REPORT_PATH, client,"_",format.Date(start, "%b%Y"), ".docx"),
      content = function(file) print(report, file)
    )
    
    
    #tryCatch({
    #  print(report, paste0(REPORT_PATH, client,"_",format.Date(start, "%b%Y"), ".docx"))
    #},
    #error = function(e) {
    #  showNotification(
    #    "Could not write to report to file. Adding random suffix.",
    #    type = "error"
    #  )
    #  tryCatch({
    #    print(report, paste0(REPORT_PATH, client,"_",format.Date(start, "%b%Y"), "_", round(runif(1) * 1e5), ".docx"))
    #  },
    #  error = function(e) {
    #   showNotification(
    #     "Could not write to report to file. Please check permissions.",
    #     type = "error"
    #    )
    #  })  
    #})  
  
  })
  
  # Update client list button event
  observeEvent(input$btn_update_client_list, {
    client_list <<- get_client_info_from_text(input$file_client_list_txt[1, "datapath"])
    client_list_to_json(client_list, path = CLIENT_LIST_PATH_JSON)
    
    # Update client selector
    updateSelectInput(session, "sel_client", 
                      choices = names(client_list),
                      selected = "")
  })
}


