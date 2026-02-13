#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
# Libraries
library(shiny)
library(bslib)
library(tidyverse)

# Set working directory to parent directory
setwd("..")

# Source
source("./Scripts/Constants.R")

# Calculations
allowed_months <- seq(Sys.Date() - 365 * ALLOWED_YEARS_PAST, Sys.Date(), by = "month") |> 
  format.Date("%B %Y") |> 
  rev()

# Define UI for application that draws a histogram
layout_columns(

    # Navbar
    navset_pill_list(
      # Generate Report page
      nav_panel("General Info",
                page_fillable(
                  card(
                    titlePanel("Generate Client Monthly Report"),
                    fluidRow(
                      column(5,
                             selectInput("sel_client",
                                         "Select Client to Generate Report",
                                         list()
                             ),
                             selectInput("sel_month",
                                         "Select month for report",
                                         choices = as.list(allowed_months),
                                         selected = first(allowed_months)
                             ),
                             textInput("txt_reporter",
                                       "Reporter Name"  
                             ),
                             dateInput("date_on_report",
                                       "Date Report"
                             )
                      ),
                      column(7,
                             div(
                               style = "max-height: 600px; overflow-y: auto;",
                               uiOutput("nested_server_checkboxes")
                             )
                      )
                    )
                  )
                )
      ),
      # Anti Virus
      nav_panel("Anti Virus",
        card(
          page_fillable(
            titlePanel("Anti-Virus"),
            verbatimTextOutput("lblAVFile"),
            uiOutput("anti_virus_ss"),
            textAreaInput("txtAV", "Additional Information", width = "100%", height = "200px")
          )
        )
      ),
      # Backup Recovery Testing
      nav_panel("Backup Recovery Testing",
        card(
          page_fillable(
            titlePanel("Backup Recovery Testing"),
            verbatimTextOutput("lblBRTFile"),
            uiOutput("backup_ss"),
            textAreaInput("txtBRT", "Additional Information", width = "100%", height = "200px")
          )
        )
      ),
      # Server Updates
      nav_panel("Server Updates",
                card(
                  page_fillable(
                    titlePanel("Server Updates"),
                    verbatimTextOutput("lblSUFile"),
                    uiOutput("updates_ss"),
                    textAreaInput("txtSU", "Additional Information", width = "100%", height = "200px")
                  )
                )
      ),
      # RAID Drives
      nav_panel("RAID Drives",
                card(
                  page_fillable(
                    titlePanel("RAID Drives"),
                    verbatimTextOutput("lblRAIDFile"),
                    uiOutput("RAID_ss"),
                    textAreaInput("txtRAID", "Additional Information", width = "100%", height = "200px")
                  )
                )
      ),
      # Additional Information Page
      nav_panel("Additional Information",
        card(
          page_fillable(
            titlePanel("Additional Information"),
            uiOutput("server_notes"),
            textAreaInput("txt_add_info",
                      label = "",
                      value = "No additional information",
                      width = "100%",
                      height = "200px"),
            input_task_button("btn_generate_report",
                              "Generate Report"
            ),
            downloadButton("dlReport",
                           label = "Download Report"
            )
          )
        )
      ),
      # Manage Clients Page
      nav_panel("Manage Clients", # The Landing Page
                card(
                  page_fillable(
                    titlePanel("Manage Clients"),
                    fileInput("file_client_list_txt", "Choose client list text file"),
                    input_task_button("btn_update_client_list", "Update Client Information")
                  ),
                )
      ),
      id = "left_menu",
      well = FALSE,
      widths = c(2, 10)
    )
)
