library(shiny)
library(shinyWidgets)
library(ggplot2)
library(bslib)
library(leaflet)
library(shinyjs)
library(DT)
library(grid)

setwd("../")
source("backend/src/get_results.R")
source("backend/src/arr_dur_functions.R")
source("backend/src/pipeline.R")

#################################################
#                    Theme                      #
#################################################

# Define the colors
nusBlue <- "#002D7C"      
offWhite <- "#FDFDFD"     
nusOrange <- "#EF7C00"  

# theme for main page
theme <- bs_theme(
  bg = offWhite,
  fg = nusBlue,
  primary = nusBlue,
  secondary = offWhite,
  info = nusBlue,
  success = nusOrange,
  warning = nusOrange,
  danger = nusOrange,
  "progress-bar-bg" = nusOrange,
  "nav-tabs" = list(
    a = list(
      background = offWhite,   # Background color for active tabs
      color = nusBlue        # Text color for active tabs
    ),
    "a:hover" = list(
      background = offWhite,   # Background color for hovered active tabs
      color = nusBlue       # Text color for hovered active tabs
    ),
    li = list(
      background = nusBlue,  # Background color for non-active tabs
      color = offWhite         # Text color for non-active tabs
    ),
    "li:hover" = list(
      background = nusBlue,  # Background color for hovered non-active tabs
      color = offWhite         # Text color for hovered non-active tabs
    )
  )
)

# theme for other pages
combined_theme <- bs_theme(
  bg = offWhite,               # Set the theme background color to white
  fg = nusBlue,              # Set text color to blue
  primary = nusBlue,         # Set the top bar background color to blue
  secondary = offWhite,      # Set text color to white
  info = offWhite,           # Set text color to white
  success = nusOrange,       # Set the progress bar background color to orange
  warning = nusOrange,
  danger = nusOrange,
  "progress-bar-bg" = nusOrange  # Set the progress bar background color to orange
)

# list of carparks
carparks = c("Carpark 3 UCC/YST Conservatory of Music",
             "Carpark 3A LKC Natural History Museum",
             "Carpark 4 Raffles Hall/CFA",
             "Carpark 5 Sports & Recreation Centre",
             "Carpark 5B NUS Staff Club (Club Members)",
             "Carpark 6B University Hall (NUS Staff & Authorized Guests)")

carparks_short = c("Carpark 3", "Carpark 3A", "Carpark 4/4A", "Carpark 5", "Carpark 5B", "Carpark 6B")

#################################################
#             Group Page Variables              #
#################################################

carparks_capacity <- list(
  "Carpark 3 UCC/YST Conservatory of Music" = 243,
  "Carpark 3A LKC Natural History Museum" = 67,
  "Carpark 4 Raffles Hall/CFA" = 116,
  "Carpark 5 Sports & Recreation Centre" = 70,
  "Carpark 5B NUS Staff Club (Club Members)" = 32,
  "Carpark 6B University Hall (NUS Staff & Authorized Guests)" = 173
)

marker_data <- data.frame(
  lat = c(1.302339, 1.301703, 1.300044,1.3001,1.29920,1.29731),
  lng = c(103.7729, 103.7736, 103.774147,103.7753,103.77588,103.77791),
  carparks = carparks,
  resource = c("car_park_3","car_park_3a,","car_park_4","car_park_5","car_park_5B","car_park_6b")
)

# Define the default latitude, longitude, and zoom level
default_latitude <- 1.3001
default_longitude <- 103.7753
default_zoom <- 18

# Customizing markers' design
selected_carparks <- reactiveValues(selected = NULL)

#################################################
#                  Home Page                    #
#################################################

home_page <- nav_panel("Home",
                       div(style = paste0("position: absolute;
                                           left: 0;
                                           top: 0;
                                           z-index: 10000;
                                           width: 100%;
                                           height: 100%;
                                           background: ", nusBlue, ";
                                           overflow: hidden;"),
                           
                           div(style = "position: relative; top: 10%; width:100%; text-align:center;",
                               # title
                               tags$strong("OptPark Simulator", 
                                           style = paste0("font-size: 4em; color: ", offWhite, ";")),
                               
                               # 4 buttons
                               div(style = "display: flex; justify-content: center;",
                                   div(style = "display: flex; flex-direction: column; align-items: flex-start; max-width: 1000px;",
                                       
                                       # single carpark simulation button
                                       div(style = "margin-top: 3em; margin-bottom: 1em;",
                                           actionButton("open-simulation-page", "Single Carpark Simulation",
                                                        style = paste0("color: ", offWhite, ";
                                                                    background-color: ", nusOrange, ";
                                                                    border-color: ", nusOrange, "; 
                                                                    width: 260px;"),
                                                        class = "btn btn-lg"),
                                           tags$p("View how your parameters affect a single carpark.",
                                                  style = "font-size: 1.2em; display: inline-block; color: white; margin-top: 1em; margin-left: 1.5em;")
                                       ),
                                       
                                       # multi-carpark simulation button
                                       div(style = "margin-bottom: 1em;",
                                           actionButton("open-grouped-page", "Multi-Carpark Simulation",
                                                        style = paste0("color: ", offWhite, ";
                                                                    background-color: ", nusOrange, ";
                                                                    border-color: ", nusOrange, "; 
                                                                    width: 260px;"),
                                                        class = "btn btn-lg"),
                                           tags$p("View how your parameters affect multiple carparks.",
                                                  style = "font-size: 1.2em; display: inline-block; color: white; margin-top: 1em; margin-left: 1.5em;")
                                       ),
                                       
                                       # simulation history button
                                       div(style = "margin-bottom: 1em;",
                                           actionButton("open-history-page", "Simulation History",
                                                        style = paste0("color: ", offWhite, ";
                                                                    background-color: ", nusOrange, ";
                                                                    border-color: ", nusOrange, "; 
                                                                    width: 260px;"),
                                                        class = "btn btn-lg"),
                                           tags$p("Review past simulations and compare results.",
                                                  style = "font-size: 1.2em; display: inline-block; color: white; margin-top: 1em; margin-left: 1.5em;")
                                       ),
                                       
                                       # upload data button
                                       div(style = "margin-bottom: 1em;",
                                           actionButton("open-upload-page", "Upload Data",
                                                        style = paste0("color: ", offWhite, ";
                                                                    background-color: ", nusOrange, ";
                                                                    border-color: ", nusOrange, "; 
                                                                    width: 260px;"),
                                                        class = "btn btn-lg"),
                                           tags$p("Upload new data files into the database.",
                                                  style = "font-size: 1.2em; display: inline-block; color: white; margin-top: 1em; margin-left: 1.5em;")
                                       )
                                   )
                               )
                           )
                       )
)

#################################################
#                Individual Page                #
#################################################

# slider input to balance the proportion of red and white lots
customSlider <- function(sliderInputId, label, min, max, value, step, post) {
  slider <- div(
    style = "position: relative; padding: 10px 0;",
    sliderInput(sliderInputId, label, min = min, max = max, value = value, 
                step = step, ticks = FALSE, post = post,
                label = HTML("Red lots: <strong id='red-lots-label'>50%</strong> | 
                             White lots: <strong id='white-lots-label'>50%</strong>")),
    div(style = "position: absolute; bottom: 10px; left: 0; color: red; font-weight: bold;", "Red Lots"), # position for the "Red" label
    div(style = "position: absolute; bottom: 10px; right: 0; color: grey; font-weight: bold;", "White Lots"), # position for the "White" label
    
    # updates the labels showing percentage of red and white lots as the slider changes
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateLabels_ind', function(message) {
        $('#red-lots-label').text(message.red + '%');
        $('#white-lots-label').text(message.white + '%');
      });
    ")),
    
    tags$head(
      tags$style(HTML("
        /* Hide tick labels 0% and 100%*/
        .irs .irs-min, .irs .irs-max {display: none;}
        /* Set the color of the slider to white */
        .js-irs-0 .irs-line, .js-irs-0 .irs-line-left {background: white;}
        /* Set the color of the selected range to red */
        .js-irs-0 .irs-single, .js-irs-0 .irs-bar {background: red;}
        /* Set the color of the slider handle to red */
        .js-irs-0 .irs-handle {background: red; border-color: red;}
      "))
    )
  )
  return(slider)
}

indiv_page <- nav_panel(
  "Individual",
  tags$head(
    tags$style(HTML("
    .bootstrap-select .btn {
      background-color: white !important;
      border: 1px solid #ccc !important;
    }
    .shiny-notification {
      width: 50%;
      position: fixed !important;
      top: 50% !important;
      left: 50% !important;
      transform: translate(-50%, -50%);
      z-index: 1000; 
    }
    .custom-size { width: 180px !important; margin-top: 5px; }
    h2 { font-family: 'Gill Sans', sans-serif; }
    .red-title { color: red; font-size: 180%; text-align: center; }
    .white-title { color: #FDFDFD; font-size: 180%; text-align: center; }
    .red-value { color: red; font-size: 200%; text-align: center; }
    .white-value { font-size: 200%; text-align: center; }
  "))
  ),
  card(
    full_screen = FALSE,
    card_header("Run your simulation."),
    layout_sidebar(
      sidebar = sidebar(width = 450,
                        uiOutput('resetable_sidebar'),
                        customSlider("lot_percentage_ind", "Percentage of Red and White Lots:",
                                     min = 0, max = 100, value = 50, step = 1, post = "%"),
                        span(
                          div(style = "display: flex; justify-content: space-between;",
                              div(
                                actionLink("reset_button_indiv", icon("undo"), class = "btn btn-primary", style = "width: 95px;"),
                                actionButton("run_button_indiv", "Run", class = "btn btn-primary", style = "width: 290px;")
                              )
                              
                          )
                        )
      ),
      fillable = FALSE,
      #################################################
      #               Simulation Results              #
      #################################################=
      hidden(
        fluidRow(
          id = "results_indiv",
          column(7, h2("Usage"), card(
            full_screen = TRUE,
            card_body(
              div(
                fluidRow(
                  column(6, div(
                    h6("Red lots:"),
                    uiOutput("plot_red_usage")),
                  ),
                  column(6, div(
                    h6("White lots:"),
                    uiOutput("plot_white_usage")),
                  ))
              ),
              height = 575)
          )),
          column(5, h2("Mean Occupancy Rate"), fluidRow(
            column(6, value_box(
              style = 'background-color: #194289!important;',
              title = tags$p("RED", style = "color: red; font-size: 150%; text-align: center"),
              value = uiOutput("mean_occ_red"),
              height = "220px")
            ),
            column(6, value_box(
              style = 'background-color: #194289!important;',
              title = tags$p("WHITE", style = "color: #FDFDFD; font-size: 150%; text-align: center"),
              value = uiOutput("mean_occ_white"),
              height = "220px")
            )), h2("Max Occupancy Rate"), fluidRow(
              column(6, value_box(
                style = 'background-color: #194289!important;',
                title = tags$p("RED", style = "color: red; font-size: 150%; text-align: center"),
                value = uiOutput("max_occ_red"),
                height = "220px")
              ),
              column(6, value_box(
                style = 'background-color: #194289!important;',
                title = tags$p("WHITE", style = "color: #FDFDFD; font-size: 150%; text-align: center"),
                value = uiOutput("max_occ_white"),
                height = "220px")
              )),
            column(12, align = "right", h5("Download your results:")),
            column(12, align = "right", downloadButton("downloadData", "Download Results",
                                                       class = "btn btn-primary custom-size"))
          )
        )
      )
    )
  )
)

#################################################
#                 Group Page                    #
#################################################

# group custom slider input to balance the proportion of red and white lots
groupcustomSlider <- function(sliderInputId, carparkName, label, min, max, value, step, post) {
  redLabelId <- paste0('red-lots-label-', sliderInputId)
  whiteLabelId <- paste0('white-lots-label-', sliderInputId)
  messageType <- paste0('updateLabels_', sliderInputId) # unique message handler type
  
  customLabel <- div(style = "text-align: left; font-weight: bold; margin-bottom: 5px;", carparkName)
  
  slider <- div(
    customLabel,
    style = "position: relative; padding: 10px 0;",
    sliderInput(sliderInputId, label, min = min, max = max, value = value, 
                step = step, ticks = FALSE, post = post,
                label = HTML(paste0("Red lots: <strong id='", redLabelId, "'>50%</strong> | 
                                     White lots: <strong id='", whiteLabelId, "'>50%</strong>"))),
    div(style = "position: absolute; bottom: 10px; left: 0; color: red; font-weight: bold;", "Red Lots"), # position for the "Red" label
    div(style = "position: absolute; bottom: 10px; right: 0; color: grey; font-weight: bold;", "White Lots"), # position for the "White" label
    
    # updates the labels showing percentage of red and white lots as the slider changes
    tags$script(HTML(paste0("
      Shiny.addCustomMessageHandler('", messageType, "', function(message) {
        $('#", redLabelId, "').text(message.red + '%');
        $('#", whiteLabelId, "').text(message.white + '%');
      });
    "))),
    
    tags$head(
      tags$style(HTML("
        /* Hide tick labels 0% and 100%*/
        .irs .irs-min, .irs .irs-max {display: none;}
        /* Set the color of the slider to white */
        .js-irs-0 .irs-line, .js-irs-0 .irs-line-left {background: white;}
        /* Set the color of the selected range to red */
        .js-irs-0 .irs-single, .js-irs-0 .irs-bar {background: red;}
        /* Set the color of the slider handle to red */
        .js-irs-0 .irs-handle {background: red; border-color: red;}
      "))
    )
  )
  return(slider)
}

group_page <- nav_panel(
  "Grouped",
  tags$head(
    tags$style(
      HTML(".leaflet-popup-content-wrapper {
             background-color: #002D7C;
             color: white;
             text-align: center; /* Center the content */
         }
         .leaflet-popup-content-wrapper h1 {
             font-weight: bold;
             font-size: 20px;
         }
         .leaflet-popup-tip {
             border-top: 19px solid #002D7C; /* Set the color of the popup tip */
         }
         .red-box {
             background-color: red;
             height: 65px;
             width: 100px;
             display: inline-block;
             border-radius: 5px;
             vertical-align:top;
         }
         .red-box-text {
             font-size: 20px; 
             color: white; 
             font-weight: bold
         }
         .white-box {
             background-color: white;
             height: 65px;
             width: 100px;
             display: inline-block;
             border-radius: 5px;
             vertical-align:top;
         }
         .white-box-text {
             font-size: 20px;
             color: #002D7C;
             font-weight: bold
         }
            .bootstrap-select .btn {
        background-color: white !important;
        border: 1px solid #ccc !important;
      }
      .shiny-notification {
        width: 50%;
        position: fixed !important;
        top: 50% !important;
        left: 50% !important;
        transform: translate(-50%, -50%);
        z-index: 1000;
      }
      .custom-width_button { width: 210px; }
      .custom-width { width: 25vw; } /* Adjust the width as needed */
      .custom-height { height: 40vh; } /* Adjust the height as needed */
      .custom-box { border: 1px solid #ccc; padding: 10px; margin: 5px; display: inline-block; }
      .custom-blue-box {
        background-color: #002D7C;
        color: white; /* Optionally, set text color to white for better visibility */
      }
      .custom-width-half { width: 50vw; }"
      )
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Run your simulation."),
    layout_sidebar(
      sidebar = sidebar(width = 500,
                        uiOutput('group_sidebar'),
                        groupcustomSlider("lot_percentage_3", "Carpark 3","Percentage of Red and White Lots:",
                                          min = 0, max = 100, value = 50, step = 1, post = "%"),
                        groupcustomSlider("lot_percentage_3A", "Carpark 3A","Percentage of Red and White Lots:",
                                          min = 0, max = 100, value = 50, step = 1, post = "%"),
                        groupcustomSlider("lot_percentage_4", "Carpark 4", "Percentage of Red and White Lots:",
                                          min = 0, max = 100, value = 50, step = 1, post = "%"),
                        groupcustomSlider("lot_percentage_5", "Carpark 5","Percentage of Red and White Lots:",
                                          min = 0, max = 100, value = 50, step = 1, post = "%"),
                        groupcustomSlider("lot_percentage_5B", "Carpark 5B", "Percentage of Red and White Lots:",
                                          min = 0, max = 100, value = 50, step = 1, post = "%"),
                        groupcustomSlider("lot_percentage_6B", "Carpark 6B", "Percentage of Red and White Lots:",
                                          min = 0, max = 100, value = 50, step = 1, post = "%"),
                        span(
                          div(style = "display: flex; justify-content: space-between;",
                              div(
                                actionLink("reset_button_group", icon("undo"), class = "btn btn-primary", style = "width: 110px;"),
                                actionButton("run_button_group", "Run", class = "btn btn-primary", style = "width: 315px;")
                              )
                              
                          )
                        )
      ),
      leafletOutput("map"),
      fluidRow(
        column(12, align = "right", span(
          actionButton("overall_button", "Overall Results", class = "btn-primary"),
          downloadButton("download_data_group", "Download Results", 
                         style = " bottom: 2px; right: 50px;", class = "btn-primary"))
        ),
        tags$div(
          id = "modalContentGroup",
          style = "display: none;"
        ))
    ),
  ),
)

#################################################
#                History Page                   #
#################################################

history_page <- nav_panel(
  "History",
  useShinyjs(),
  tags$style(HTML('table.dataTable tr.active td, table.dataTable tr.active {box-shadow: inset 0 0 0 9999px #002D7C !important;}',
                  '.dataTables_wrapper .dataTables_scroll {border-radius: 10px;}',
                  '#selected_rows { margin-bottom: 5px; margin-left: 10px; font-size: 16px; width: 1000px; height: 50px; }',
                  '.custom-size-history { width: 100px; margin-top: 5px; }',
                  '#error_message_view { margin-left: 10px; font-size: 20px; color: red; } ',
                  '#error_message_compare { margin-left: 10px; font-size: 20px; color: red; } ',
                  '.custom-h5 h5 { margin-bottom: 5px; }',
                  '.custom-width { width: 200px; }')),
  div(class = "custom-h5",
      h5("See your past simulations (Note: This is currently simulated data):"),
      tags$ul(
        tags$li("Select one row and press 'View' to see one past simulation."),
        tags$li("Select two rows and press 'Compare' to compare two past simulations.",
        )
      )
  ),
  fluidRow(
    column(9, verbatimTextOutput('selected_rows')),
    column(3, span(
      actionButton("view_button", "View", class = "btn btn-primary custom-size-history"),
      actionButton("compare_button", "Compare", class = "btn btn-primary custom-size-history"),
      actionLink("reset_button_history", icon("undo"), class = "btn btn-primary custom-size-history"),
      style = "right:2em;"
    )),
    hidden(div(id = "error_message_compare", "Error! You can only compare 2 simulations at once. Please select again.")),
    hidden(div(id = "error_message_view", "Error! You can only view 1 simulation. Please select again.")),
    div(style = "margin-top: 10px;", DTOutput("table")),
    hidden(
      fluidRow(
        id = "history_view",
        h5("2023-11-07T07:00:00"),
        column(6, h4("Usage"), card(
          full_screen = TRUE,
          card_body(
            div(
              img(src='history-image-red.png', height="90%", width="90%"),
              img(src='history-image-white.png', height="90%", width="90%")),
            height = 400)
        )),
        column(6, h4("Mean Occupancy Rate"), fluidRow(
          column(6, value_box(style = 'background-color: #194289!important;', title ="Red", value = tags$p("0.243", style = "font-size: 200%;"), height = "160px")),
          column(6, value_box(style = 'background-color: #194289!important;', title ="White", value = tags$p("0.290", style = "font-size: 200%;"), height = "160px"))),
          h4("Max Occupancy Rate"),
          fluidRow(
            column(6, value_box(style = 'background-color: #194289!important;', title ="Red", value = tags$p("0.789", style = "font-size: 200%;"), height = "160px")),
            column(6, value_box(style = 'background-color: #194289!important;', title ="White", value = tags$p("0.705", style = "font-size: 200%;"), height = "160px"))
          ),
          column(12, align = "right", actionButton("download-data-history", "Download Data", class = "btn btn-primary custom-width"))
        )
      )
    ),
    hidden(
      fluidRow(
        id = "history_compare",
        column(6, h6("2023-11-07T07:00:00")),  
        column(6, h6("2023-11-05T02:30:00")),
        column(6, fluidRow(
          column(6, h4("Usage"),card(
            full_screen = TRUE,
            card_body(
              div(
                img(src='history-image-red.png', height="90%", width="90%"),
                img(src='history-image-white.png', height="90%", width="90%")),
              height = 400)
          )
          ),
          column(6, h4("Mean Occupancy Rate"), fluidRow(
            column(6, value_box(style = 'background-color: #194289!important;', title ="Red", value = tags$p("0.243", style = "font-size: 200%;"), height = "160px")),
            column(6, value_box(style = 'background-color: #194289!important;', title ="White", value = tags$p("0.290", style = "font-size: 200%;"), height = "160px"))),
            h4("Max Occupancy Rate"),
            fluidRow(
              column(6, value_box(style = 'background-color: #194289!important;', title ="Red", value = tags$p("0.789", style = "font-size: 200%;"), height = "160px")),
              column(6, value_box(style = 'background-color: #194289!important;', title ="White", value = tags$p("0.705", style = "font-size: 200%;"), height = "160px"))
            ),
            column(12, align = "right", actionButton("download-data-compare-1", "Download Data", class = "btn btn-primary custom-width"))
          ))), 
        column(6, fluidRow(
          column(6, h4("Usage"),card(
            full_screen = TRUE,
            card_body(
              div(
                img(src='history-image-red.png', height="90%", width="90%"),
                img(src='history-image-white.png', height="90%", width="90%")),
              height = 400)
          )
          ),
          column(6, h4("Mean Occupancy Rate"), fluidRow(
            column(6, value_box(style = 'background-color: #194289!important;', title ="Red", value = tags$p("0.353", style = "font-size: 200%;"), height = "160px")),
            column(6, value_box(style = 'background-color: #194289!important;', title ="White", value = tags$p("0.190", style = "font-size: 200%;"), height = "160px"))),
            h4("Max Occupancy Rate"),
            fluidRow(
              column(6, value_box(style = 'background-color: #194289!important;', title ="Red", value = tags$p("0.830", style = "font-size: 200%;"), height = "160px")),
              column(6, value_box(style = 'background-color: #194289!important;', title ="White", value = tags$p("0.715", style = "font-size: 200%;"), height = "160px"))
            ),
            column(12, align = "right", actionButton("download-data-compare-2", "Download Data", class = "btn btn-primary custom-width"))
          )))
      )
    )
  )
)

#################################################
#                 Upload Page                 #
#################################################

upload_page <- nav_panel(
  "Upload Data", 
  card(full_screen = TRUE,
       card_header("Upload your data."),
       card_body(
         fileInput(inputId = "file",
                   label = "Choose CSV File:",
                   multiple = TRUE,
                   accept = c(".csv"),
                   buttonLabel = "Browse...",
                   placeholder = "No file selected"),
         style = "overflow-y: auto;",
         tags$p("Your file should follow this template:"),
         tags$img(src = "tableformat.png", height = "100px"),
         tableOutput("dataTemplate"),
         tags$p("Once your data has been uploaded, you will see a preview here."),
         uiOutput("data_preview")
       ),
  ),
  tags$head(
    tags$style(HTML(paste0("
      .btn-file {
        background-color:", nusOrange, ";
        color: white;
      }
      .btn-file:hover {
        background-color: #D27D2D; /* darker orange color on hover */
      }
    ")))
  )
)

#################################################
#                 UI Function                   #
#################################################

ui <- page_navbar(
  id = "page",
  title = "OptPark",
  theme = combined_theme,  # Use the combined theme
  nav_panel(title = "Home", home_page),
  nav_panel(title = "Individual", indiv_page),
  nav_panel(title = "Grouped", group_page),
  nav_panel(title = "History", history_page),
  nav_panel(title = "Upload Data", upload_page)
)

#################################################
#                Server Function                #
#################################################

server <- function(input, output, session) {
  
  # functions to open respective pages when buttons are clicked
  observeEvent(input$`open-simulation-page`, {
    updateTabsetPanel(session, "page", "Individual")
  })
  observeEvent(input$`open-grouped-page`, {
    updateTabsetPanel(session, "page", "Grouped")
  })
  observeEvent(input$`open-history-page`, {
    updateTabsetPanel(session, "page", "History")
  })
  observeEvent(input$`open-upload-page`, {
    updateTabsetPanel(session, "page", "Upload Data")
  })
  
  months = month.abb
  
  # sidebar input panel for individual simulation page
  output$resetable_sidebar <- renderUI({
    hideElement("results_indiv")
    hideElement("plot_red_usage")
    hideElement("plot_white_usage")
    times <- input$reset_button_indiv
    div(id = letters[(times %% length(letters)) + 1],
        
        pickerInput(
          inputId = "carpark_input",
          label = "Select Carpark:",
          choices = carparks,
          options = list(`actions-box` = TRUE, `live-search` = TRUE)
        ),
        
        # Numeric input for number of simulations
        numericInput("num_simulations", "Number of simulations to be run (minimum value of 1):", value = 20, min =1),
        
        # Numeric input for specifying capacity of the carpark
        numericInput("capacity", "Capacity (exact number of parking lots):", value = 243),
        
        # Dropdown for selection start month to end month
        fluidRow(
          column(6, selectInput("month_st", "Start month: ",
                                choices = months)),
          column(6, selectInput("month_en", "End month: ",
                                choices = months))
        ),
        tags$br(),
        
        numericInput("increase_arrival_rate", "Enter % of arrival rate (100% for no change):", value = 100, min=1, max=200)
        
    )
  })
  
  # function to ensure end month is later than start month
  observe({
    if (!is.null(input$month_st)) {
      start_month_index <- match(input$month_st, month.abb)
      end_month_choices <- month.abb[start_month_index:length(month.abb)]
      updateSelectInput(session, "month_en", choices = end_month_choices)
    }
  })
  
  # for updating the red and white lots label as the indiv slider changes
  observe({
    red_percentage <- input$lot_percentage_ind
    white_percentage <- 100 - red_percentage
    session$sendCustomMessage(type = 'updateLabels_ind', message = list(red = red_percentage, white = white_percentage))
  })
  
  # reset logic for indiv slider
  observeEvent(input$reset_button_indiv, {
    updateSliderInput(session, "lot_percentage_ind", value = 50)
    session$sendCustomMessage(type = 'updateLabels_ind', message = list(red = 50, white = 50))
  })
  
  ################################################################
  ###       Sidebar Input Panel for Group Simulation Page      ###
  ################################################################
  
  output$group_sidebar <- renderUI({
    times <- input$reset_button_group
    
    default_selected_carparks <- carparks[1]
    default_selected_carparks_short <- carparks[1]
    div(
      tags$head(
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
        tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
        tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: red}")),
        tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: red}")),
        tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: red}")),
        tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: red}")),
        tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: red}")),
      ),
      id = letters[(times %% length(letters)) + 1],
      pickerInput("Options", "Select Carpark to Open:",
                  choices = carparks, 
                  multiple = TRUE, 
                  selected = carparks,
                  options = list(container = 'body')),
      
      
      tags$br(),
      
      # Numeric input for number of simulations
      numericInput("num_simulations_grp", "Number of simulations to be run:", value = 20, min = 1),
      
      tags$br(),
      HTML("<p>Capacity (exact number of parking lots):</p>"),
      fluidRow(
        lapply(1:6, function(carpark_name) {
          column(
            6,
            numericInput(
              paste0("capacity_", carpark_name),
              carparks_short[carpark_name],
              value = if (carpark_name %in% default_selected_carparks) carparks_capacity[[carpark_name]] else NA,
              min = 0,
              max = 1000 
            )
          )
        })
      ),
      tags$br(),
      
      # Dropdown for selection start month to end month
      fluidRow(
        column(6, selectInput("month_st_group", "Start month: ",
                              choices = months)),
        column(6, selectInput("month_en_group", "End month: ",
                              choices = months))
      ),
      
      tags$br(),
      HTML("<p>% of Sheltered Lots:</p>"),
      # numeric input for percentage of sheltered lots
      fluidRow(
        column(6, numericInput("sheltered_percentage_3", "Carpark 3", value = 50, min=0, max=100)),
        column(6, numericInput("sheltered_percentage_3A", "Carpark 3A", value = 50, min=0, max=100)),
        column(6, numericInput("sheltered_percentage_4A", "Carpark 4/4A", value = 50, min=0, max=100)),
        column(6, numericInput("sheltered_percentage_5", "Carpark 5", value = 50, min=0, max=100)),
        column(6, numericInput("sheltered_percentage_5B", "Carpark 5B", value = 50, min=0, max=100)),
        column(6, numericInput("sheltered_percentage_6B", "Carpark 6B", value = 50, min=0, max=100))
      ),
      tags$br(),
      HTML("<p>% of Carpark Open for Use:</p>"),
      # numeric input for percentage of lots open for use
      fluidRow(
        column(6, numericInput("usable_percentage_3", "Carpark 3", value = 50, min=0, max=100)),
        column(6, numericInput("usable_percentage_3A", "Carpark 3A", value = 50, min=0, max=100)),
        column(6, numericInput("usable_percentage_4A", "Carpark 4/4A", value = 50, min=0, max=100)),
        column(6, numericInput("usable_percentage_5", "Carpark 5", value = 50, min=0, max=100)),
        column(6, numericInput("usable_percentage_5B", "Carpark 5B", value = 50, min=0, max=100)),
        column(6, numericInput("usable_percentage_6B", "Carpark 6B", value = 50, min=0, max=100))
      ),
      tags$br(),
      #numeric input for arrival rate (group)
      numericInput("increase_arrival_rate_group", "Enter % of arrival rate (100% for no change):", value = 100, min=1, max=200)
    )
  })
  
  # for updating the red and white lots label as the group slider changes
  observe({
    red_percentage_3 <- input$lot_percentage_3
    white_percentage_3 <- 100 - red_percentage_3
    session$sendCustomMessage(type = 'updateLabels_lot_percentage_3', message = list(red = red_percentage_3, white = white_percentage_3))
  })
  
  observe({
    red_percentage_3A <- input$lot_percentage_3A
    white_percentage_3A <- 100 - red_percentage_3A
    session$sendCustomMessage(type = 'updateLabels_lot_percentage_3A', message = list(red = red_percentage_3A, white = white_percentage_3A))
  })
  
  observe({
    red_percentage_4 <- input$lot_percentage_4
    white_percentage_4 <- 100 - red_percentage_4
    session$sendCustomMessage(type = 'updateLabels_lot_percentage_4', message = list(red = red_percentage_4, white = white_percentage_4))
  })
  
  observe({
    red_percentage_5 <- input$lot_percentage_5
    white_percentage_5 <- 100 - red_percentage_5
    session$sendCustomMessage(type = 'updateLabels_lot_percentage_5', message = list(red = red_percentage_5, white = white_percentage_5))
  })
  
  observe({
    red_percentage_5B <- input$lot_percentage_5B
    white_percentage_5B <- 100 - red_percentage_5B
    session$sendCustomMessage(type = 'updateLabels_lot_percentage_5B', message = list(red = red_percentage_5B, white = white_percentage_5B))
  })
  
  observe({
    red_percentage_6B <- input$lot_percentage_6B
    white_percentage_6B <- 100 - red_percentage_6B
    session$sendCustomMessage(type = 'updateLabels_lot_percentage_6B', message = list(red = red_percentage_6B, white = white_percentage_6B))
  })
  
  # Reset logic for group sliders
  observeEvent(input$reset_button_group, {
    updateSliderInput(session, "lot_percentage_3", value = 50)
    session$sendCustomMessage(type = 'updateLabels_lot_percentage_3', message = list(red = 50, white = 50))
  })
  
  observeEvent(input$reset_button_group, {
    updateSliderInput(session, "lot_percentage_3A", value = 50)
    session$sendCustomMessage(type = 'updateLabels_lot_percentage_3A', message = list(red = 50, white = 50))
  })
  
  observeEvent(input$reset_button_group, {
    updateSliderInput(session, "lot_percentage_4", value = 50)
    session$sendCustomMessage(type = 'updateLabels_lot_percentage_4', message = list(red = 50, white = 50))
  })
  
  observeEvent(input$reset_button_group, {
    updateSliderInput(session, "lot_percentage_5", value = 50)
    session$sendCustomMessage(type = 'updateLabels_lot_percentage_5', message = list(red = 50, white = 50))
  })
  
  observeEvent(input$reset_button_group, {
    updateSliderInput(session, "lot_percentage_5B", value = 50)
    session$sendCustomMessage(type = 'updateLabels_lot_percentage_5B', message = list(red = 50, white = 50))
  })
  
  observeEvent(input$reset_button_group, {
    updateSliderInput(session, "lot_percentage_6B", value = 50)
    session$sendCustomMessage(type = 'updateLabels_lot_percentage_6B', message = list(red = 50, white = 50))
  })
  
  # Observer to update capacity based on selected carpark
  observe({
    selected_carpark <- input$carpark_input
    
    # Check if the selected carpark exists in the carparks_capacity list
    if (!is.null(selected_carpark) && selected_carpark %in% names(carparks_capacity)) {
      default_capacity <- carparks_capacity[[selected_carpark]]
      updateNumericInput(session, "capacity", value = default_capacity)
    } 
  })
  
  #to ensure end month input is not before start month
  observe({
    if (!is.null(input$month_st_group)) {
      start_month_index <- match(input$month_st_group, months)
      end_month_choices <- months[start_month_index:length(months)]
      updateSelectInput(session, "month_en_group", choices = end_month_choices)
    }
  })
  
  observeEvent(input$run_button_indiv, {
    
    #################################################
    #                 Progress Bar                  #
    #################################################
    progress <- shiny::Progress$new(session, min = 0, max = 10)
    progress$set(message = "Processing", value = 0)
    
    showModal(modalDialog(
      title = "Hang on, carpark geniuses at work...",
      footer = NULL
    ))
    
    # dummy loading time
    for (i in 1:10) {
      progress$set(value = i)
      Sys.sleep(0.3)
    }
    
    progress$close()
    removeModal()
    
    #################################################
    #        Simulation Results Individual          #
    #################################################
    
    # Retrieve the user input
    carpark_input <- input$carpark_input 
    num_simulations <- input$num_simulations
    capacity <- input$capacity
    month_st <- input$month_st
    month_en <- input$month_en
    lot_percentage_ind <- input$lot_percentage_ind
    increase_arrival_rate <- input$increase_arrival_rate / 100
    cp_index <- which(carparks == carpark_input)
    
    # Simulation results (single):
    res <- run_sim_n_times_s(n = num_simulations,
                             cp_index = cp_index,
                             cp_capacity = capacity,
                             cp_red_perc = lot_percentage_ind,
                             arrival_rate = increase_arrival_rate,
                             month_st = month_st,
                             month_en = month_en)
    occ_results <- res[[1]] %>% summarise(red_mean_parking = mean(red_mean_parking, na.rm = T),
                                          white_mean_parking = mean(white_mean_parking, na.rm = T),
                                          red_max_parking = mean(red_max_parking, na.rm = T),
                                          white_max_parking = mean(white_max_parking, na.rm = T))
    
    output$mean_occ_red <- renderUI(
      tags$div(style="color: red; font-size: 100%; text-align: center;", round(occ_results[[1]], 3))
    )
    output$mean_occ_white <- renderUI(
      tags$div(style="text-align: center;", round(occ_results[[2]], 3))
    )
    output$max_occ_red <- renderUI(
      tags$div(style="color: red; text-align: center;", round(occ_results[[3]], 3))
    )
    output$max_occ_white <- renderUI(
      tags$div(style="text-align: center;", round(occ_results[[4]], 3))
    )
    
    index_st = which(months == month_st)
    index_en = which(months == month_en)
    num_months = index_en - index_st + 1
    
    output$plot_red_usage <- renderUI({
      plot_output_list <- lapply(1:num_months, function(i) {
        plotOutput(paste0("plot", i))
        
        output[[paste0("plot", i)]] <- renderPlot({
          res[[2]][[i]] + theme_bw() + guides(color = "none")
        })
      })
      do.call(tagList, plot_output_list)
    })
    
    output$plot_white_usage <- renderUI({
      plot_output_list_2 <- lapply(1:num_months, function(i) {
        plotOutput(paste0("plot_white", i))
        
        output[[paste0("plot_white", i)]] <- renderPlot({
          res[[3]][[i]] + theme_bw() + guides(color = "none")
        })
      })
      do.call(tagList, plot_output_list_2)
    })
    
    showElement("results_indiv")
    showElement("plot_red_usage")
    showElement("plot_white_usage")
    
    #################################################
    #         Download Simulation Results           # 
    #################################################    
    
    output$downloadData <- downloadHandler(
      filename = "report_individual.pdf",
      content = function(file) {
        pdf(file, width = 8, height = 6)
        mean_red <- paste("Mean Occupancy rate (Red): ", round(occ_results[[1]], 3), sep = "")
        mean_white <- paste("Mean Occupancy rate (white): ", round(occ_results[[2]], 3), sep = "")
        max_red <- paste("Max Occupancy rate (Red): ", round(occ_results[[3]], 3), sep = "")
        max_white <- paste("Max Occupancy rate (white): ", round(occ_results[[4]], 3), sep = "")
        grid.text(mean_red, x = 0.5, y = 0.8, just = "center", gp = gpar(fontsize = 14))
        grid.text(mean_white, x = 0.5, y = 0.7, just = "center", gp = gpar(fontsize = 14))
        grid.text(max_red, x = 0.5, y = 0.4, just = "center", gp = gpar(fontsize = 14))
        grid.text(max_white, x = 0.5, y = 0.3, just = "center", gp = gpar(fontsize = 14))
        for (i in 1:num_months) {
          grid.arrange(res[[2]][[i]] + theme_bw() + guides(color = "none"), top = "Red Usage")
        }
        for (i in 1:num_months) {
          grid.arrange(res[[3]][[i]] + theme_bw() + guides(color = "none"), top = "White Usage")
        }
        dev.off()
      }
    )
  })
  
  observe({
    selected_carparks <- input$Options
    res_dict <- 1:6
    names(res_dict) <- names(carparks_capacity)
    
    # Enable all numeric inputs
    lapply(res_dict[names(carparks_capacity)], function(carpark_name) {
      input_id <- paste0("capacity_", carpark_name)
      shinyjs::enable(input_id)
    })
    
    # Disable and set default values for deselected carparks
    lapply(res_dict[setdiff(names(carparks_capacity), selected_carparks)], function(carpark_name) {
      input_id <- paste0("capacity_", carpark_name)
      shinyjs::disable(input_id)
      updateNumericInput(session, input_id, value = NA)
    })
    
    # Enable and set default values for selected carparks
    lapply(res_dict[selected_carparks], function(carpark_name) {
      input_id <- paste0("capacity_", carpark_name)
      shinyjs::enable(input_id)
      updateNumericInput(session, input_id, value = carparks_capacity[[carpark_name]])
    })
    
    selected_resources_group <- marker_data$resource[marker_data$carparks %in% selected_carparks]
    marker_data_2 <- marker_data[marker_data$resource %in% selected_resources_group, ]
    
    output$map <- renderLeaflet({
      m <- leaflet() %>%
        addTiles() %>%
        setView(lng = default_longitude, lat = default_latitude, zoom = default_zoom)
      
      #################################################
      #           Markers of Carpark locations        #
      #################################################
      for (i in 1:nrow(marker_data_2)) {
        popup_content <- paste(
          "<div class='custom-popup-box'>",
          "<h1>Mean Occupancy Rate</h1>",
          "<div class='red-box'><div class='red-box-text'>", "</div></div>",
          "<div class='white-box'><div class='white-box-text'>", "</div></div>",
          "</div>"
        )
        
        # Check if the carpark is selected in the pickerInput
        carpark_selected <- marker_data_2[i, "carparks"] %in% selected_carparks
        
        # Add a marker to the map only if the carpark is selected
        if (carpark_selected) {
          m <- addMarkers(
            m,
            data = marker_data_2[i,],  # Use data for the current row
            lng = ~lng,
            lat = ~lat,
            popup = HTML(popup_content),
            label = ~carparks
            #icon=marker_icon
          )
        }
      }
      m
    })
  })
  
  # Handle group page syncing of data
  observeEvent(input$run_button_group, {
    
    #################################################
    #                 Progress Bar                  #
    #################################################
    progress <- shiny::Progress$new(session, min = 0, max = 10)
    progress$set(message = "Processing", value = 0)
    
    showModal(modalDialog(
      title = "Hang on, carpark geniuses at work...",
      footer = NULL
    ))
    
    # dummy loading time
    for (i in 1:10) {
      progress$set(value = i)
      Sys.sleep(0.3)
    }
    
    progress$close()
    removeModal()
    
    #################################################
    #          Simulation Results Group             #
    #################################################
    
    state = c("closed", "closed", "closed", "closed", "closed", "closed")
    for (option in input$Options) {
      index = which(carparks == option)
      state[index] = "open"
    }
    
    # Retrieve the user input
    num_simulations_grp <- input$num_simulations_grp
    
    capacity_1 <- input$capacity_1
    capacity_2 <- input$capacity_2
    capacity_3 <- input$capacity_3
    capacity_4 <- input$capacity_4
    capacity_5 <- input$capacity_5
    capacity_6 <- input$capacity_6
    
    month_en <- input$month_en_group
    month_st <- input$month_st_group
    
    sheltered_percentage_3 <- input$sheltered_percentage_3
    sheltered_percentage_3A <- input$sheltered_percentage_3A
    sheltered_percentage_4A <- input$sheltered_percentage_4A
    sheltered_percentage_5 <- input$sheltered_percentage_5
    sheltered_percentage_5B <- input$sheltered_percentage_5B
    sheltered_percentage_6B <- input$sheltered_percentage_6B
    
    lot_percentage_3 <- input$lot_percentage_3
    lot_percentage_3A <- input$lot_percentage_3A
    lot_percentage_4 <- input$lot_percentage_4
    lot_percentage_5 <- input$lot_percentage_5
    lot_percentage_5B <- input$lot_percentage_5B
    lot_percentage_6B <- input$lot_percentage_6B
    
    increase_arrival_rate_group <- input$increase_arrival_rate_group / 100
    
    # Simulation results (multiple):
    simulation_results <- run_sim_n_times_m(n=num_simulations_grp,
                                            cp_list = c("3", "3a", "4", "5", "5b", "6b"),
                                            cp_state=state,
                                            cp_capacity = c(capacity_1, capacity_2, capacity_3, capacity_4, capacity_5, capacity_6),
                                            cp_sheltered = c(sheltered_percentage_3, sheltered_percentage_3A, sheltered_percentage_4A, sheltered_percentage_5, sheltered_percentage_5B, sheltered_percentage_6B),
                                            cp_red_perc = c(lot_percentage_3, lot_percentage_3A, lot_percentage_4, lot_percentage_5, lot_percentage_5B, lot_percentage_6B),
                                            arrival_rate = increase_arrival_rate_group,
                                            month_st = month_st,
                                            month_en = month_en,
                                            distance_matrix = matrix(c(
                                              0, 500, 1000, 150, 200, 250,
                                              500, 0, 800, 1300, 180, 230,
                                              1000, 800, 0, 600, 1100, 160,
                                              150, 1300, 600, 0, 500, 1000,
                                              200, 180, 1100, 500, 0, 500,
                                              250, 230, 160, 1000, 500, 0
                                            ), nrow = 6, byrow = TRUE,
                                            dimnames = list(cp_list, cp_list)))
    df_main <- simulation_results[[1]]
    plist_red_util <- simulation_results[[2]]
    plist_white_util <- simulation_results[[3]]
    plist_red_use <- simulation_results[[4]]
    plist_white_use <- simulation_results[[5]]
    
    # mean and max occupancy rates
    df_means <- df_main %>% group_by(resource, mon) %>%
      summarise(red_mean_parking = mean(red_mean_parking, na.rm = T),
                white_mean_parking = mean(white_mean_parking, na.rm = T),
                red_max_parking = mean(red_max_parking, na.rm = T),
                white_max_parking = mean(white_max_parking, na.rm = T),
                mean_sd_red = mean(sd_red), mean_sd_white = mean(sd_white))
    
    mean_red_group <-
      aggregate(
        red_mean_parking ~ resource, 
        data = df_means, 
        FUN = function(x) round(mean(x),3)
      )
    mean_white_group<-
      aggregate(
        white_mean_parking ~ resource, 
        data = df_means, 
        FUN = function(x) round(mean(x),3)
      )
    max_red_group <-
      aggregate(
        red_max_parking ~ resource, 
        data = df_means, 
        FUN = function(x) round(mean(x),3)
      )
    max_white_group<-
      aggregate(
        white_max_parking ~ resource, 
        data = df_means, 
        FUN = function(x) round(mean(x),3)
      )
    max_red_group_global <- as.vector(max_red_group$red_max_parking)
    max_white_group_global <- as.vector(max_white_group$white_max_parking)
    
    selected_carparks_group<-input$Options
    selected_resources_group <- marker_data$resource[marker_data$carparks %in% selected_carparks_group]
    mean_red_group_global <- as.vector(mean_red_group$red_mean_parking)
    mean_white_group_global <- as.vector(mean_white_group$white_mean_parking)
    selected_resources_group <- marker_data$resource[marker_data$carparks %in% selected_carparks_group]
    marker_data_2 <- marker_data[marker_data$resource %in% selected_resources_group, ]
    output$map <- renderLeaflet({
      m <- leaflet() %>%
        addTiles() %>%
        setView(lng = default_longitude, lat = default_latitude, zoom = default_zoom)
      
      #################################################
      #           Markers of Carpark locations        #
      #################################################
      for (i in 1:nrow(marker_data_2)) {
        popup_content <- paste(
          "<div class='custom-popup-box'>",
          "<h1>Mean Occupancy Rate</h1>",
          "<div class='red-box'><div class='red-box-text'>R Lots: ", mean_red_group_global[i], "</div></div>",
          "<div class='white-box'><div class='white-box-text'>W Lots: ", mean_white_group_global[i], "</div></div>",
          "</div>"
        )
        
        # Check if the carpark is selected in the pickerInput
        carpark_selected <- marker_data_2[i, "carparks"] %in% input$Options
        
        # Add a marker to the map only if the carpark is selected
        if (carpark_selected) {
          m <- addMarkers(
            m,
            data = marker_data_2[i,],  # Use data for the current row
            lng = ~lng,
            lat = ~lat,
            popup = HTML(popup_content),
            label = ~carparks
            #icon=marker_icon
          )
        }
      }
      m
    })
    observeEvent(input$btn, {
      showModal(modalDialog(size = "xl"))
    })
    
    ################################################
    #            GRP POPUP CLOSE BUTTON            #
    ################################################
    # Declare topRow and bottomRow globally
    topRow <- NULL
    bottomRow <- NULL
    
    # Reactive value to track modal visibility
    modalVisible <- reactiveVal(FALSE)
    
    observeEvent(input$overall_button, {
      # Function to create a box with Flexbox styles
      createFlexBox <- function(title, meanRate, maxRate, meanRed, maxRed, meanWhite, maxWhite) {
        return(
          tags$div(
            tags$p(title, style = "font-weight: bold; font-size: xx-large; margin-bottom: 10px; overflow: hidden;"),  
            tags$p(meanRate, style = "font-size: x-large; font-weight: bold; margin-bottom: 5px; overflow: hidden;"),  
            tags$p(HTML(paste('<span style="color: red;">', paste( meanRed, "</span><br><span style='color: white;'>", meanWhite, '</span>'))), style = "font-size: x-large; font-weight: bold; margin-bottom: 5px; overflow: hidden;"),  
            tags$p(maxRate, style = "font-size: x-large; font-weight: bold; margin-bottom: 5px; overflow: hidden;"),  
            tags$p(HTML(paste('<span style="color: red;">', paste( maxRed, "</span><br><span style='color: white;'>", maxWhite, '</span>'))), style = "font-size: x-large; font-weight: bold; margin-bottom: 5px; overflow: hidden;"),  
            style = "display: flex; flex-direction: column; justify-content: space-between; align-items: center; width: 300px; height: 380px; overflow: hidden;",  # Adjust width and height as needed
            class = "custom-box custom-width custom-height custom-blue-box"
          )
        )
      }
      
      combined_df <- Reduce(function(x, y) left_join(x, y, by = "resource"), 
                            list(mean_red_group, mean_white_group, max_red_group, max_white_group))
      carparks <- list(
        list(title = "Carpark 3", meanRate = "Mean Occupancy Rate", maxRate = "Max Occupancy Rate", meanRed = mean_red_group_global[1], maxRed = max_red_group_global[1], meanWhite = paste("White:", mean_white_group_global[1]), maxWhite = paste("White:", max_white_group_global[1]),resource="car_park_3"),
        list(title = "Carpark 3A", meanRate = "Mean Occupancy Rate", maxRate = "Max Occupancy Rate", meanRed = mean_red_group_global[2], maxRed = max_red_group_global[2], meanWhite = paste("White:", mean_white_group_global[2]), maxWhite = paste("White:", max_white_group_global[2]),resource="car_park_3a"),
        list(title = "Carpark 4/4A", meanRate = "Mean Occupancy Rate", maxRate = "Max Occupancy Rate", meanRed = mean_red_group_global[3], maxRed = max_red_group_global[3], meanWhite = paste("White:", mean_white_group_global[3]), maxWhite = paste("White:", max_white_group_global[3]),resource="car_park_4"),
        list(title = "Carpark 5", meanRate = "Mean Occupancy Rate", maxRate = "Max Occupancy Rate", meanRed = mean_red_group_global[4], maxRed = max_red_group_global[4], meanWhite = paste("White:", mean_white_group_global[4]), maxWhite = paste("White:", max_white_group_global[4]),resource="car_park_5"),
        list(title = "Carpark 5B", meanRate = "Mean Occupancy Rate", maxRate = "Max Occupancy Rate", meanRed = mean_red_group_global[5], maxRed = max_red_group_global[5], meanWhite = paste("White:", mean_white_group_global[5]), maxWhite = paste("White:", max_white_group_global[5]),resource="car_park_5b"),
        list(title = "Carpark 6B", meanRate = "Mean Occupancy Rate", maxRate = "Max Occupancy Rate", meanRed = mean_red_group_global[6], maxRed = max_red_group_global[6], meanWhite = paste("White:", mean_white_group_global[6]), maxWhite = paste("White:", max_white_group_global[6]),resource="car_park_6b")
      )
      
      for (i in seq_along(carparks)) {
        # Extract the resource value from the current carpark list
        resource_value <- carparks[[i]]$resource
        
        # Check if the resource_value exists in the global vectors
        if (!(resource_value %in% combined_df$resource)) {
          # If not, update the values in the carpark list to "close:"
          carparks[[i]]$meanRed <- paste("Red:", "closed")
          carparks[[i]]$maxRed <- paste("Red:", "closed")
          carparks[[i]]$meanWhite <- paste("White:", "closed")
          carparks[[i]]$maxWhite <- paste("White:", "closed")
        } else {
          # If it exists, update the values to the corresponding global vectors
          index <- combined_df$resource == resource_value
          carparks[[i]]$meanRed <- paste("Red:", mean_red_group_global[index])
          carparks[[i]]$maxRed <- paste("Red:", max_red_group_global[index])
          carparks[[i]]$meanWhite <- paste("White:", mean_white_group_global[index])
          carparks[[i]]$maxWhite <- paste("White:", max_white_group_global[index])
        }
      }
      
      observeEvent(input$overall_button, {
        # Check if the modal is already visible
        if (!modalVisible()) {
          # Update the global variables
          topRow <<- tagList(
            fluidRow(
              column(8, h3("Overall Statistics")),  # Title column
              column(4, align = "right", actionButton("dismiss_button", "Close", class = "btn btn-link", style = "font-size: 21px;" )),
            ),
            fluidRow(
              lapply(1:3, function(i) column(4, createFlexBox(carparks[[i]]$title, carparks[[i]]$meanRate, carparks[[i]]$maxRate, carparks[[i]]$meanRed, carparks[[i]]$maxRed, carparks[[i]]$meanWhite, carparks[[i]]$maxWhite)))
            )
          )
          
          bottomRow <<- fluidRow(
            lapply(4:6, function(i) column(4, createFlexBox(carparks[[i]]$title, carparks[[i]]$meanRate, carparks[[i]]$maxRate, carparks[[i]]$meanRed, carparks[[i]]$maxRed, carparks[[i]]$meanWhite, carparks[[i]]$maxWhite)))
          )
          
          # Show the modal with all carparks and updated title
          showModal(modalDialog(
            title = NULL,  # Setting title to NULL to use custom title layout
            size = "xl",
            topRow,
            bottomRow,
            footer = NULL
          ))
          
          # Set modal visibility to TRUE
          modalVisible(TRUE)
        }
      })
    })
    
    observeEvent(input$dismiss_button, {
      # Close the modal when the dismiss button is clicked
      removeModal()
      # Set modal visibility to FALSE
      modalVisible(FALSE)
    })
    
    #################################################
    #     Download Group Simulation Results         # 
    #################################################    
    
    output$download_data_group <- downloadHandler(
      filename = "report_multiple.pdf",
      content = function(file) {
        pdf(file, width = 8, height = 6)
        for (i in 1:length(selected_carparks_group)) {
          grid.newpage()
          grid.text(selected_carparks_group[i], x = 0.5, y = 0.8, just = "center", gp = gpar(fontsize = 14))
          mean_red <- paste("Mean Occupancy Rate (Red): ", round(mean_red_group_global[[i]], 3), sep = "")
          mean_white <- paste("Mean Occupancy Rate (White): ", round(mean_white_group_global[[i]], 3), sep = "")
          max_red <- paste("Max Occupancy Rate (Red): ", round(max_red_group_global[[i]], 3), sep = "")
          max_white <- paste("Max Occupancy Rate (White): ", round(max_red_group_global[[i]], 3), sep = "")
          grid.text(mean_red, x = 0.5, y = 0.6, just = "center", gp = gpar(fontsize = 14))
          grid.text(mean_white, x = 0.5, y = 0.5, just = "center", gp = gpar(fontsize = 14))
          grid.text(max_red, x = 0.5, y = 0.3, just = "center", gp = gpar(fontsize = 14))
          grid.text(max_white, x = 0.5, y = 0.2, just = "center", gp = gpar(fontsize = 14))
        }
        dev.off()
      }
    )
  })
  
  
  #################################################
  #         DataTable of Past Simulations         #
  #################################################
  
  # simulated database
  data <- read.csv("data/simulations.csv")
  
  output$table <- DT::renderDataTable(
    data,
    class = "display nowrap",
    selection = list(mode = 'multiple'),
    options = list(
      scrollY = TRUE,
      autoWidth = FALSE,
      lengthChange = FALSE,
      paging = TRUE,
      pageLength = 7,
      searching = FALSE,
      initComplete = JS(
        "function(settings, json) {",
        "$('th').css({'background-color': '#EF7C00'});",
        "$('th').css({'color': 'white'});",
        "$('th:first-child').css({'border-top-left-radius': '15px'});",
        "$('th:last-child').css({'border-top-right-radius': '15px'});",
        "}")
    )
  )
  
  # show selected rows
  output$selected_rows <- renderText({
    selected_data <- data[input$table_rows_selected, "datetime"]
    selected_values <- paste(selected_data, collapse = ", ")
    selected_values
  })
  
  observeEvent(input$view_button, {
    if (!(length(input$table_rows_selected) == 1)) {
      showElement("error_message_view")
    } else {
      hideElement("table")
      showElement("history_view")
    }
  })
  
  observeEvent(input$compare_button, {
    if (!(length(input$table_rows_selected) == 2)) {
      showElement("error_message_compare")
    } else {
      showElement("history_compare")
      hideElement("table")
    }
  })
  
  # proxy to reset selected rows
  proxy <- dataTableProxy("table")
  observeEvent(input$reset_button_history, {
    proxy %>% selectRows(NULL)
    showElement("table")
    hideElement("error_message_view")
    hideElement("error_message_compare")
    hideElement("history_view")
    hideElement("history_compare")
  })
  
  #################################################
  #                  Upload Page                  #
  #################################################
  
  # # increase max file size that can be uploaded to 30MB
  options(shiny.maxRequestSize=30*1024^2) 
  
  # upload data to folder in frontend
  files_data <- reactiveValues(files_list = list()) # store new file paths
  
  observeEvent(input$file, {
    req(input$file)
    for (i in seq_along(input$file$name)) {
      new_path <- file.path("uploaded_data", input$file$name[i])
      file.rename(input$file$datapath[i], new_path) # move file to the new location
      files_data$files_list[[input$file$name[i]]] <- new_path # update reactive val
    }
    
    # display file name
    output$data_preview <- renderUI({
      output_list <- lapply(names(files_data$files_list), function(name) {
        table_id <- paste0("table_", gsub("[[:punct:]]", "", name)) # clean file name for a valid output ID
        # create title and table placeholder for each file
        tagList(
          tags$h4(name),
          dataTableOutput(outputId = table_id),
          tags$br()
        )
      })
      do.call(tagList, output_list) # combine UI elements into a single tagList
    })
    
    # display tables
    lapply(names(files_data$files_list), function(name) {
      table_id <- paste0("table_", gsub("[[:punct:]]", "", name))
      # render table
      output[[table_id]] <- renderDataTable({
        read.csv(files_data$files_list[[name]])
      }, options = list(pageLength = 5, autoWidth = TRUE))
    })
    
    # sync with backend
    data_update_pipeline(filepaths = files_data$files_list)
  })
  
}


shinyApp(ui, server)