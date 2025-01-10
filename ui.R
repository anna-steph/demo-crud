# See global.R for setup

# UI -------------------------------------------------------------------------

message("ui")

ui <- fluidPage(
  
  # Navbar structure
  navbarPage(title = "Shiny Demo with Redfin",
             theme = shinytheme("lumen"),
             windowTitle = "Shiny Demo",
             
   # DT Table with filters ------------------------------------------
   tabPanel("DT Table",  fluid = TRUE,
            icon = icon("table", lib = "font-awesome"),
            
    titlePanel("Redfin DT Table"),
    
    sidebarLayout(
      
      # Options ------------------------------------------
      sidebarPanel(
        
        # Notification -----------------------------------
        tags$head(
          tags$style(
            HTML(".shiny-notification {
                  position:fixed;
                  top: calc(14%);
                  left: calc(18%);
                     }"))),
        actionButton("show_dt_readme", "README"),
        br(),
        br(),

        # Refresh ---------------------------------------
        actionButton("dt_refresh", "Refresh"),
        br(),
        br(),

        # Date Range -----------------------------------
        dateRangeInput("dt_daterange",
                       "List Date",
                       start = as.Date("20170801", format = "%Y%m%d"),
                       end = as.Date("20170930", format = "%Y%m%d"),
                       min = as.Date("19990630", format = "%Y%m%d"),
                       max = Sys.Date() - 7,
                       format = "mm-dd-yyyy"),

        # Priority Entries -------------------------------
        checkboxInput("dt_priority",
                      "Priority",
                      value = FALSE
        ),

        # City update from data ---------------------
        checkboxInput("dt_city", "Location"),
        conditionalPanel(
          condition = "input.dt_city == true",
          selectInput("city_id",
                      "Locations",
                      choices = city_choices$city, # from global.R
                      selected = default_city,
                      multiple = TRUE)
        ),

        # Conditional selection ---------------------
        checkboxInput("dt_proptype", "Property Type"),
        conditionalPanel(
          condition = "input.dt_proptype == true",
          selectizeInput("dt_pick_prop",
                         "Types",
                         choices = NULL,
                         multiple = TRUE)
        ),

        # Max price --------------------------------------
        numericInput("dt_max_price",
                     "Max Price (in thousands, up to $5 million)",
                     value = 5 * 10^3,
                     min = 0,
                     max = 5 * 10^3,
                     step = 5 * 10^2),

        # Keyword search --------------------------------
        checkboxInput("dt_keywords", "Keywords in Address"),
        conditionalPanel(
          condition = "input.dt_keywords == true",
          textInput("dt_pick_keywords", "Keywords")
        ),
        
        # Save csv button -------------------------------
        downloadButton("dt_proplist", "CSV"),
        br(),
        br(),
        
        # Set width of sidebar panel --------------------
        width = 3
        
      ), # sidebar panel
      
      mainPanel(
        h3("DT Table with Filters"),
        div(
          textOutput("dt_timestamp")
        ),
        br(),
        div(
          DT::DTOutput("dt_filtered") %>%
            withSpinner(color = "#99ffdd")
        )
      ) # main panel

    ) # sidebar layout
 
   ), # DT Table tab panel

   # Rhandsontable ------------------------------------------
   tabPanel("Rhandson CRUD",  fluid = TRUE,
            icon = icon("clipboard", lib = "font-awesome"),

          # Page navigation warning ---------------------------
          tags$head(tags$script(HTML("
          // Navigation notification
          window.onbeforeunload = function() {
            return true;
          };
        "))),

        titlePanel("Redfin Rhandson CRUD"),

        sidebarLayout(

          # Options ------------------------------------------
          sidebarPanel(

            # Notification -----------------------------------
            tags$head(
              tags$style(
                HTML(".shiny-notification {
                  position:fixed;
                  top: calc(14%);
                  left: calc(18%);
                     }"))),
            actionButton("show_rhandson_readme", "README"),
            br(),
            br(),

            # City ---------------------
            selectizeInput(
              "rh_city_id",
              "City",
              choices = city_choices$city, # from global.R
              selected = default_city,
              multiple = TRUE
            ),

            # Property type ---------------------
            selectizeInput(
              "rh_prop_id",
              "Property Type",
              choices = prop_choices$property_type, # from global.R
              selected = "Condo/Co-op",
              multiple = TRUE
            ),

            # Save button -----------------------------------
            actionButton("save_button", "Save Data", icon("paper-plane")),
            br(),
            br(),
            
            # Reset button ----------------------------------
            actionButton("reset_input", "Reset"),
            br(),
            br(),

            # Set width of sidebar panel --------------------
            width = 3

          ), # sidebar panel

          mainPanel(
            h3("Rhandsontable CRUD"),
            div(
              textOutput("rh_timestamp")
            ),
            br(),
            div(
              rHandsontableOutput("rhandson_redfin") %>%
                withSpinner(color = "#99ffdd")
            )
          ) # main panel

        ) # sidebar layout

   ), # Rhandsontable tab panel

   # Priority price dist panel -----------------------------------------------
   tabPanel("View Priority",  fluid = TRUE,
            icon = icon("chart-area", lib = "font-awesome"),
            
    titlePanel("View Priority Homes"),
    
    sidebarLayout(
      
      # Options ------------------------------------------
      sidebarPanel(
        
        # Notification -----------------------------------
        tags$head(
          tags$style(
            HTML(".shiny-notification {
          position:fixed;
          top: calc(14%);
          left: calc(18%);
             }"))),
        actionButton("show_dist_readme", "README"),
        br(),
        br(),

        # Refresh ---------------------------------------
        actionButton("dist_refresh", "Refresh"),
        br(),
        br(),

        # City update from data ---------------------
        selectizeInput(
          "dist_city_id",
          "City",
          choices = city_choices$city, # from global.R
          selected = default_city,
          multiple = FALSE
        ),

        # Set width of sidebar panel -------------------
        width = 2

      ), # sidebar panel
      
      mainPanel(
        div(
          textOutput("dist_timestamp")
        ),
        br(),
        plotOutput("dist_plot") %>%
          withSpinner(color = "#99ffdd"),
        h3("Priority Home Data"),
        div(
          DT::DTOutput("dist_table") %>%
            withSpinner(color = "#99ffdd")
        )
      ) # main panel
      
    ) # sidebar layout
            
   ) # Priority price dist panel

  ) # navbar page

) # fluid page
