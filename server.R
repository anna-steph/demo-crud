# See global.R for setup

# Server ---------------------------------------------------------------------

message("Run server")

server <- function(input, output, session) {
  
  # DT Filtered Tab -----------------------------------------------------------
  message("DT Filtered")
  
  # README
  observeEvent(input$show_dt_readme, {
    showNotification(dt_readme,
                     type = "message",
                     duration = 30)
  })

  # Property type lookup, with formatted dropdown table
  observe({
    
    if (input$dt_proptype == TRUE) {
      
      updateSelectizeInput(session, "dt_pick_prop", choices = list(
        `Single Family` = c("Single Family Residential", "Townhouse"),
        `Multi-Family` = c("Condo/Co-op", "Multi-Family (2-4 Unit)", "Multi-Family (5+ Unit)")
        ),
        selected = "Condo/Co-op")
    }
  })

  # Filtered reactive table
  redfin_data <- reactive({
    
    input$dt_refresh

    # list_date date range is the limiting data pipeline variable
    dt_start <- input$dt_daterange[1]
    dt_end <- input$dt_daterange[2]

    # Message to flag unreasonable dates
    validate(
      need(
        dt_end >= dt_start,
        "Review date range: start date later than end date.")
    )

    # Pull from redfin_pool defined in database_connections.R
    redfin <- redfin_pool %>%
      tbl("redfin_demo") %>%
      collect() %>%
      mutate(across(c(sold_date, list_date, seen_date), 
                    ~ as.Date(., "%m/%d/%Y"))) %>%
      filter(list_date >= dt_start & list_date <= dt_end) %>%
      mutate(
        priority = case_when(
          priority == 1 ~ "yes",
          priority == 2 ~ "maybe",
          TRUE ~ "")
        )
    
    # Syntax if data is in schema myschema:
    # redfin <- redfin_pool %>%
    #   tbl(dbplyr::in_schema("myschema", "redfin_demo")) %>%
    #   collect()

    # Format keywords
    keyword_list <- paste(input$dt_pick_keywords, collapse = "|") %>%
      tolower()

    filtered <- redfin %>%
      filter(
        # Max Price
        price <= input$dt_max_price * 1000,
        # Priority
        conditional(input$dt_priority == TRUE,
                    (priority == "yes" | priority == "maybe")), # note priority is char
        # Location
        conditional(input$dt_city == TRUE,
                    city %in% format_input(input$city_id)),
        # Conditional Prop type
        conditional(input$dt_proptype == TRUE,
                    property_type %in% format_input(input$dt_pick_prop)),
        # Conditional Keywords
        conditional(input$dt_keywords == TRUE,
                    grepl(keyword_list,
                          tolower(address)))
      ) %>%
      arrange(city, desc(list_date))
  })

  output$dt_filtered <- DT::renderDT(

            DT::datatable(redfin_data(),
                  rownames = FALSE, # no obs index
                  selection = "single",
                  extensions = c("KeyTable", "RowGroup"), # Group rows
                  filter = list(positions = "top", clear = FALSE),
                  options = list(keys = TRUE,
                                 rowGroup = list(dataSrc = c(6)), # Group rows by col 7
                                 columnDefs = list(list(visible = FALSE,
                                                        targets = c(0, 2, 3, 7, 8, 13, 15))), # hide cols
                                 dom = "Blfrtip",
                                 orientation = "landscape",
                                 searchHighlight = TRUE,
                                 search = list(smart = TRUE),
                                 scrollX = TRUE,
                                 scrollY = "500px", # allows column names to freeze at top
                                 pageLength = 50,
                                 orderClasses = TRUE)
    ), 
    server = FALSE # if true, only prints rows visible on the page
  ) # close table

  output$dt_proplist <- downloadHandler(
    filename = "redfin_DTtable.csv",
    content = function(file) {
      write.csv(redfin_data(),
                file, row.names = FALSE)
    }
  )

  dt_tstamp <- reactive({input$dt_refresh

    tstamp <- file.mtime("data/redfin_demo.sqlite3")
    state_tstamp <- paste0("Last updated at:  ", tstamp)
    state_tstamp

  })

  output$dt_timestamp <- renderText({
    dt_tstamp()
  })

  # Rhandsontable CRUD Tab --------------------------------------------------
  message("Rhandsontable CRUD")

  # README
  observeEvent(input$show_rhandson_readme, {
    showNotification(rhandson_readme,
                     type = "message",
                     duration = 30)
  })

  # Redfin data filtered by keywords in address
  rh_redfin_data <- reactive({

    input$rh_city_id
    input$rh_prop_id

    # Filtered pull from redfin_pool defined in database_connections.R
    # Note: any data cut here needs to be rejoined with SQL
    # This call doesn't cut fields; full rows are appended back to main df
    redfin <- redfin_pool %>%
      tbl("redfin_demo") %>%
      collect() %>%
      filter(
        #City
        city %in% format_input(input$rh_city_id),
        property_type %in% format_input(input$rh_prop_id)
      ) %>%
      mutate(
        priority = case_when(
          priority == 1 ~ "yes",
          priority == 2 ~ "maybe",
          TRUE ~ ""
          )
      ) %>%
      arrange(city, list_date)
  })

  # Update and reset data from multiple inputs

  redfin_values <- reactiveValues(data = as.data.frame(NULL))

  # Begin
  observeEvent(
                {input$rh_city_id # note multiple inputs
                input$rh_prop_id
                1}, {
    redfin_values$data <- rh_redfin_data()
  })

  # Reset back to rh_redfin_data ---NOTE this does not re-pull from SQL
  observeEvent(input$reset_input, {
    redfin_values$data <- rh_redfin_data()
  })

  message("Rhandsontable")

  output$rhandson_redfin <- renderRHandsontable({

    input$rh_city_id
    input$rh_prop_id

    # Dropdown menu options - can also be taken from a flat file lookup
    priority_options <- c("yes", "maybe", "")

    table <- rhandsontable({redfin_values$data},
                           search = TRUE,
                           highlightRow = TRUE,
                           highlightCol = TRUE,
                           stretchH = "all",
                           height = 500,
                           width = 1000,
                           rowHeaders = FALSE,
                           selectCallback = TRUE) %>%
      hot_context_menu(allowRowEdit = TRUE,
                       allowColEdit = FALSE) %>%
      # Minimize columns that don't require user reference or edits
      hot_col(col = c("id", "sale_type", "sold_date", "state",
                      "zip_code", "lot_size", "year_built", "status",
                      "metro_distance"),
              colWidths = 0.1) %>%
      hot_col(col = "priority", type = "dropdown",
              source = priority_options) %>%
      hot_col(col = "notes", colWidths = 80)

    table

  })

  # Save user-edited table

  message("Save data")

  observeEvent(input$save_button, {

    # Pull table from UI - note fields likely formatted as character
    temp <- as.data.frame(hot_to_r(req(input$rhandson_redfin)))

    # Format for SQLite col types
    temp_sql <- temp %>%  
      mutate(priority = case_when(
        priority == "yes" ~ 1,
        priority == "maybe" ~ 2,
        priority == "" ~ 0
      )) %>%
      mutate(across(c(sold_date, list_date, seen_date),
                    ~ as.character(.))) %>%
      mutate(across(c(id, price, beds, baths, zip_code, square_feet, priority),
                    ~ as.numeric(.)))

    # Add updates to data
    
    update_ids <- temp_sql$id

    orig_table <- redfin_pool %>%
      tbl("redfin_demo") %>%
      collect() %>%
      mutate(across(c(sold_date, list_date, seen_date),
                    ~ as.character(.))) %>%
      mutate(across(c(id, price, beds, baths, zip_code, square_feet, priority),
                    ~ as.numeric(.))) %>%
      filter(!(id %in% all_of(update_ids))) %>%
      bind_rows(temp_sql)

    write.csv(orig_table,
              file = "tests/redfindemo_backup.csv",
              row.names = FALSE)

    RSQLite::dbWriteTable(redfin_pool, "redfin_demo", orig_table, overwrite = TRUE)

    message("Saved")

  }) # close save

  rh_tstamp <- reactive({

    input$save_button
    tstamp <- file.mtime("data/redfin_demo.sqlite")
    state_tstamp <- paste0("Last updated at:  ", tstamp)
    state_tstamp

  })

  output$rh_timestamp <- renderText({
    rh_tstamp()
  })

  # Priority price distribution tab ------------------------------------------
  message("Priority price dist")
  
  # README Notification for disk tab
  observeEvent(input$show_dist_readme, {
    showNotification(dist_readme,
                     type = "message",
                     duration = 30)
  })
  
  # "Priority" price dist by property type: data table
  priority_price <- reactive({
    
    input$dist_refresh
    input$dist_city_id
    
    # Adding message to choose property type
    validate(
      need(
        !(is.na(input$dist_city_id)),
        "Select a location")
    )
    
  filtered <- redfin_pool %>%
            tbl("redfin_demo") %>%
            collect() %>%
            filter(
              priority == 1,
              city %in% format_input(input$dist_city_id)
            ) %>%
            arrange(price)

  filtered

  })
  
  output$dist_table <- DT::renderDT(
    
    DT::datatable(priority_price(),
                  rownames = FALSE, # no obs index
                  selection = "single",
                  extensions = c("KeyTable", "Buttons"),
                  filter = list(positions = "top", clear = FALSE),
                  options = list(keys = TRUE,
                                 columnDefs = list(list(visible = FALSE,
                                                        targets = c(0, 2, 3, 7, 8, 13, 15))), # hide cols
                                 dom = "Blfrtip",
                                 orientation = "landscape",
                                 buttons = list(list(extend = "colvis",
                                                     text = "Hide Fields"),
                                                list(extend = "collection",
                                                     buttons = c("csv", "excel"),
                                                     text = "Spreadsheet")),
                                 searchHighlight = TRUE,
                                 search = list(smart = TRUE),
                                 scrollX = TRUE,
                                 scrollY = "300px", # allows column names to freeze at top
                                 pageLength = 50,
                                 orderClasses = TRUE)) %>%
      DT::formatCurrency(c(9), digits = 0, currency = "$"),
    server = FALSE # if true, only prints rows visible on the page
  ) # close table
  
  priority_price_dist <- reactive({
    
    input$dist_refresh
    input$dist_city_id

    # Adding message to choose property type
    validate(
      need(
        !(is.na(input$dist_city_id)),
        "Select a location")
    )

    dist_df <- priority_price()
    
    dist <- ggplot(dist_df,
                   aes(x = price, group = property_type, fill = property_type)) +
      geom_density(adjust = 1.5, colour = "black", size = 0.15) +
      theme_bw() +
      scale_fill_brewer() +
      facet_wrap(~property_type) +
      theme(
        legend.position = "none",
        panel.spacing = unit(0.1, "lines"),
        axis.ticks.x = element_blank()
      ) +
      ggtitle(paste("Priority",
                    input$dist_city_id,
                    "Home Prices by Property Type",
                    sep = " ")) +
      labs(x = "Price ($)")

    dist

  })
  
  output$dist_plot <- renderPlot({ priority_price_dist() })
  
  # Show time of last update
  
  dist_tstamp <- reactive({
    
    input$dist_refresh
    tstamp <- file.mtime("data/redfin_demo.sqlite3")
    
    state_tstamp <- paste0("Redfin table last updated at:  ", tstamp)
    state_tstamp
    
  })
  
  output$dist_timestamp <- renderText({
    dist_tstamp()
  })

} # close server