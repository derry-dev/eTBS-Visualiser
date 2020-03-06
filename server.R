function(input, output, session) {
  
  # Store client session data
  cdata <- session$clientData
  
  # Debug session data display
  onclick("think_logo", showModal(debug_dialogue()))
  output$clientdataText <- renderText({
    cnames <- sort(names(cdata)) %>% .[. %!in% grep("^output_spinner.*$", ., value=T)]
    allvalues <- lapply(cnames, function(name) {
      paste(name, cdata[[name]], sep = " = ")
    })
    paste(allvalues, collapse = "\n")
  })
  
  # Think Logo
  output$think_logo <- renderImage({
    list(
      src = "www/Think_Logo_White.png",
      contentType = "image/png",
      height = 37,
      width = 100
    )
  }, deleteFile = F)
  
  # Loading Spinner Icon
  output$spinner <- renderUI({
    # htmltools::HTML('<i class="fa fa-spinner fa-spin fa-3x fa-fw" style="font-size:24px"></i>')
    htmltools::HTML('<div class="loader"></div>')
  })
  
  # Start-up show database connection dialogue
  showModal(connection_dialogue())
  
  # Show database connection dialogue on button click
  onclick("db_button", showModal(connection_dialogue()))
  
  # Database connection
  con <- eventReactive(input$db_connect, {
    get_db_connection(input$db_driver, input$db_server, input$db_database, input$db_username, input$db_password)
  })
  
  # Database connection status feedback
  observeEvent(con(), {
    output$db_status <- renderUI({
      if (con() != -1L) {
        removeModal()
      } else {
        div(style="margin: 7px 0 0 6px;", "Error", icon("times-circle"))
      }
    })
  })
  
  # ----------------------------------------------------------------------- #
  # Database Tab ------------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  # Get custom query
  query <- eventReactive(input$db_execute, {
    input$db_query %>% sqlQuery(con(),.) %>% as.data.table()
  })
  
  # Render custom query table
  output$db_output <- DT::renderDataTable({
    datatable(
      query(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = T)
  
  # Clear query textbox
  observeEvent(input$db_clear, {
    updateTextAreaInput(session, "db_query", value="")
  })
  
  # Database stats
  
  # tbl_Flight_Plan
  flightplan <- reactive({
    " SELECT
      	t1.Flight_Plan_ID,
      	t1.FP_Date,
      	t1.FP_Time,
      	t1.Callsign,
      	t1.SSR_Code,
      	t1.Aircraft_Type,
      	t3.Wake AS \"Wake_Vortex\",
      	t1.Origin,
      	t1.Destination,
      	t1.STAR,
      	t1.SID,
      	t1.Landing_Runway,
      	t1.Departure_Runway,
      	t2.Time_At_4DME,
      	t2.Time_At_1DME
      FROM tbl_Flight_Plan AS t1
      LEFT JOIN (
      	SELECT Flight_Plan_ID AS Flight_Plan_ID, Time_At_4DME, Time_At_1DME FROM tbl_Flight_Plan_Derived
      ) AS t2 ON t1.Flight_Plan_ID = t2.Flight_Plan_ID
      LEFT JOIN (
      	SELECT * FROM tbl_Aircraft_Type_To_Wake
      ) AS t3 ON t1.Aircraft_Type = t3.Aircraft_Type
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_fp_table <- DT::renderDataTable({
    datatable(
      flightplan(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = T)
  
  # tbl_Landing_Pair
  landing_pairs <- reactive({
    lp <- "SELECT * FROM tbl_Landing_Pair" %>% sqlQuery(con(),.) %>% as.data.table()
    lead <- flightplan(); names(lead) <- paste0("Leader_", names(lead))
    foll <- flightplan(); names(foll) <- paste0("Follower_", names(foll))
    x <- merge(merge(lp, lead, by = "Leader_Flight_Plan_ID"), foll, by = "Follower_Flight_Plan_ID")
    setcolorder(x, c(3, 4, 5, 2, seq(6, 19, 1), 1, seq(20, length(x), 1)))
    return(x)
  })
  output$db_lp_table <- DT::renderDataTable({
    datatable(
      landing_pairs(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = T)
  
  # tbl_Polygon
  volumes <- eventReactive(con(), {
    " SELECT * FROM tbl_Polygon
      LEFT JOIN (
        SELECT Volume_Name AS Volume_Name_2, Min_Altitude, Max_Altitude FROM tbl_Volume
      ) AS t ON Volume_Name = Volume_Name_2
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_volumes <- DT::renderDataTable({
    datatable(
      volumes(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = T)
  
  # tbl_Path_Leg
  legs <- eventReactive(con(), {
    " SELECT * FROM tbl_Path_Leg
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_legs <- DT::renderDataTable({
    datatable(
      legs(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = T)

  # Flight Plan Stats
  
  db_fp_stats <- reactive({
    fp_id <- " SELECT Flight_Plan_ID FROM tbl_Flight_Plan
    " %>% sqlQuery(con(),.) %>% unlist() %>% as.vector()
    fpd_id <- " SELECT Flight_Plan_ID FROM tbl_Flight_Plan_Derived
    " %>% sqlQuery(con(),.) %>% unlist() %>% as.vector()
    both_id <- intersect(fp_id, fpd_id)
    
    fp_gen <- data.table(
      `Name` = c(
        "tbl_Flight_Plan ID count",
        "tbl_Flight_Plan orphaned IDs",
        "tbl_Flight_Plan_Derived ID count",
        "tbl_Flight_Plan_Derived orphaned IDs",
        "IDs missing Time_At_4DME",
        "IDs missing Time_At_1DME",
        "IDs missing WTC",
        "Aircraft Types without WTC"
      ),
      Value = c(
        length(fp_id),
        paste(setdiff(fp_id, both_id),  collapse = ", "),
        length(fpd_id),
        paste(setdiff(fpd_id, both_id),  collapse = ", "),
        length(flightplan()$Time_At_4DME %>% .[is.na(.)]),
        length(flightplan()$Time_At_1DME %>% .[is.na(.)]),
        length(flightplan()$Wake_Vortex %>% .[is.na(.)]),
        paste(unique(flightplan()[is.na(Wake_Vortex)]$Aircraft_Type), collapse = ", ")
      )
    )
    
    fp_ac <- as.data.table(table(flightplan()$Aircraft_Type))[order(V1)]
    names(fp_ac) <- c("Aircraft Type", "Count")
    fp_ac$`Percentage (Numeric)` <- fp_ac$Count / sum(fp_ac$Count)
    fp_ac$`Percentage (String)` <- paste0(round(fp_ac$Count / sum(fp_ac$Count) * 100, 3), "%")
    
    fp_wake <- as.data.table(table(flightplan()$Wake_Vortex))
    names(fp_wake) <- c("Wake Cat", "Count")
    fp_wake$`Wake Cat` <- factor(fp_wake$`Wake Cat`, levels = c("J", "H", "UM", "M", "S", "L", LETTERS[1:7], NA))
    fp_wake <- fp_wake[order(`Wake Cat`)]
    fp_wake$`Percentage (Numeric)` <- fp_wake$Count / sum(fp_wake$Count)
    fp_wake$`Percentage (String)` <- paste0(round(fp_wake$Count / sum(fp_wake$Count) * 100, 3), "%")
    
    fp_lrwy <- as.data.table(table(flightplan()$Landing_Runway))[order(V1)]
    names(fp_lrwy) <- c("Landing Runway", "Count")
    fp_lrwy$`Percentage (Numeric)` <- fp_lrwy$Count / sum(fp_lrwy$Count)
    fp_lrwy$`Percentage (String)` <- paste0(round(fp_lrwy$Count / sum(fp_lrwy$Count) * 100, 3), "%")
    
    fp_lrwyt <- as.data.table(table(flightplan()$Landing_Runway, as.numeric(flightplan()$FP_Time) %/% 3600))
    names(fp_lrwyt) <- c("Landing Runway", "Hour", "Count")
    fp_lrwyt$Hour <- as.numeric(fp_lrwyt$Hour)
    fp_lrwyt$Count <- paste0(fp_lrwyt$Count, " (", round(fp_lrwyt$Count / sum(fp_lrwyt$Count) * 100, 3), "%)")
    
    return(list(
      fp_gen = fp_gen,
      fp_ac = fp_ac,
      fp_wake = fp_wake,
      fp_lrwy = fp_lrwy,
      fp_lrwyt = tidyr::spread(fp_lrwyt, Hour, Count)
    ))
  })
  output$db_fp_general_table <- DT::renderDataTable({
    datatable(
      db_fp_stats()[["fp_gen"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  output$db_fp_type_table <- DT::renderDataTable({
    datatable(
      db_fp_stats()[["fp_ac"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  output$db_fp_wake_table <- DT::renderDataTable({
    datatable(
      db_fp_stats()[["fp_wake"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  output$db_fp_lrwy_table <- DT::renderDataTable({
    datatable(
      db_fp_stats()[["fp_lrwy"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  output$db_fp_lrwyt_table <- DT::renderDataTable({
    datatable(
      db_fp_stats()[["fp_lrwyt"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  
  # Landing Pair Stats
  
  observeEvent(landing_pairs(), {
    db_lp_type_choices <- landing_pairs()$Landing_Pair_Type %>% as.character() %>% unique()
    updatePickerInput(
      session,
      "db_lp_type",
      choices = db_lp_type_choices,
      selected = db_lp_type_choices
    )
  })
  
  db_lp_stats <- reactive({
    req(input$db_lp_type)
    lp <- landing_pairs()[Landing_Pair_Type %in% input$db_lp_type]
    lp$Landing_Runway <- ifelse(
      lp$Leader_Landing_Runway == lp$Follower_Landing_Runway,
      as.character(lp$Leader_Landing_Runway),
      paste0(lp$Leader_Landing_Runway, "-", lp$Follower_Landing_Runway)
    )
    lp$Wake_Vortex <- paste0(lp$Leader_Wake_Vortex, "-", lp$Follower_Wake_Vortex)
    
    lp_wake <- as.data.table(table(lp$Wake_Vortex))
    names(lp_wake) <- c("Wake", "Count")
    lp_wake$`Percentage (Numeric)` <- lp_wake$Count / sum(lp_wake$Count)
    lp_wake$`Percentage (String)` <- paste0(round(lp_wake$Count / sum(lp_wake$Count) * 100, 3), "%")
    
    lp_rwy <- as.data.table(table(lp$Landing_Runway))[order(V1)]
    names(lp_rwy) <- c("Runway", "Count")
    lp_rwy$`Percentage (Numeric)` <- lp_rwy$Count / sum(lp_rwy$Count)
    lp_rwy$`Percentage (String)` <- paste0(round(lp_rwy$Count / sum(lp_rwy$Count) * 100, 3), "%")
    
    lp_wakerwy <- as.data.table(table(lp$Wake_Vortex, lp$Landing_Runway))
    names(lp_wakerwy) <- c("Wake", "Runway", "Count")
    lp_wakerwy$Count <- paste0(lp_wakerwy$Count, " (", round(lp_wakerwy$Count / sum(lp_wakerwy$Count) * 100, 3), "%)")
    
    return(list(
      lp_wake = lp_wake,
      lp_rwy = lp_rwy,
      lp_wakerwy = tidyr::spread(lp_wakerwy, Runway, Count)
    ))
  })
  output$db_lp_wake_table <- DT::renderDataTable({
    datatable(
      db_lp_stats()[["lp_wake"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  output$db_lp_lrwy_table <- DT::renderDataTable({
    datatable(
      db_lp_stats()[["lp_rwy"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  output$db_lp_wakerwy_table <- DT::renderDataTable({
    datatable(
      db_lp_stats()[["lp_wakerwy"]],
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  
  # Adaptation Data Views
  
  db_aircraft_adaptation <- reactive({
    " EXEC usp_GI_Get_ORD_Aircraft_Adaptation_Data
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_aircraft_adaptation_table <- DT::renderDataTable({
    datatable(
      db_aircraft_adaptation(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  
  db_dbs_adaptation <- reactive({
    " EXEC usp_GI_Get_ORD_DBS_Adaptation_Data
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_dbs_adaptation_table <- DT::renderDataTable({
    datatable(
      db_dbs_adaptation(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  
  db_runway_adaptation <- reactive({
    " EXEC usp_GI_Get_ORD_Runway_Adaptation_Data 'CYYZ'
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_runway_adaptation_table <- DT::renderDataTable({
    datatable(
      db_runway_adaptation(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  
  db_wake_adaptation <- reactive({
    " EXEC usp_GI_Get_ORD_Wake_Adaptation_Data
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  output$db_wake_adaptation_table <- DT::renderDataTable({
    datatable(
      db_wake_adaptation(),
      rownames = F, selection = "none", extensions = c("Buttons"), style = "bootstrap4", options = list(pageLength = 15, lengthMenu = seq(5, 100, 5), columnDefs = list(list(className = 'dt-center', targets = "_all")), scrollX = T, dom = '<"dataTables_row"Bf><"dataTables_row"il>rtp', buttons = c('copy', 'csv', 'excel'))
    )
  }, server = F)
  
  # ----------------------------------------------------------------------- #
  # PLT Tab -----------------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  # Render PLT map tiles
  output$pltmap <- renderLeaflet({
    x <- leaflet(options = leafletOptions(zoomControl = F, preferCanvas = T)) %>%
      setView(lng = 0, lat = 0, zoom = 3)
    tile_providers <- list(
      `Esri Satellite` = "Esri.WorldImagery",
      `CartoDB Light` = "CartoDB.Positron",
      `CartoDB Light 2` = "CartoDB.PositronNoLabels",
      `CartoDB Dark` = "CartoDB.DarkMatter",
      `CartoDB Dark 2` = "CartoDB.DarkMatterNoLabels",
      `OSM Mapnik` = "OpenStreetMap.Mapnik"
    )
    for (i in 1:length(tile_providers)) {
      x <- x %>% addProviderTiles(providers[[tile_providers[[i]]]], options = providerTileOptions(noWrap = T), group = names(tile_providers)[i])
    }
    x <- x %>% addLayersControl(baseGroups = names(tile_providers), options = layersControlOptions(collapsed = T))
  })
  
  # Map centering
  map_centre <- reactive({
    " SELECT
      	Grid_Projection_Origin_Lat/PI()*180 AS Lat,
      	Grid_Projection_Origin_Lon/PI()*180 AS Lon
      FROM tbl_Adaptation_Data
    " %>% sqlQuery(con(),.) %>% as.data.table()
  })
  
  observeEvent(map_centre(), {
    leafletProxy("pltmap") %>% setView(lng = map_centre()$Lon, lat = map_centre()$Lat, zoom = 10)
  })
  
  observeEvent(input$pltmap_fpid, {
    time_range <- sprintf(
      " SELECT MIN(Track_Time) AS Min_Time, MAX(Track_Time) AS Max_Time FROM tbl_Radar_Track_Point
        LEFT JOIN (
          SELECT Radar_Track_Point_ID AS Radar_Track_Point_ID_2, Corrected_Mode_C, Range_To_Threshold, Range_To_ILS, Path_Leg
          FROM tbl_Radar_Track_Point_Derived
        ) AS t ON Radar_Track_Point_ID = Radar_Track_Point_ID_2
        WHERE Flight_Plan_ID IN ('%s')
      ",
      paste(input$pltmap_fpid, collapse = "','")
    ) %>%
      sqlQuery(con(),.) %>%
      as.data.table()
    updateSliderInput(
      session,
      "pltmap_time_range",
      min = time_range$Min_Time,
      max = time_range$Max_Time,
      value = c(time_range$Min_Time, time_range$Max_Time)
    )
  })
  
  # Subsetted tbl_Radar_Track_Point
  tracks_full <- eventReactive(input$pltmap_fpid, {
    sprintf(
      " SELECT * FROM tbl_Radar_Track_Point
        LEFT JOIN (
          SELECT Radar_Track_Point_ID AS Radar_Track_Point_ID_2, Corrected_Mode_C, Range_To_Threshold, Range_To_ILS, Path_Leg
          FROM tbl_Radar_Track_Point_Derived
        ) AS t ON Radar_Track_Point_ID = Radar_Track_Point_ID_2
        WHERE Flight_Plan_ID IN ('%s')
      ",
      paste(input$pltmap_fpid, collapse = "','")
    ) %>%
      sqlQuery(con(),.) %>%
      as.data.table()  %>%
      .[is.na(Path_Leg), Path_Leg := "NA"]
  })
  
  # Subsetted tbl_Radar_Track_Point
  tracks <- reactive({
    tracks_full()[Track_Time >= input$pltmap_time_range[1] & Track_Time <= input$pltmap_time_range[2]]
  })
  
  # PLT Map Top Left Dropdown & Screenshot Buttons
  output$pltmap_filters_ui <- renderUI({
    div(
      class = "pltmap-filters",
      dropdown(
        pickerInput("pltmap_fpdate", "Select Date", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        pickerInput("pltmap_fpid", "Select FP ID", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        pickerInput("pltmap_legs", "Filter By Leg", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        pickerInput("pltmap_volumes", "Display Volumes", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        pickerInput("pltmap_colour", "Colour Data", c("Path_Leg", "Mode_C", "Corrected_Mode_C"), selected="Path Leg", width="220px"),
        style = "minimal", icon = icon("filter"),
        tooltip = tooltipOptions(title = "Plotting Options", placement = "right")
      ),
      div(style = "height: 5px"),
      dropdown(
        div(style = "text-align: center", tags$b("Track Marker Settings")),
        sliderTextInput("pltmap_marker_radius", "Radius", choices=seq(1, 50, 1), selected=5, width="220px"),
        sliderTextInput("pltmap_marker_weight", "Weight", choices=seq(1, 50, 1), selected=5, width="220px"),
        sliderTextInput("pltmap_marker_opacity", "Opacity", choices=seq(0, 1, 0.01), selected=0.85, width="220px"),
        sliderTextInput("pltmap_marker_fillopacity", "Fill Opacity", choices=seq(0, 1, 0.01), selected=0.85, width="220px"),
        pickerInput("pltmap_marker_palette", "Colour Palette", rownames(brewer.pal.info), selected="Spectral", options = list(`live-search` = T), width="220px"),
        style = "minimal", icon = icon("bullseye"),
        tooltip = tooltipOptions(title = "Marker Settings", placement = "right")
      ),
      div(style = "height: 5px"),
      dropdown(
        div(style = "text-align: center", tags$b("Volume Polygon Settings")),
        div(style = "height: 5px;"),
        div(
          style = "display: flex; flex-direction: row; flex-wrap: wrap; justify-content: space-around; height: 320px; width: 460px",
          sliderTextInput("pltmap_volume_weight", "Weight", choices=seq(1, 50, 1), selected=5, width="220px"),
          sliderTextInput("pltmap_volume_highlightweight", "Highlight Weight", choices=seq(1, 50, 1), selected=3, width="220px"),
          sliderTextInput("pltmap_volume_opacity", "Opacity", choices=seq(0, 1, 0.01), selected=0.5, width="220px"),
          sliderTextInput("pltmap_volume_highlightopacity", "Highlight Opacity", choices=seq(0, 1, 0.01), selected=0.5, width="220px"),
          sliderTextInput("pltmap_volume_fillopacity", "Fill Opacity", choices=seq(0, 1, 0.01), selected=0.1, width="220px"),
          sliderTextInput("pltmap_volume_highlightfillopacity", "Highlight Fill Opacity", choices=seq(0, 1, 0.01), selected=0.5, width="220px"),
          sliderTextInput("pltmap_volume_dash", "Dash Size", choices=seq(1, 50, 1), selected=5, width="220px")
        ),
        div(
          style = "display: inline-flex; flex-direction: column; flex-wrap: wrap; justify-content: space-between; height: 80px; width: 460px",
          div(style = "padding-left: 23px;", tags$b("Colour")),
          div(
            style = "display: flex; justify-content: center; vertical-align: middle; margin-top: -18px;",
            div(style = "padding: 26px 3px 0 10px", "R"),
            textInput("pltmap_volume_colour_r", "", 255, width="46px"),
            div(style = "padding: 26px 3px 0 10px", "G"),
            textInput("pltmap_volume_colour_g", "", 0, width="46px"),
            div(style = "padding: 26px 3px 0 10px", "B"),
            textInput("pltmap_volume_colour_b", "", 0, width="46px")
          ),
          div(style = "padding-left: 23px;", tags$b("Highlight Colour")),
          div(
            style = "display: flex; justify-content: center; vertical-align: middle; margin-top: -18px;",
            div(style = "padding: 26px 3px 0 10px", "R"),
            textInput("pltmap_volume_highlightcolour_r", "", 128, width="46px"),
            div(style = "padding: 26px 3px 0 10px", "G"),
            textInput("pltmap_volume_highlightcolour_g", "", 0, width="46px"),
            div(style = "padding: 26px 3px 0 10px", "B"),
            textInput("pltmap_volume_highlightcolour_b", "", 0, width="46px")
          )
        ),
        style = "minimal", icon = icon("draw-polygon"),
        tooltip = tooltipOptions(title = "Polygon Settings", placement = "right")
      ),
      div(style = "height: 5px"),
      dropdown(
        div(
          style = "width: calc(100vw - 110px); height: 100%;",
          DT::dataTableOutput("plt_tracks")
        ),
        style = "minimal", icon = icon("route"),
        tooltip = tooltipOptions(title = "Plotted Tracks", placement = "right")
      ),
      div(style = "height: 5px"),
      actionBttn("pltmap_toggle_timefilter", NULL, style = "minimal", icon = icon("clock")),
      div(style = "height: 5px"),
      actionBttn("pltmap_clearmarker", NULL, style = "minimal", icon = icon("eraser")),
      div(style = "height: 5px"),
      downloadButton("pltmap_screenshot", NULL, class = "bttn-minimal")
    )
  })
  
  # Update Date Filter Choices
  observeEvent(flightplan(), {
    updatePickerInput(
      session,
      "pltmap_fpdate",
      choices = flightplan()$FP_Date %>% as.character() %>% unique() %>% .[order(as.Date(., format="%d/%m/%Y"))]
    )
  })
  
  # Update FPID Filter Choices
  observeEvent(input$pltmap_fpdate, {
    pltmap_fpid_choices <- flightplan()[FP_Date %in% input$pltmap_fpdate, c("Flight_Plan_ID", "Callsign")][order(Flight_Plan_ID)] %>% unique()
    updatePickerInput(
      session,
      "pltmap_fpid",
      choices = pltmap_fpid_choices$Flight_Plan_ID %>% as.character(),
      choicesOpt = list(subtext = pltmap_fpid_choices$Callsign %>% as.character())
    )
  })
  
  # Update Volume Filter Choices
  observeEvent(volumes(), {
    updatePickerInput(
      session,
      "pltmap_volumes",
      choices = volumes()$Volume_Name %>% as.character() %>% unique()
    )
  })
  
  # Update Path Leg Filter Choices
  observeEvent(legs(), {
    pltmap_legs_choices <- legs()$Path_Leg_Name %>% as.character() %>% c(., "NA")
    updatePickerInput(
      session,
      "pltmap_legs",
      choices = pltmap_legs_choices,
      selected = pltmap_legs_choices,
      choicesOpt = list(subtext = legs()$Path_Leg_Type %>% as.character())
    )
  })
  
  # PLT Map Track Point Labels
  pltmap_lab <- reactive({
    if (dim(tracks())[1] != 0 & dim(tracks())[2] != 0) {
      sprintf("
              <b>Callsign</b>: %s <font size='1'><b>FP ID</b> %s</font><br/>
              <b>Time</b>: %s %s <font size='1'><b>Point ID</b> %s</font><br/>
              <b>Mode C</b>: %s <b>Corrected</b>: %s<br/>
              <b>Path Leg</b>: %s<br/>
              <b>Range to ILS</b>: %s<br/>
              <b>Range to Threshold</b>: %s<br/>
              ",
              tracks()$Callsign, tracks()$Flight_Plan_ID,
              tracks()$Track_Date, tracks()$Track_Time, tracks()$Radar_Track_Point_ID,
              tracks()$Mode_C, tracks()$Corrected_Mode_C,
              tracks()$Path_Leg,
              tracks()$Range_To_ILS,
              tracks()$Range_To_Threshold
      ) %>% lapply(htmltools::HTML)
    } else {
      NULL
    }
  })
  
  # Update PLT Map Volume Colour Selection (Red)
  observeEvent(input$pltmap_volume_colour_r, {
    x <- input$pltmap_volume_colour_r %>% as.numeric() %>% ifelse(is.na(.), -1, .)
    if (x > 255 | x < 0) updateTextInput(session, "pltmap_volume_colour_r", value=0)
  })
  
  # Update PLT Map Volume Colour Selection (Green)
  observeEvent(input$pltmap_volume_colour_g, {
    x <- input$pltmap_volume_colour_g %>% as.numeric() %>% ifelse(is.na(.), -1, .)
    if (x > 255 | x < 0) updateTextInput(session, "pltmap_volume_colour_g", value=0)
  })
  
  # Update PLT Map Volume Colour Selection (Blue)
  observeEvent(input$pltmap_volume_colour_b, {
    x <- input$pltmap_volume_colour_b %>% as.numeric() %>% ifelse(is.na(.), -1, .)
    if (x > 255 | x < 0) updateTextInput(session, "pltmap_volume_colour_b", value=0)
  })
  
  # Update PLT Map Volume Highlight Colour Selection (Red)
  observeEvent(input$pltmap_volume_highlightcolour_r, {
    x <- input$pltmap_volume_highlightcolour_r %>% as.numeric() %>% ifelse(is.na(.), -1, .)
    if (x > 255 | x < 0) updateTextInput(session, "pltmap_volume_highlightcolour_r", value=0)
  })
  
  # Update PLT Map Volume Highlight Colour Selection (Green)
  observeEvent(input$pltmap_volume_highlightcolour_g, {
    x <- input$pltmap_volume_highlightcolour_g %>% as.numeric() %>% ifelse(is.na(.), -1, .)
    if (x > 255 | x < 0) updateTextInput(session, "pltmap_volume_highlightcolour_g", value=0)
  })
  
  # Update PLT Map Volume Highlight Colour Selection (Blue)
  observeEvent(input$pltmap_volume_highlightcolour_b, {
    x <- input$pltmap_volume_highlightcolour_b %>% as.numeric() %>% ifelse(is.na(.), -1, .)
    if (x > 255 | x < 0) updateTextInput(session, "pltmap_volume_highlightcolour_b", value=0)
  })
  
  # Get PLT Map Volume Colour Based on RGB Selection
  pltmap_volume_colour <- reactive({
    r <- input$pltmap_volume_colour_r
    g <- input$pltmap_volume_colour_g
    b <- input$pltmap_volume_colour_b
    return(paste0("rgb(",r,",",g,",",b,")"))
  })
  
  # Get PLT Map Volume Highlight Colour Based on RGB Selection
  pltmap_volume_highlightcolour <- reactive({
    r <- input$pltmap_volume_highlightcolour_r
    g <- input$pltmap_volume_highlightcolour_g
    b <- input$pltmap_volume_highlightcolour_b
    return(paste0("rgb(",r,",",g,",",b,")"))
  })
  
  # Placeholder for additional PLT map elements displayed (for screenshotting)
  update_pltmap <- reactiveValues(moved = F, markers = NULL, volumes = NULL)
  
  # Plot tracks
  observe({
    if (any(is.na(input$pltmap_legs))) {
      d <- tracks()[Path_Leg %in% input$pltmap_legs | is.na(Path_Leg)]
    } else {
      d <- tracks()[Path_Leg %in% input$pltmap_legs]
    }
    p <- leafletProxy("pltmap", data=d) %>% clearGroup("Tracks") %>% removeControl("Legend")
    pal <- if (input$pltmap_colour == "Path_Leg") {
      colorFactor(brewer.pal(11, input$pltmap_marker_palette), domain=legs()$Path_Leg_Name)
    } else {
      colorNumeric(brewer.pal(11, input$pltmap_marker_palette), domain=d[[input$pltmap_colour]])
    }
    p %>% addCircleMarkers(
      lng = ~Lon*180/pi,
      lat = ~Lat*180/pi,
      color = ~pal(eval(parse(text=input$pltmap_colour))),
      label=pltmap_lab(),
      labelOptions=labelOptions(textsize="13px", direction="auto"),
      weight=input$pltmap_marker_weight,
      radius=input$pltmap_marker_radius,
      stroke=T,
      opacity=input$pltmap_marker_opacity,
      fillOpacity=input$pltmap_marker_fillopacity,
      group="Tracks"
    ) %>%
      addLegend(
        position = "bottomleft",
        title = input$pltmap_colour,
        pal = pal,
        values = ~eval(parse(text=input$pltmap_colour)),
        opacity = 0.85,
        layerId = "Legend"
      )
    update_pltmap$markers <- d
  })
  
  # Plot volumes
  observe({
    p <- leafletProxy("pltmap") %>% clearGroup("Volumes")
    # pal <- colorFactor(brewer.pal(11, "Spectral"), domain=volumes()$Volume_Name)
    for (i in input$pltmap_volumes) {
      p %>% addPolygons(
        data = Polygon(
          volumes()[Volume_Name %in% i, c("Longitude","Latitude")][,':='(Latitude=Latitude*180/pi, Longitude=Longitude*180/pi)]
        ),
        weight = input$pltmap_volume_weight,
        opacity = input$pltmap_volume_opacity,
        fillOpacity = input$pltmap_volume_fillopacity,
        color = pltmap_volume_colour(),
        dashArray = paste0(input$pltmap_volume_dash),
        label = i,
        labelOptions = labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
        highlight = highlightOptions(
          weight = input$pltmap_volume_highlightweight,
          color = pltmap_volume_highlightcolour(),
          dashArray = "",
          opacity = input$pltmap_volume_highlightopacity,
          fillOpacity = input$pltmap_volume_highlightfillopacity,
          bringToFront = F
        ),
        group = "Volumes"
      )
    }
    update_pltmap$volumes <- volumes()[Volume_Name %in% input$pltmap_volumes]
  })
  
  # Map screenshot functionality
  output$pltmap_screenshot <- downloadHandler(
    filename = function() {
      paste0("PLT_Map_", gsub(" ", "_", gsub("-|:", "", as.character(Sys.time()))),".png")
    },
    content = function(file) {
      
      p <- leaflet(options = leafletOptions(zoomControl = F, preferCanvas = T))
      
      if (input$pltmap_groups == "Satellite") {
        p <- p %>% addProviderTiles(providers$Esri.WorldImagery, options=providerTileOptions(noWrap=TRUE))
      } else if (input$pltmap_groups == "Grey") {
        p <- p %>% addProviderTiles(providers$CartoDB.Positron, options=providerTileOptions(noWrap=TRUE))
      } else if (input$pltmap_groups == "Dark") {
        p <- p %>% addProviderTiles(providers$CartoDB.DarkMatter, options=providerTileOptions(noWrap=TRUE))
      } else if (input$pltmap_groups == "Light") {
        p <- p %>% addProviderTiles(providers$Esri.WorldTopoMap, options=providerTileOptions(noWrap=TRUE))
      } else if (input$pltmap_groups == "Topo") {
        p <- p %>% addProviderTiles(providers$Esri.DeLorme, options=providerTileOptions(noWrap=TRUE))
      } else if (input$pltmap_groups == "OSM") {
        p <- p %>% addProviderTiles(providers$OpenStreetMap.Mapnik, options=providerTileOptions(noWrap=TRUE))
      } else if (input$pltmap_groups == "OSM B&W") {
        p <- p %>% addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options=providerTileOptions(noWrap=TRUE))
      }
      
      p <- p %>% setView(lng = input$pltmap_center$lng, lat = input$pltmap_center$lat, zoom = input$pltmap_zoom)
      
      if (!is.null(update_pltmap$markers)) {
        pal <- colorFactor(brewer.pal(11, input$pltmap_marker_palette), domain=legs()$Path_Leg_Name)
        p <- p %>%
          clearGroup("Tracks") %>%
          addCircleMarkers(
            data = update_pltmap$markers,
            lng = ~Lon*180/pi,
            lat = ~Lat*180/pi,
            color = ~pal(Path_Leg),
            weight=input$pltmap_marker_weight,
            radius=input$pltmap_marker_radius,
            stroke=T,
            opacity=input$pltmap_marker_opacity,
            fillOpacity=input$pltmap_marker_fillopacity,
            group="Tracks"
          )
      }
      
      if (!is.null(update_pltmap$volumes)) {
        p <- p %>% clearGroup("Volumes")
        for (i in unique(update_pltmap$volumes$Volume_Name)) {
          p <- p %>% addPolygons(
            data = Polygon(
              update_pltmap$volumes[Volume_Name %in% i, c("Longitude","Latitude")][,':='(Latitude=Latitude*180/pi, Longitude=Longitude*180/pi)]
            ),
            weight = input$pltmap_volume_weight,
            opacity = input$pltmap_volume_opacity,
            fillOpacity = input$pltmap_volume_fillopacity,
            color = pltmap_volume_colour(),
            dashArray = paste0(input$pltmap_volume_dash),
            group = "Volumes"
          )
        }
      }
      
      mapshot(p, file = file, vwidth = input$pltDim[1], vheight = input$pltDim[2], delay = 0)
      
    },
    contentType = "image/png"
  )
  
  # Render subsetted tracks table
  output$plt_tracks <- DT::renderDataTable({
    datatable(
      tracks(),
      rownames = F,
      selection = "none",
      options = list(
        pageLength = 15,
        lengthMenu = seq(5, 15, 5),
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        scrollX = T
      )
    )
  })
  
  # Time range filter
  
  output$pltmap_time_range_ui <- renderUI({
    div(
      class = "centered",
      style = "
        position: relative;
        bottom: 85px;
        z-index: 1000;
        height: 0
      ",
      sliderInput(
        "pltmap_time_range",
        NULL,
        min = NA,
        max = NA,
        value = c(NA, NA),
        step = 1,
        round = T,
        animate = animationOptions(interval = 100, loop = T),
        dragRange = T,
        width = "90%"
      )
    )
  })

  onclick("pltmap_toggle_timefilter", toggle("pltmap_time_range_ui"))
  
  observeEvent(input$pltmap_click, {
    click <- input$pltmap_click
    clat <- click$lat
    clon <- click$lng
    write_clip(paste(clat, clon))
    lab <- sprintf("<b>Latitude</b>: %s<br/><b>Longitude</b>: %s", clat, clon) %>% lapply(htmltools::HTML)
    leafletProxy("pltmap") %>%
      addMarkers(
        clon,
        clat,
        label = lab,
        labelOptions = labelOptions(textsize="13px", direction="auto"),
        options = markerOptions(opacity = 0.7),
        group = "Clicked"
      )
  })
  
  observeEvent(input$pltmap_clearmarker, {
    leafletProxy("pltmap") %>% clearGroup("Clicked")
  })
  
  # ----------------------------------------------------------------------- #
  # ORD Calibration Viewer --------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  ord_dates <- eventReactive(con(), {
    "   SET DATEFORMAT dmy
        SELECT DISTINCT FP_Date, CAST(FP_Date AS datetime) AS Date FROM vw_ORD_Calibration_View
        ORDER BY CAST(FP_Date AS datetime)
    " %>% sqlQuery(con(),.) %>% .$FP_Date %>% unlist() %>% as.vector()
  })
  
  ord_actypes <- eventReactive(con(), {
    "   SELECT DISTINCT Follower_Aircraft_Type FROM vw_ORD_Calibration_View
        ORDER BY Follower_Aircraft_Type
    " %>% sqlQuery(con(),.) %>% unlist() %>% as.vector()
  })
  
  ord_wakes <- eventReactive(con(), {
    "   SELECT DISTINCT Follower_RECAT_Wake_Turbulence_Category FROM vw_ORD_Calibration_View
        ORDER BY Follower_RECAT_Wake_Turbulence_Category
    " %>% sqlQuery(con(),.) %>% unlist() %>% as.vector()
  })
  
  output$tab_ord_ui_1 <- renderUI({
    box(
      title = "1. Search for Flights",
      width = NULL,
      solidHeader = T,
      div(
        style = "display: flex; justify-content: space-around; flex-wrap: wrap;",
        pickerInput("ord_date", "Date in:", ord_dates(), multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        div(style = "padding-top: 35px", "AND"),
        pickerInput("ord_actype", "Aircraft Type in:", ord_actypes(), multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
        div(style = "padding-top: 35px", "AND"),
        pickerInput("ord_wake", "Wake in:", ord_wakes(), multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px")
      ),
      div(
        style = "text-align: center",
        actionButton("ord_search", "Query")
      )
    )
  })
  
  ord_queried <- eventReactive(input$ord_search, {
    sprintf(
      "   SELECT * FROM vw_ORD_Calibration_View
          WHERE FP_Date IN ('%s')
          AND Follower_Aircraft_Type IN ('%s')
          AND Follower_RECAT_Wake_Turbulence_Category IN ('%s')
      ",
      paste(as.character(input$ord_date), collapse = "','"),
      paste(as.character(input$ord_actype), collapse = "','"),
      paste(as.character(input$ord_wake), collapse = "','")
    ) %>% sqlQuery(con(),.) %>% as.data.table()
  })
  
  observeEvent(ord_queried(), {
    
    output$tab_ord_ui_2 <- renderUI({
      
      ord_callsign_choices <- unique(ord_queried()[, c("Follower_Flight_Plan_ID", "Follower_Callsign")])
      
      ord_headwind_range <- c(
        floor(min(ord_queried()$Follower_Threshold_Surface_Headwind, na.rm = T)),
        ceiling(max(ord_queried()$Follower_Threshold_Surface_Headwind, na.rm = T))
      )
      
      box(
        title = "2. Filter Flights",
        width = NULL,
        solidHeader = T,
        div(
          style = "display: flex; justify-content: space-around; flex-wrap: wrap;",
          pickerInput(
            "ord_callsign",
            "FPID/Callsign",
            choices = ord_callsign_choices$Follower_Flight_Plan_ID %>% as.character(),
            choicesOpt = list(subtext = ord_callsign_choices$Follower_Callsign %>% as.character()),
            selected = ord_callsign_choices$Follower_Flight_Plan_ID %>% as.character(),
            multiple = T,
            options = list(`actions-box` = T, `live-search` = T),
            width = "220px"
          )
        ),          
        div(
          style = "display: flex; justify-content: space-around; flex-wrap: wrap;",
          sliderInput(
            "ord_surfaceheadwind",
            "Surface Headwind (kts)",
            min = ord_headwind_range[1],
            max = ord_headwind_range[2],
            value = ord_headwind_range,
            step = 0.1,
            dragRange = T,
            width = "450px"
          )
        ),
        div(
          style = "display: flex; justify-content: space-around; flex-wrap: wrap;",
          materialSwitch("ord_remove_stationary", "Remove Stationary Points", value = T, status = "danger"),
          materialSwitch("ord_remove_invalid_RTT", "Remove flights with bad RTT", value = T, status = "danger")
        ),
        div(
          style = "text-align: center",
          actionButton("ord_filter", "Filter")
        )
      )
      
    })
    
  })
  
  observeEvent(input$ord_callsign, {
    ord_surfaceheadwind_choices <- range(ord_queried()[Follower_Flight_Plan_ID %in% input$ord_callsign]$Follower_Threshold_Surface_Headwind, na.rm = T)
    updateSliderInput(
      session,
      "ord_surfaceheadwind",
      min = ord_surfaceheadwind_choices[1],
      max = ord_surfaceheadwind_choices[2],
      value = ord_surfaceheadwind_choices
    )
  })
  
  ord_filtered <- eventReactive(input$ord_filter, {
    x <- ord_queried()[Follower_Flight_Plan_ID %in% input$ord_callsign]
    invalid_RTT <- ddply(
      x, "Follower_Flight_Plan_ID", summarise,
      MinRTT = min(Follower_Range_To_Threshold, na.rm = T),
      MaxRTT = max(Follower_Range_To_Threshold, na.rm = T),
      Surface_Headwind = min(Follower_Threshold_Surface_Headwind, na.rm = T)
    ) %>% 
      as.data.table() %>%
      .[Surface_Headwind >= input$ord_surfaceheadwind[1] & Surface_Headwind <= input$ord_surfaceheadwind[2]]
    if (input$ord_remove_invalid_RTT) {
      invalid_RTT <- invalid_RTT[MinRTT >= 1 | MaxRTT < 4]
    }
    x <- x[Follower_Flight_Plan_ID %!in% invalid_RTT$Follower_Flight_Plan_ID]
    if (input$ord_remove_stationary) {
      k <- 2
      while (k <= nrow(x)) {
        same_range <- x$Follower_Range_To_Threshold[k] == x$Follower_Range_To_Threshold[k-1]
        same_x <- x$X_Position[k] == x$X_Position[k-1]
        same_y <- x$Y_Position[k] == x$Y_Position[k-1]
        if (same_range | (same_x & same_y)) {
          x <- x[-k]
        } else {
          k <- k + 1
        }
      }
    }
    return(x)
  })
  
  observeEvent(input$ord_filter, {
    
    output$tab_ord_ui_3 <- renderUI({
      box(
        title = "3. Calibration Settings",
        width = NULL,
        solidHeader = T,
        DT::dataTableOutput("ord_queried_output"),
        div(style = "height: 5px"),
        div(
          style = "display: flex; justify-content: space-around; flex-wrap: wrap;",
          radioGroupButtons(
            "ord_speedtype",
            "Speed Type",
            choices = c("Track Speed", "Mode S GSPD", "Mode S IAS", "Mode S TAS", "Derived GSPD"),
            selected = "Derived GSPD"
          )
        ),
        div(
          style = "text-align: center",
          actionButton("ord_run", "Run Calibration")
        )
      )
    })
    
  })
  
  output$ord_queried_output <- DT::renderDataTable({
    datatable(
      ord_filtered(),
      rownames = F,
      selection = "none",
      options = list(
        pageLength = 5,
        lengthMenu = seq(5, 100, 5),
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        scrollX = T
      )
    )
  })
  
  observeEvent(input$ord_run, {
    
    output$tab_ord_ui_4 <- renderUI({
      box(
        title = "4. Calibration Output",
        width = NULL,
        solidHeader = T,
        plotlyOutput("ord_speedprofile_plot"),
        div(style = "height: 5px;"),
        verbatimTextOutput("ord_nls_print")
      )
    })
    
  })
  
  ord_nls <- eventReactive(input$ord_run, {
    
    if (nrow(ord_filtered()) == 0) return(list(x = NA, y = NA, m = NA))
    
    if (input$ord_speedtype == "Derived GSPD") {
      
      # Calculate speed based on positional difference between timestamps
      tracks_new <- ord_filtered()[,c("Track_Time", "Follower_Threshold_Surface_Headwind", "Follower_Range_To_Threshold", "X_Position", "Y_Position", "Altitude", "Track_Speed")]
      tracks_new$Distance_Travelled <- NA
      for (k in 2:nrow(tracks_new)) {
        tracks_new$Distance_Travelled[k] <- sqrt((tracks_new$X_Position[k] - tracks_new$X_Position[k-1])^2 + (tracks_new$Y_Position[k] - tracks_new$Y_Position[k-1])^2 + ((tracks_new$Altitude[k] - tracks_new$Altitude[k-1])/6076.12)^2)
        if (k == 2) {
          tracks_new$Distance_Travelled[k-1] <- tracks_new$Distance_Travelled[k]
        }
      }
      tracks_new$Point_Speed <- NA
      tracks_new$Point_Speed[1] <- tracks_new$Track_Speed[1]
      for (k in 2:nrow(tracks_new)) {
        tracks_new$Point_Speed[k] <- tracks_new$Distance_Travelled[k]/((tracks_new$Track_Time[k]-tracks_new$Track_Time[k-1])/3600)
      }
      
      # Filter strange speeds (20% above or below Track_Speed range)
      tracks_new <- tracks_new[Point_Speed >= max(min(tracks_new$Track_Speed, na.rm=T)*0.8, 50, na.rm=T) & Point_Speed <= max(tracks_new$Track_Speed, na.rm=T)*1.2]
      
      # Get tracks x and y vectors
      x <- tracks_new$Follower_Range_To_Threshold
      y <- (tracks_new$Point_Speed+tracks_new$Follower_Threshold_Surface_Headwind)/(1+550/60000)
      
    } else {
      
      x <- ord_filtered()[["Follower_Range_To_Threshold"]]
      y <- ord_filtered()[[gsub(" ", "_", input$ord_speedtype)]]
      
    }
    
    if (length(x) < length(y)) {
      y <- y[1:length(x)]
    } else if (length(x) > length(y)) {
      x <- x[1:length(y)]
    }
    
    # Generate NLS model
    m <- tryCatch(
      suppressWarnings(nls(
        y ~ airspeed_model_vector_break(x, a, a1, b, n1, n2),
        start = list(a = 140, a1 = 140, b = 160, n1 = 3, n2 = 4),
        control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
      )),
      error = function(e) NULL
    )
    
    # Generate Simplified NLS model if first one fails
    if (is.null(m)) {
      x <- x %>% .[. <= 2]
      y <- y[1:length(x)]
      m <- tryCatch(
        suppressWarnings(nls(
          y ~ airspeed_model_vector_break_simplified(x, a, a1),
          start = list(a = 140, a1 = 140),
          control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
        )),
        error = function(e) NULL
      )
    }
    
    # Generate Even More Simplified (TM) NLS model if previous ones fail
    if (is.null(m)) {
      m <- tryCatch(
        suppressWarnings(nls(
          y ~ a,
          start = list(a = 140),
          control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
        )),
        error = function(e) NULL
      )
    }
    
    return(list(x = x, y = y, m = m))
    
  })
  
  observeEvent(ord_nls(), {
    
    output$ord_speedprofile_plot <- renderPlotly({
      p <- plot_ly() %>%
        add_markers(
          x = ord_nls()$x,
          y = ord_nls()$y,
          name = "Raw Data",
          marker = list(color = "rgb(128,34,69)")
        ) %>%
        add_lines(
          x = ord_nls()$x,
          y = ord_nls()$m$m$fitted(),
          name = "Fitted Profile",
          line = list(color = "rgb(213,16,103)")
        ) %>%
        layout(
          hovermode = "compare",
          xaxis = list(title = "Follower Range to Threshold (NM)"),
          yaxis = list(title = "Speed (kts)")
        ) %>%
        config(
          displaylogo = F
        )
      ggplotly(p, width = session$clientData$output_ord_speedprofile_plot_width) # Width fix
    })
    
    output$ord_nls_print <- renderPrint({
      print(ord_nls())
    })
    
  })
  
  # ----------------------------------------------------------------------- #
  # Landing Pair Viewer -----------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  # db_lp_ids <- reactive({
  #   " SELECT DISTINCT Landing_Pair_ID
  #     FROM vw_All_Pair_Reference_Data
  #     ORDER BY Landing_Pair_ID
  #   " %>% sqlQuery(con(),.) %>% unlist() %>% as.vector()
  # })
  # 
  # output$tab_ord_ui_a <- renderUI({
  #   box(
  #     width = NULL,
  #     solidHeader = T,
  #     div(
  #       style = "display: flex; justify-content: space-around; flex-wrap: wrap;",
  #       textAreaInput(
  #         "lp_select",
  #         NULL,
  #         placeholder = "Landing Pair ID",
  #         width = "100%",
  #         height = "38px",
  #         resize = "none"
  #       )
  #     ),
  #     div(
  #       style = "text-align: center",
  #       actionButton("lp_search", "Search")
  #     )
  #   )
  # })
  # 
  # db_lp_lead_tracks <- eventReactive(input$lp_search, {
  #   " SELECT DISTINCT Landing_Pair_ID
  #     FROM vw_All_Pair_Reference_Data
  #     ORDER BY Landing_Pair_ID
  #   " %>% sqlQuery(con(),.) %>% unlist() %>% as.vector()
  # })
  
}
