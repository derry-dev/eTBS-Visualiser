server <- function(input, output, session) {

  # ----------------------------------------------------------------------- #
  # Database Connection -----------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  con <- eventReactive(input$db_connect, {
    valueExpr = get_db_connection(input$db_driver, input$db_server, input$db_database, input$db_username, input$db_password)
  })
  
  observeEvent(con(), {
    output$db_status <- renderUI({
      if (con() != -1L) {
        div(style="margin: 7px 0 0 6px;", "Connected", icon("check-circle"))
      } else {
        div(style="margin: 7px 0 0 6px;", "Error", icon("times-circle"))
      }
    })
  })
  
  # ----------------------------------------------------------------------- #
  # Query Tool --------------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  query <- eventReactive(input$db_execute, {
    if (con() != -1L) input$db_query %>% sqlQuery(con(),.) %>% as.data.table()
  })
  
  output$db_output <- DT::renderDataTable({
    datatable(
      query(),
      rownames = F,
      options = list(
        pageLength = 15,
        lengthMenu = seq(5, 100, 5),
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        scrollX = T
      )
    )
  })
  
  observeEvent(input$db_clear, {
    updateTextAreaInput(session, "db_query", value="")
  })
  
  # ----------------------------------------------------------------------- #
  # Database List -----------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  db_list <- eventReactive(con(), {
    if (con() != -1L) query_table_list %>% sqlQuery(con(),.) %>% as.data.table()
  })
  
  output$db_databases <- DT::renderDataTable({
    datatable(
      db_list(),
      rownames = F,
      options = list(
        pageLength = 15,
        lengthMenu = seq(5, 100, 5),
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        scrollX = T
      )
    )
  })
  
  # ----------------------------------------------------------------------- #
  # Flight Plan -------------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  flightplan <- eventReactive(con(), {
    if (con() != -1L) query_flightplan %>% sqlQuery(con(),.) %>% as.data.table()
  })
  
  output$db_flightplans <- DT::renderDataTable({
    datatable(
      flightplan(),
      rownames = F,
      options = list(
        pageLength = 15,
        lengthMenu = seq(5, 100, 5),
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        scrollX = T
      )
    )
  })
  
  observeEvent(flightplan(), {
    updatePickerInput(
      session,
      "pltmap_fpdate",
      choices = flightplan()$FP_Date %>% as.character() %>% unique() %>% .[order(as.Date(., format="%d/%m/%Y"))]
    )
  })
  
  observeEvent(input$pltmap_fpdate, {
    pltmap_fpid_choices <- flightplan()[FP_Date %in% input$pltmap_fpdate]$Flight_Plan_ID %>% as.character() %>% unique() %>% sort()
    updatePickerInput(
      session,
      "pltmap_fpid",
      choices = pltmap_fpid_choices,
      choicesOpt = list(subtext = flightplan()[Flight_Plan_ID %in% pltmap_fpid_choices]$Callsign %>% as.character())
    )
  })
  
  # ----------------------------------------------------------------------- #
  # Volumes -----------------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  volumes <- eventReactive(con(), {
    if (con() != -1L) {
      query_volumes %>% sqlQuery(con(),.) %>% as.data.table() %>%
        ifelse(c("Longitude", "Latitude") %in% names(.) %>% all(), .[,':='(Latitude=Latitude*180/pi, Longitude=Longitude*180/pi)], .)
    }
  })
  
  output$db_volumes <- DT::renderDataTable({
    datatable(
      volumes(),
      rownames = F,
      options = list(
        pageLength = 15,
        lengthMenu = seq(5, 100, 5),
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        scrollX = T
      )
    )
  })
  
  observeEvent(volumes(), {
    updatePickerInput(
      session,
      "pltmap_volumes",
      choices = volumes()$Volume_Name %>% as.character() %>% unique()
    )
  })
  
  # ----------------------------------------------------------------------- #
  # Legs --------------------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  legs <- eventReactive(con(), {
    if (con() != -1L) query_legs %>% sqlQuery(con(),.) %>% as.data.table()
  })
  
  output$db_legs <- DT::renderDataTable({
    datatable(
      legs(),
      rownames = F,
      options = list(
        pageLength = 15,
        lengthMenu = seq(5, 100, 5),
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        scrollX = T
      )
    )
  })
  
  # observeEvent(legs(), {
  #   pltmap_legs_choices <- legs()$Path_Leg_Name %>% as.character()
  #   updatePickerInput(
  #     session,
  #     "pltmap_legs",
  #     choices = pltmap_legs_choices,
  #     selected = pltmap_legs_choices,
  #     choicesOpt = list(subtext = legs()$Path_Leg_Type %>% as.character())
  #   )
  # })
  
  # ----------------------------------------------------------------------- #
  # Tracks ------------------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  tracks <- eventReactive(input$pltmap_update, {
    if (con() != -1L) {
      sprintf(
        "%s WHERE Flight_Plan_ID IN (%s)",
        query_tracks, paste(input$pltmap_fpid, collapse = ",")
      ) %>%
        sqlQuery(con(),.) %>%
        as.data.table()
    }
  })
  
  
  observeEvent(tracks(), {
    output$plt_tracks_button <- renderUI({
      dropdown(
        DT::dataTableOutput("plt_tracks"),
        style = "simple",
        icon = icon("table"),
        width = "600px",
        tooltip = tooltipOptions(title = "View Track Table", placement = "bottom")
      )
    })
  })
  
  output$plt_tracks <- DT::renderDataTable({
    datatable(
      tracks(),
      rownames = F,
      options = list(
        pageLength = 15,
        lengthMenu = seq(5, 15, 5),
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        scrollX = T
      )
    )
  })
  
  # ----------------------------------------------------------------------- #
  # PLT Map -----------------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  map_position <- reactiveValues(default = 0, current = 0)
  
  output$pltmap <- renderLeaflet({
    map_position$default <- leaflet(options = leafletOptions(zoomControl = F, preferCanvas = T)) %>% 
      setView(lng = -0.45, lat = 51.46, zoom = 10) %>%
      addProviderTiles(providers$Esri.WorldImagery, options=providerTileOptions(noWrap=TRUE), group="Satellite") %>%
      addProviderTiles(providers$CartoDB.Positron, options=providerTileOptions(noWrap=TRUE), group="Grey") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, options=providerTileOptions(noWrap=TRUE), group="Dark") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, options=providerTileOptions(noWrap=TRUE), group="Light") %>%
      addProviderTiles(providers$Esri.DeLorme, options=providerTileOptions(noWrap=TRUE), group="Topo") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, options=providerTileOptions(noWrap=TRUE), group="OSM") %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options=providerTileOptions(noWrap=TRUE), group="OSM B&W") %>%
      addLayersControl(baseGroups=c("Satellite","Grey","Dark","Light","Topo","OSM","OSM B&W"), options=layersControlOptions(collapsed=T))
  })
  
  observeEvent({
    input$pltmap_zoom
    input$pltmap_center
  },{
    map_position$current <- map_position$default %>%
      setView(lng = input$pltmap_center$lng,
              lat = input$pltmap_center$lat,
              zoom = input$pltmap_zoom)
  })
  
  output$pltmap_screenshot <- downloadHandler(
    filename = function() {
      timestamp <- Sys.time() %>% as.character() %>% gsub("-|:", "", .) %>% gsub(" ", "_", .)
      name <- paste0("PLT_Map_", timestamp,".png")
      return(name)
    },
    content = function(file) {
      mapshot(map_position$current, file = file, vwidth = input$pltDim[1], vheight = input$pltDim[2])
    }
  )
  
  # ----------------------------------------------------------------------- #
  # PLT Map Plotting --------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
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
  
  observeEvent(tracks(), {
    p <- leafletProxy("pltmap", data=tracks()) %>% clearGroup("Tracks")
    pal <- colorFactor(brewer.pal(11, "Spectral"), domain=legs()$Path_Leg_Name)
    p %>% addCircleMarkers(
      lng = ~Lon*180/pi,
      lat = ~Lat*180/pi,
      color = ~pal(Path_Leg),
      label=pltmap_lab(), labelOptions=labelOptions(textsize="13px", direction="auto"),
      weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Tracks"
    )
  })
  
  observe({
    p <- leafletProxy("pltmap") %>% clearGroup("Volumes")
    pal <- colorFactor(brewer.pal(11, "Spectral"), domain=volumes()$Volume_Name)
    for (i in volumes()$Volume_Name %>% unique()) {
      p %>% addPolygons(
        data = Polygon(volumes()[Volume_Name %in% i, c("Longitude","Latitude")]),
        weight = 5,
        opacity = 0.5,
        fillOpacity = 0.1,
        color = "Red",
        dashArray = "18",
        label = i,
        labelOptions = labelOptions(style = list("font-weight" = "bold"),opacity = 1, textsize="12px", direction = "auto"),
        highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.5, bringToFront = F),
        group = "Volumes"
      )
    }
  })
  
}
