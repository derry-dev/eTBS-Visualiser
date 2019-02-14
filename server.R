dialog_import <- function(stage) {
  if (stage == "Connect") {
    md <- modalDialog(
      title = div(style = "text-align: center;", "Connect to Database"),
      div(style = "margin-top: -10px;", textInput("db_driver", "Driver Name", "SQL Server Native Client 11.0")),
      div(style = "padding-top: 5px;", textInput("db_server", "Server Name", "DESKTOP-F25RUHL")),
      div(style = "padding-top: 5px;", textInput("db_database", "Database Name", "UTMA_Validation")),
      div(style = "position: relative;", htmlOutput("db_warning")),
      footer = div(
        style = "text-align: center;",
        tagList(
          actionButton("db_connect", "Connect"),
          modalButton("Cancel")
        )
      ),
      size = c("s")
    )
  }
  return(showModal(md))
}

server <- function(input, output, session) {
  
  observeEvent(input$import, {
    dialog_import("Connect")
  })
  
  modal_state <- reactiveVal(0)
  
  observeEvent(input$db_connect, {
    con <- get_db_connection(input$db_driver, input$db_server, input$db_database)
    if (con == -1) {
      output$db_warning <- renderText("<div style='color:red; text-align:center;'>ODBC connection failed</div>")
    } else {
      import_all(con)
      removeModal()
    }
  })
  
  observeEvent({
    input$db_driver
    input$db_server
    input$db_database
  },{
    output$db_warning <- renderText("")
  })
  
  output$plt_callsign_ui <- renderUI({
    plt_callsign_choices <- dat$tracks %>% subset(Track_Date %in% input$plt_date & Path_Leg %in% input$plt_leg) %>% .$Callsign %>% unique() %>% as.vector()
    pickerInput("plt_callsign", "Callsign", choices = plt_callsign_choices, multiple = T, options = pickerOptions(actionsBox = T))
  })
  
  output$plt_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = F)) %>% 
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
  
  d <- reactive({
    subset(dat$tracks, Track_Date %in% input$plt_date & Callsign %in% input$plt_callsign)
  })
  
  lab <- reactive({
    if (dim(d())[1] != 0 & dim(d())[2] != 0) {
      sprintf("
              <b>Callsign</b>: %s <font size='1'><b>FP ID</b> %s</font><br/>
              <b>Time</b>: %s %s <font size='1'><b>Point ID</b> %s</font><br/>
              <b>Path Leg</b>: %s<br/>
              <b>Range to ILS</b>: %s<br/>
              <b>Range to Threshold</b>: %s<br/>
              ",
              d()$Callsign, d()$Flight_Plan_ID,
              d()$Track_Date, d()$Track_Time_New, d()$Radar_Track_Point_ID,
              d()$Path_Leg,
              d()$Range_To_ILS,
              d()$Range_To_Threshold
      ) %>% lapply(htmltools::HTML)
    } else {
      NULL
    }
  })
  
  observe({
    p <- leafletProxy("plt_map", data=d()) %>% clearGroup("Tracks")
    pal1 <- colorFactor(brewer.pal(11, "Spectral"), domain=dat$tracks$Path_Leg)
    p %>% addCircleMarkers(
      lng = ~as.numeric(Lon)*180/pi,
      lat = ~as.numeric(Lat)*180/pi,
      color = ~pal1(Path_Leg),
      label=lab(), labelOptions=labelOptions(textsize="13px", direction="auto"),
      weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Tracks"
    )
  })
  
}