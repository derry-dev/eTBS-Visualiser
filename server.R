function(input, output, session) {
  
  # Store client session data
  cdata <- session$clientData
  
  # Debug session data display
  onclick("think_logo", showModal(debug_dialogue()))
  output$clientdataText <- renderText({
    cnames <- names(cdata) %>% .[. %!in% grep("^output_spinner.*$", ., value=T)]
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
    htmltools::HTML('<i class="fa fa-spinner fa-spin fa-3x fa-fw" style="font-size:24px"></i>')
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
        div(style="margin: 7px 0 0 6px;", "Connected", icon("check-circle"))
      } else {
        div(style="margin: 7px 0 0 6px;", "Error", icon("times-circle"))
      }
    })
  })
  
  # Render PLT map tiles
  output$pltmap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = F, preferCanvas = T)) %>%
      addProviderTiles(providers$Esri.WorldImagery, options=providerTileOptions(noWrap=TRUE), group="Satellite") %>%
      addProviderTiles(providers$CartoDB.Positron, options=providerTileOptions(noWrap=TRUE), group="Grey") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, options=providerTileOptions(noWrap=TRUE), group="Dark") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, options=providerTileOptions(noWrap=TRUE), group="Light") %>%
      addProviderTiles(providers$Esri.DeLorme, options=providerTileOptions(noWrap=TRUE), group="Topo") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, options=providerTileOptions(noWrap=TRUE), group="OSM") %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options=providerTileOptions(noWrap=TRUE), group="OSM B&W") %>%
      addLayersControl(baseGroups=c("Satellite","Grey","Dark","Light","Topo","OSM","OSM B&W"), options=layersControlOptions(collapsed=T))
  })
  
  observe({
    
    # Runs only when database connection is valid
    if (con() != -1L) {
      
      if (input$tabs == "tab_db") {
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
            rownames = F,
            selection = "none",
            options = list(
              pageLength = 15,
              lengthMenu = seq(5, 100, 5),
              columnDefs = list(list(className = 'dt-center', targets = "_all")),
              scrollX = T
            )
          )
        })
        
        # Clear query textbox
        observeEvent(input$db_clear, {
          updateTextAreaInput(session, "db_query", value="")
        })
        
        # Get list of databases, schemas and tables
        db_list <- reactive({
          " SET NOCOUNT ON
            DECLARE @AllTables table (\"Database\" nvarchar(4000), \"Schema\" nvarchar(4000), \"Table\" nvarchar(4000))
            INSERT INTO @AllTables (\"Database\", \"Schema\", \"Table\")
            EXEC sp_msforeachdb 'select \"?\", s.name, t.name from [?].sys.tables t inner join sys.schemas s on t.schema_id=s.schema_id'
            SET NOCOUNT OFF
            SELECT * FROM @AllTables ORDER BY 1
          " %>% sqlQuery(con(),.) %>% as.data.table()
        })
        
        # Render list of databases, schemas and tables
        output$db_databases <- DT::renderDataTable({
          datatable(
            db_list(),
            rownames = F,
            selection = "none",
            options = list(
              pageLength = 15,
              lengthMenu = seq(5, 100, 5),
              columnDefs = list(list(className = 'dt-center', targets = "_all")),
              scrollX = T
            )
          )
        })
        
      } else if (input$tabs == "tab_plt") {
      # ----------------------------------------------------------------------- #
      # PLT Tab -----------------------------------------------------------------
      # ----------------------------------------------------------------------- #
        
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
        
        # tbl_Flight_Plan
        flightplan <- reactive({
          " SELECT * FROM tbl_Flight_Plan
            LEFT JOIN (
              SELECT Flight_Plan_ID AS Flight_Plan_ID_2, Time_At_4DME, Time_At_1DME FROM tbl_Flight_Plan_Derived
            ) AS t ON Flight_Plan_ID = Flight_Plan_ID_2
          " %>% sqlQuery(con(),.) %>% as.data.table()
        })
        
        # tbl_Polygon
        volumes <- eventReactive(con(), {
          " SELECT * FROM tbl_Polygon
            LEFT JOIN (
              SELECT Volume_Name AS Volume_Name_2, Min_Altitude, Max_Altitude FROM tbl_Volume
            ) AS t ON Volume_Name = Volume_Name_2
          " %>% sqlQuery(con(),.) %>% as.data.table()
        })
        
        # tbl_Path_Leg
        legs <- eventReactive(con(), {
          " SELECT * FROM tbl_Path_Leg
          " %>% sqlQuery(con(),.) %>% as.data.table()
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
            style = "position: absolute; left: 20px; top: 21px; z-index: 100",
            dropdown(
              pickerInput("pltmap_fpdate", "Select Date", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
              pickerInput("pltmap_fpid", "Select FP ID", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
              pickerInput("pltmap_legs", "Filter By Leg", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
              pickerInput("pltmap_volumes", "Display Volumes", NULL, multiple=T, options = list(`actions-box` = T, `live-search` = T), width="220px"),
              style = "simple",
              icon = icon("filter"),
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
              style = "simple",
              icon = div(style = "padding-right: 1px;", icon("bullseye")),
              tooltip = tooltipOptions(title = "Marker Settings", placement = "right")
            ),
            div(style = "height: 5px"),
            dropdown(
              div(style = "text-align: center", tags$b("Volume Polygon Settings")),
              sliderTextInput("pltmap_volume_weight", "Weight", choices=seq(1, 50, 1), selected=5, width="220px"),
              sliderTextInput("pltmap_volume_highlightweight", "Highlight Weight", choices=seq(1, 50, 1), selected=3, width="220px"),
              sliderTextInput("pltmap_volume_dash", "Dash Size", choices=seq(1, 50, 1), selected=5, width="220px"),
              sliderTextInput("pltmap_volume_opacity", "Opacity", choices=seq(0, 1, 0.01), selected=0.5, width="220px"),
              sliderTextInput("pltmap_volume_highlightopacity", "Highlight Opacity", choices=seq(0, 1, 0.01), selected=0.5, width="220px"),
              sliderTextInput("pltmap_volume_fillopacity", "Fill Opacity", choices=seq(0, 1, 0.01), selected=0.1, width="220px"),
              sliderTextInput("pltmap_volume_highlightfillopacity", "Highlight Fill Opacity", choices=seq(0, 1, 0.01), selected=0.5, width="220px"),
              div(style = "text-align: center", tags$b("Colour")),
              div(
                style = "display: flex; justify-content: space-between; vertical-align: middle; margin-top: -18px;",
                div(style = "padding-top: 25px", "R"),
                textInput("pltmap_volume_colour_r", "", 255, width="46px"),
                div(style = "padding-top: 25px", "G"),
                textInput("pltmap_volume_colour_g", "", 0, width="46px"),
                div(style = "padding-top: 25px", "B"),
                textInput("pltmap_volume_colour_b", "", 0, width="46px")
              ),
              div(style = "text-align: center", tags$b("Highlight Colour")),
              div(
                style = "display: flex; justify-content: space-between; vertical-align: middle; margin-top: -18px;",
                div(style = "padding-top: 25px", "R"),
                textInput("pltmap_volume_highlightcolour_r", "", 128, width="46px"),
                div(style = "padding-top: 25px", "G"),
                textInput("pltmap_volume_highlightcolour_g", "", 0, width="46px"),
                div(style = "padding-top: 25px", "B"),
                textInput("pltmap_volume_highlightcolour_b", "", 0, width="46px")
              ),
              style = "simple",
              icon = icon("th"),
              tooltip = tooltipOptions(title = "Polygon Settings", placement = "right")
            ),
            div(style = "height: 5px"),
            downloadButton("pltmap_screenshot", NULL, class = "bttn-simple")
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
          p <- leafletProxy("pltmap", data=d) %>% clearGroup("Tracks") %>% removeControl("Leg_Legend")
          pal <- colorFactor(brewer.pal(11, input$pltmap_marker_palette), domain=legs()$Path_Leg_Name)
          p %>% addCircleMarkers(
            lng = ~Lon*180/pi,
            lat = ~Lat*180/pi,
            color = ~pal(Path_Leg),
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
              title = "Leg",
              pal = pal,
              values = ~Path_Leg,
              opacity = 0.85,
              layerId = "Leg_Legend"
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
            
            mapshot(p, file = file, vwidth = input$pltDim[1], vheight = input$pltDim[2])
            
          },
          contentType = "image/png"
        )
        
        # Render flightplan table
        output$plt_flightplans <- DT::renderDataTable({
          datatable(
            flightplan(),
            rownames = F,
            selection = "none",
            options = list(
              pageLength = 15,
              lengthMenu = seq(5, 100, 5),
              columnDefs = list(list(className = 'dt-center', targets = "_all")),
              scrollX = T
            )
          )
        })
        
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
        
        # Render volumes table
        output$plt_volumes <- DT::renderDataTable({
          datatable(
            volumes(),
            rownames = F,
            selection = "none",
            options = list(
              pageLength = 15,
              lengthMenu = seq(5, 100, 5),
              columnDefs = list(list(className = 'dt-center', targets = "_all")),
              scrollX = T
            )
          )
        })
        
        # Render Path Legs table
        output$plt_legs <- DT::renderDataTable({
          datatable(
            legs(),
            rownames = F,
            selection = "none",
            options = list(
              pageLength = 15,
              lengthMenu = seq(5, 100, 5),
              columnDefs = list(list(className = 'dt-center', targets = "_all")),
              scrollX = T
            )
          )
        })
      
      } else if (input$tabs == "tab_ord") {
      # ----------------------------------------------------------------------- #
      # ORD Tab -----------------------------------------------------------------
      # ----------------------------------------------------------------------- #
        
        # ORD Calibration By Aircraft
        
        ord_cali_1 <- reactiveVal("SELECT TOP (0) * FROM vw_ORD_Calibration_View" %>% sqlQuery(con(), .) %>% as.data.table())
        
        output$ord_1 <- renderUI({
          
          ord_1_date_choices <- "
            SET DATEFORMAT dmy
            SELECT DISTINCT CAST(FP_Date AS date) AS FP_Date FROM vw_ORD_Calibration_View
            ORDER BY CAST(FP_Date AS date)
          " %>% sqlQuery(con(), .) %>% unlist() %>% as.vector()
          
          fluidPage(
            pickerInput(
              "ord_1_date",
              "Select Date",
              choices = ord_1_date_choices,
              multiple = T,
              options = list(`actions-box` = T, `live-search` = T),
              width = "200px"
            ),
            div(style = "width: 15px"),
            pickerInput(
              "ord_1_callsign",
              "Select Callsign",
              choices = NULL,
              multiple = T,
              options = list(`actions-box` = T, `live-search` = T),
              width = "200px"
            ),
            radioGroupButtons(
              "ord_1_speedtype",
              "Select Speed Type",
              choices = c("IAS", "GSPD")
            ),
            uiOutput("ord_1_alt_ui"),
            actionButton("ord_1_run", "Run Calibration"),
            uiOutput("ord_output_1")
          )
          
        })
        
        observeEvent(input$ord_1_date, {
          
          ord_1_callsign_choices <- sprintf(
            " SET DATEFORMAT dmy
              SELECT DISTINCT CAST(FP_Date AS date) AS FP_Date, Follower_Callsign, Follower_Aircraft_Type
              FROM vw_ORD_Calibration_View WHERE CAST(FP_Date AS date) IN ('%s')
            ",
            paste(input$ord_1_date %>% as.Date() %>% as.character(), collapse = "','")
          ) %>% sqlQuery(con(), .) %>% as.data.table()
          
          updatePickerInput(
            session,
            "ord_1_callsign",
            choices = ord_1_callsign_choices$Follower_Callsign %>% as.character(),
            choicesOpt = list(subtext = ord_1_callsign_choices[,paste(Follower_Aircraft_Type, FP_Date)] %>% as.character())
          )
          
        })
        
        observeEvent(input$ord_1_speedtype, {
          if (input$ord_1_speedtype == "GSPD") {
            output$ord_1_alt_ui <- renderUI({
              numericInput("ord_1_alt", "Airport Altitude Above Sea Level (ft)", value = 550, step = 10, width = "200px")
            })
          }
        })
        
        onclick(
          "ord_1_run",
          ord_cali_1(
            sprintf(
              " SET DATEFORMAT dmy
                SELECT * FROM vw_ORD_Calibration_View
                WHERE CAST(FP_Date AS date) IN ('%s') AND Follower_Callsign IN ('%s')",
              paste(input$ord_1_date %>% as.Date() %>% as.character(), collapse = "','"),
              paste(input$ord_1_callsign, collapse = "','")
            ) %>% sqlQuery(con(), .) %>% as.data.table()
          )
        )
        
        ord_cali_nls_1 <- reactive({
          d <- ord_cali_1()[Follower_Range_To_Threshold >= 0 & Follower_Range_To_Threshold <= 6 & !is.na(Mode_S_IAS)][order(Follower_Range_To_Threshold)]
          y <- if (input$ord_1_speedtype == "IAS") {
            d$Mode_S_IAS
          } else if (input$ord_1_speedtype == "GSPD") {
            d$Track_Speed/(1+input$ord_1_alt/60000)
          }
          x <- d$Follower_Range_To_Threshold
          if (min(x) < 1 & max(x) >= 4) {
            m <- tryCatch(
              nls(
                y ~ airspeed_model_vector_break(x, a, a1, b, n1, n2),
                start = list(a = 140, a1 = 140, b = 160, n1 = 3, n2 = 4),
                control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
              ),
              error = function(e) NULL
            )
            if (is.null(m)) {
              x <- x %>% .[. <= 2]
              y <- y[1:length(x)]
              m <- tryCatch(
                nls(
                  y ~ airspeed_model_vector_break_2(x, a, a1),
                  start = list(a = 140, a1 = 140),
                  control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
                ),
                error = function(e) NULL
              )
              if (is.null(m)) {
                m <- tryCatch(
                  nls(
                    y ~ a,
                    start = list(a = 140),
                    control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
                  ),
                  error = function(e) NULL
                )
              }
            }
          }
          return(list(d=d,x=x,y=y,m=m))
        })
        
        observeEvent(ord_cali_1(), {
          if (nrow(ord_cali_1()) > 0) {
            output$ord_output_1 <- renderUI({
              tagList(
                plotlyOutput("ord_iasprofile_1"),
                div(style = "height: 15px"),
                verbatimTextOutput("ord_iasprofile_nls_1"),
                hr(),
                column(
                  6,
                  div(style = "margin-bottom: 15px; font-size: 15px;", "Unique Flight Pairs"),
                  DT::dataTableOutput("ord_cali_flights_1")
                ),
                column(
                  6,
                  div(style = "margin-bottom: 15px; font-size: 15px;", "ORD Calibration View"),
                  DT::dataTableOutput("ord_cali_table_1")
                )
              )
            })
          } else {
            output$ord_output_1 <- renderUI({})
          }
        })
        
        output$ord_cali_flights_1 <- DT::renderDataTable({
          datatable(
            unique(ord_cali_1()[,c("FP_Date",
                                   "Leader_Callsign",
                                   "Leader_Aircraft_Type",
                                   "Leader_RECAT_Wake_Turbulence_Category",
                                   "Follower_Callsign",
                                   "Follower_Aircraft_Type",
                                   "Follower_RECAT_Wake_Turbulence_Category")]),
            rownames = F,
            selection = "none",
            options = list(
              pageLength = 15,
              lengthMenu = seq(5, 100, 5),
              columnDefs = list(list(className = 'dt-center', targets = "_all")),
              scrollX = T
            )
          )
        })
        
        output$ord_cali_table_1 <- DT::renderDataTable({
          datatable(
            ord_cali_1(),
            rownames = F,
            selection = "none",
            options = list(
              pageLength = 15,
              lengthMenu = seq(5, 100, 5),
              columnDefs = list(list(className = 'dt-center', targets = "_all")),
              scrollX = T
            )
          )
        })
        
        output$ord_iasprofile_nls_1 <- renderText({
          paste0(
            "Model Output Parameters\n",
            paste0(names(ord_cali_nls_1()$m$m$getPars()), " = ", ord_cali_nls_1()$m$m$getPars(), collapse = "  ")
          )
        })
        
        output$ord_iasprofile_1 <- renderPlotly({
          p <- plot_ly() %>%
            # add_lines(
            #   x = ord_cali_nls_1()$x,
            #   y = ord_cali_nls_1()$d$Follower_Threshold_Surface_Headwind %>% min(., na.rm=T),
            #   name = "Surface Headwind",
            #   line = list(color = "rgb(85,87,89)")
            # ) %>%
            add_markers(
              x = ord_cali_nls_1()$x,
              y = ord_cali_nls_1()$y,
              name = "Observed Speed",
              marker = list(color = "rgb(128,34,69)")
            ) %>%
            add_lines(
              x = ord_cali_nls_1()$x,
              y = ord_cali_nls_1()$m$m$fitted(),
              name = "Fitted Speed",
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
          ggplotly(p, width = session$clientData$output_ord_iasprofile_1_width) # Width fix
        })
        
        # ORD Calibration By Aircraft Type
        
        ord_cali_2 <- reactiveVal("SELECT TOP (0) * FROM vw_ORD_Calibration_View" %>% sqlQuery(con(), .) %>% as.data.table())
        
        output$ord_2 <- renderUI({
          
          ord_2_type_choices <- "
          SELECT DISTINCT Follower_Aircraft_Type, COUNT(FP_Date+'_'+Follower_Callsign) AS Flight_Num
          FROM (
          	SELECT DISTINCT Follower_Aircraft_Type, FP_Date, Follower_Callsign FROM vw_ORD_Calibration_View
          ) AS t
          GROUP BY Follower_Aircraft_Type ORDER BY Follower_Aircraft_Type
          " %>% sqlQuery(con(), .) %>% as.data.table()
          
          fluidPage(
            pickerInput(
              "ord_2_type",
              "Select Aircraft Type",
              choices = ord_2_type_choices$Follower_Aircraft_Type %>% as.character(),
              choicesOpt = list(subtext = ord_2_type_choices$Flight_Num %>% paste("Flights:", .)),
              multiple = T,
              options = list(`actions-box` = T, `live-search` = T),
              width = "200px"
            ),
            sliderInput(
              "ord_2_wind",
              "Filter by Observed Surface Headwind (kts)",
              min = 0,
              max = 1000,
              step = 1,
              value = c(1,100),
              dragRange = T,
              width = "450px"
            ),
            pickerInput(
              "ord_2_flights",
              "Filter by Flight",
              choices = NULL,
              multiple = T,
              options = list(`actions-box` = T, `live-search` = T),
              width = "200px"
            ),
            h5("Note: Aircraft types with more unique flights will take longer to run!"),
            actionButton("ord_2_run", "Run Calibration"),
            uiOutput("ord_output_2")
          )
          
        })
        
        observeEvent(input$ord_2_type, {
          
          ord_2_wind_choices <- sprintf(
            " SELECT
              	 FLOOR(MIN(Follower_Threshold_Surface_Headwind)) AS Min_Headwind,
              	 CEILING(MAX(Follower_Threshold_Surface_Headwind)) AS Max_Headwind
              FROM (
              	SELECT * FROM vw_ORD_Calibration_View
              	WHERE Follower_Aircraft_Type IN ('%s')
              ) AS t
            ",
            paste(input$ord_2_type, collapse = "','")
          ) %>% sqlQuery(con(), .) %>% as.data.table()
          
          updateSliderInput(
            session,
            "ord_2_wind",
            min = ord_2_wind_choices$Min_Headwind,
            max = ord_2_wind_choices$Max_Headwind,
            value = c(ord_2_wind_choices$Min_Headwind, ord_2_wind_choices$Max_Headwind)
          )
          
        })
        
        observeEvent(input$ord_2_wind, {
          
          ord_2_flights_choices <- sprintf(
            " SELECT DISTINCT FP_Date, Follower_Callsign, MIN(Follower_Threshold_Surface_Headwind) AS Surface_Headwind FROM vw_ORD_Calibration_View
              WHERE Follower_Aircraft_Type IN ('%s') AND Follower_Threshold_Surface_Headwind BETWEEN %i AND %i
              GROUP BY FP_Date, Follower_Callsign ORDER BY FP_Date, Follower_Callsign
            ",
            paste(input$ord_2_type, collapse = "','"), input$ord_2_wind[1], input$ord_2_wind[2]
          ) %>% sqlQuery(con(), .) %>% as.data.table() %>% .[order(as.Date(FP_Date, format="%d/%m/%y", origin="1970-01-01"), Follower_Callsign)]
          
          updatePickerInput(
            session,
            "ord_2_flights",
            choices = ord_2_flights_choices[,paste(FP_Date, Follower_Callsign)] %>% as.character(),
            selected = ord_2_flights_choices[,paste(FP_Date, Follower_Callsign)] %>% as.character(),
            choicesOpt = list(subtext = ord_2_flights_choices[,paste0("Surface Headwind: ", round(Surface_Headwind, 2), "kts")] %>% as.character())
          )
          
        })
        
        onclick(
          "ord_2_run",
          ord_cali_2(
            sprintf(
              "SELECT * FROM vw_ORD_Calibration_View WHERE Follower_Aircraft_Type IN ('%s') AND FP_Date+' '+Follower_Callsign IN ('%s')",
              paste(input$ord_2_type, collapse = "','"),
              paste(input$ord_2_flights, collapse = "','")
            ) %>%
              sqlQuery(con(), .) %>% as.data.table()
          )
        )
        
        ord_cali_nls_2 <- reactive({
          d <- ord_cali_2()[Follower_Range_To_Threshold >= 0 & Follower_Range_To_Threshold <= 6 & !is.na(Mode_S_IAS)][order(Follower_Range_To_Threshold)]
          y <- d$Mode_S_IAS
          x <- d$Follower_Range_To_Threshold
          if (min(x) < 1 & max(x) >= 4) {
            m <- tryCatch(
              nls(
                y ~ airspeed_model_vector_break(x, a, a1, b, n1, n2),
                start = list(a = 140, a1 = 140, b = 160, n1 = 3, n2 = 4),
                control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
              ),
              error = function(e) NULL
            )
            if (is.null(m)) {
              x <- x %>% .[. <= 2]
              y <- y[1:length(x)]
              m <- tryCatch(
                nls(
                  y ~ airspeed_model_vector_break_2(x, a, a1),
                  start = list(a = 140, a1 = 140),
                  control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
                ),
                error = function(e) NULL
              )
              if (is.null(m)) {
                m <- tryCatch(
                  nls(
                    y ~ a,
                    start = list(a = 140),
                    control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
                  ),
                  error = function(e) NULL
                )
              }
            }
          }
          return(list(d=d,x=x,y=y,m=m))
        })

        observeEvent(ord_cali_2(), {
          if (nrow(ord_cali_2()) > 0) {
            output$ord_output_2 <- renderUI({
              tagList(
                plotlyOutput("ord_iasprofile_2"),
                div(style = "height: 15px"),
                verbatimTextOutput("ord_iasprofile_nls_2"),
                div(style = "height: 15px"),
                plotlyOutput("ord_a2"),
                hr(),
                column(
                  6,
                  div(style = "margin-bottom: 15px; font-size: 15px;", "Unique Flight Pairs"),
                  DT::dataTableOutput("ord_cali_flights_2")
                ),
                column(
                  6,
                  div(style = "margin-bottom: 15px; font-size: 15px;", "ORD Calibration View"),
                  DT::dataTableOutput("ord_cali_table_2")
                )
              )
            })
          } else {
            output$ord_output_2 <- renderUI({})
          }
        })

        output$ord_cali_flights_2 <- DT::renderDataTable({
          datatable(
            unique(ord_cali_2()[,c("FP_Date",
                                 "Leader_Callsign",
                                 "Leader_Aircraft_Type",
                                 "Leader_RECAT_Wake_Turbulence_Category",
                                 "Follower_Callsign",
                                 "Follower_Aircraft_Type",
                                 "Follower_RECAT_Wake_Turbulence_Category")]),
            rownames = F,
            selection = "none",
            options = list(
              pageLength = 15,
              lengthMenu = seq(5, 100, 5),
              columnDefs = list(list(className = 'dt-center', targets = "_all")),
              scrollX = T
            )
          )
        })

        output$ord_cali_table_2 <- DT::renderDataTable({
          datatable(
            ord_cali_2(),
            rownames = F,
            selection = "none",
            options = list(
              pageLength = 15,
              lengthMenu = seq(5, 100, 5),
              columnDefs = list(list(className = 'dt-center', targets = "_all")),
              scrollX = T
            )
          )
        })

        output$ord_iasprofile_nls_2 <- renderText({
          paste0(
            "Model Output Parameters\n",
            paste0(names(ord_cali_nls_2()$m$m$getPars()), " = ", ord_cali_nls_2()$m$m$getPars(), collapse = "  ")
          )
        })

        output$ord_iasprofile_2 <- renderPlotly({
          p <- plot_ly() %>%
            add_markers(
              x = ord_cali_nls_2()$x,
              y = ord_cali_nls_2()$y,
              name = "Observed Speed",
              marker = list(color = "rgb(128,34,69)")
            ) %>%
            add_lines(
              x = ord_cali_nls_2()$x,
              y = ord_cali_nls_2()$m$m$fitted(),
              name = "Fitted Speed",
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
          ggplotly(p, width = session$clientData$output_ord_iasprofile_2_width) # Width fix
        })
        
        output$ord_a2 <- renderPlotly({

          d <- ord_cali_2()[Follower_Range_To_Threshold >= 0 & Follower_Range_To_Threshold <= 6 & !is.na(Mode_S_IAS)][order(Follower_Range_To_Threshold)]
          d$landing_adjustment <- sapply(1:nrow(d), function(i) calc_landing_adjustment(lss_types[[d$Follower_Aircraft_Type[i]]], d$Follower_Threshold_Surface_Headwind[i]))
          d$a2 <- ord_cali_nls_2()$m$m$getPars()[["a"]] - d$landing_adjustment
          
          p <- plot_ly() %>%
            add_histogram(
              x = d$a2,
              histfunc = "count",
              histnorm = "probability"
            ) %>%
            layout(
              hovermode = "compare",
              xaxis = list(title = "a2")
            ) %>%
            config(
              displaylogo = F
            )
          ggplotly(p, width = session$clientData$output_ord_iasprofile_width) # Width fix

        })
        
      }
      
    }
    
  })
  
}
