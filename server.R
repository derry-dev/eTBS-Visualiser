function(input, output, session) {
  
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
    valueExpr = get_db_connection(input$db_driver, input$db_server, input$db_database, input$db_username, input$db_password)
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
          query_table_list %>% sqlQuery(con(),.) %>% as.data.table()
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
        
        # tbl_Flight_Plan
        flightplan <- reactive({
          query_flightplan %>% sqlQuery(con(),.) %>% as.data.table()
        })
        
        # tbl_Polygon
        volumes <- eventReactive(con(), {
          query_volumes %>% sqlQuery(con(),.) %>% as.data.table()
        })
        
        # tbl_Path_Leg
        legs <- eventReactive(con(), {
          query_legs %>% sqlQuery(con(),.) %>% as.data.table()
        })
        
        # Subsetted tbl_Radar_Track_Point
        tracks <- eventReactive(input$pltmap_fpid, {
          sprintf(
            "%s WHERE Flight_Plan_ID IN ('%s')",
            query_tracks, paste(input$pltmap_fpid, collapse = "','")
          ) %>%
            sqlQuery(con(),.) %>%
            as.data.table()  %>%
            .[is.na(Path_Leg), Path_Leg := "NA"]
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
          pltmap_fpid_choices <- flightplan()[FP_Date %in% input$pltmap_fpdate]$Flight_Plan_ID %>% as.character() %>% unique() %>% sort()
          updatePickerInput(
            session,
            "pltmap_fpid",
            choices = pltmap_fpid_choices,
            choicesOpt = list(subtext = flightplan()[Flight_Plan_ID %in% pltmap_fpid_choices]$Callsign %>% as.character())
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
          if (is.na(as.numeric(input$pltmap_volume_colour_r))) {
            updateTextInput(session, "pltmap_volume_colour_r", value=0)
          } else {
            if (as.numeric(input$pltmap_volume_colour_r) > 255 | as.numeric(input$pltmap_volume_colour_r) < 0) {
              updateTextInput(session, "pltmap_volume_colour_r", value=0)
            }
          }
        })
        
        # Update PLT Map Volume Colour Selection (Green)
        observeEvent(input$pltmap_volume_colour_g, {
          if (is.na(as.numeric(input$pltmap_volume_colour_g))) {
            updateTextInput(session, "pltmap_volume_colour_g", value=0)
          } else {
            if (as.numeric(input$pltmap_volume_colour_g) > 255 | as.numeric(input$pltmap_volume_colour_g) < 0) {
              updateTextInput(session, "pltmap_volume_colour_g", value=0)
            }
          }
        })
        
        # Update PLT Map Volume Colour Selection (Blue)
        observeEvent(input$pltmap_volume_colour_b, {
          if (is.na(as.numeric(input$pltmap_volume_colour_b))) {
            updateTextInput(session, "pltmap_volume_colour_b", value=0)
          } else {
            if (as.numeric(input$pltmap_volume_colour_b) > 255 | as.numeric(input$pltmap_volume_colour_b) < 0) {
              updateTextInput(session, "pltmap_volume_colour_b", value=0)
            }
          }
        })
        
        # Update PLT Map Volume Highlight Colour Selection (Red)
        observeEvent(input$pltmap_volume_highlightcolour_r, {
          if (is.na(as.numeric(input$pltmap_volume_highlightcolour_r))) {
            updateTextInput(session, "pltmap_volume_highlightcolour_r", value=0)
          } else {
            if (as.numeric(input$pltmap_volume_highlightcolour_r) > 255 | as.numeric(input$pltmap_volume_highlightcolour_r) < 0) {
              updateTextInput(session, "pltmap_volume_highlightcolour_r", value=0)
            }
          }
        })
        
        # Update PLT Map Volume Highlight Colour Selection (Green)
        observeEvent(input$pltmap_volume_highlightcolour_g, {
          if (is.na(as.numeric(input$pltmap_volume_highlightcolour_g))) {
            updateTextInput(session, "pltmap_volume_highlightcolour_g", value=0)
          } else {
            if (as.numeric(input$pltmap_volume_highlightcolour_g) > 255 | as.numeric(input$pltmap_volume_highlightcolour_g) < 0) {
              updateTextInput(session, "pltmap_volume_highlightcolour_g", value=0)
            }
          }
        })
        
        # Update PLT Map Volume Highlight Colour Selection (Blue)
        observeEvent(input$pltmap_volume_highlightcolour_b, {
          if (is.na(as.numeric(input$pltmap_volume_highlightcolour_b))) {
            updateTextInput(session, "pltmap_volume_highlightcolour_b", value=0)
          } else {
            if (as.numeric(input$pltmap_volume_highlightcolour_b) > 255 | as.numeric(input$pltmap_volume_highlightcolour_b) < 0) {
              updateTextInput(session, "pltmap_volume_highlightcolour_b", value=0)
            }
          }
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
        
        ord_dates <- reactive({
          query_vw_ORD_Calibration_View_Date %>% sqlQuery(con(), .) %>% unlist() %>% as.vector() %>% .[order(as.Date(., format="%d/%m/%y", origin="1970-01-01"))] %>% as.character()
        })
        
        observeEvent(ord_dates(), {
          updatePickerInput(
            session,
            "iasprofile_dates",
            choices = ord_dates()
          )
        })
        
        ord_callsigns <- reactive({
          sprintf(
            "%s WHERE FP_Date IN ('%s')",
            query_vw_ORD_Calibration_View_Callsigns,
            paste(input$iasprofile_dates, collapse = "','")
          ) %>%
            sqlQuery(con(), .) %>% as.data.table()
        })
        
        observeEvent(ord_callsigns(), {
          updatePickerInput(
            session,
            "iasprofile_callsigns",
            choices = ord_callsigns()$Callsign %>% as.character()
          )
        })
        
        ord_cali <- reactive({
          sprintf(
            "%s WHERE FP_Date IN ('%s') AND Follower_Callsign IN ('%s')",
            query_vw_ORD_Calibration_View,
            paste(input$iasprofile_dates, collapse = "','"),
            paste(input$iasprofile_callsigns, collapse = "','")
          ) %>%
            sqlQuery(con(), .) %>% as.data.table()
        })
        
        output$ord_cali_table <-  DT::renderDataTable({
          datatable(
            ord_cali(),
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
        
        ord_cali_nls <- reactive({
          req(input$iasprofile_callsigns)
          d <- ord_cali()[Follower_Range_To_Threshold >= 0 & Follower_Range_To_Threshold <= 6 & !is.na(Mode_S_IAS)][order(Follower_Range_To_Threshold)]
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
        
        output$ord_iasprofile_nls <- renderText({
          paste0(
            "Model Output Parameters\r\n",
            paste0(names(ord_cali_nls()$m$m$getPars()), ": ", ord_cali_nls()$m$m$getPars(), collapse = "\r\n")
          )
        })
        
        output$ord_iasprofile <- renderPlotly({
          plot_ly() %>%
            add_markers(
              x = ord_cali_nls()$x,
              y = ord_cali_nls()$y,
              name = "Observed",
              marker=list(color="rgb(85,87,89)")
            ) %>%
            add_lines(
              x = ord_cali_nls()$x,
              y = ord_cali_nls()$m$m$fitted(),
              name = "Fitted",
              line=list(color="rgb(213,16,103)")
            ) %>% layout(
              xaxis = list(title="Follower Range to Threshold (NM)"),
              yaxis = list(title="Mode S IAS (kts)"),
              showlegend = F
            ) %>%
            config(
              displaylogo = F
            )
        })
        
      }
      
    }
    
  })
  
}
