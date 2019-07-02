# ----------------------------------------------------------------------- #
# Headers -----------------------------------------------------------------
# ----------------------------------------------------------------------- #

header <- dashboardHeader(
  title = "eTBS Visualiser",
  titleWidth = 250
)

# ----------------------------------------------------------------------- #
# Sidebar -----------------------------------------------------------------
# ----------------------------------------------------------------------- #

sidebar <- dashboardSidebar(
  collapsed	= T,
  width = 250,
  sidebarMenu(
    id = "tabs",
    menuItem(
      text = "Database Dashboard",
      tabName = "tab_db",
      icon = icon("database")
    ),
    menuItem(
      text = "Path Leg Tracking",
      tabName = "tab_plt",
      icon = icon("map-marker-alt"),
      menuItem(
        text = "Track Visualiser",
        tabName = "tab_pltmap",
        icon = icon("map-marked-alt")
      ),
      menuItem(
        text = "Plots & Analysis",
        tabName = "tab_pltplot",
        icon = icon("chart-line")
      )
    ),
    menuItem(
      text = "Optimised Runway Delivery",
      tabName = "tab_ord",
      icon = icon("plane-arrival"),
      menuItem(
        text = "Calibration",
        tabName = "tab_ordcali",
        icon = icon("cog")
      ),
      menuItem(
        text = "Validation",
        tabName = "tab_ordvali",
        icon = icon("clipboard-check")
      )
    )
  )
)

# ----------------------------------------------------------------------- #
# Body --------------------------------------------------------------------
# ----------------------------------------------------------------------- #

body <- dashboardBody(
  tags$link(rel = "stylesheet", type = "text/css", href = "theme.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "wrappers.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "cheeky_tweaks.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "loading.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "bttn.min.css"),
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$script(src = "pltmap.js"),
  tabItems(
    tabItem(
      "tab_db",
      fluidRow(
        column(
          3,
          box(
            title = "Connection Manager",
            solidHeader = T,
            collapsible  = F,
            width = NULL,
            fluidPage(
              textInput("db_driver", "Driver Name", "SQL Server"),
              textInput("db_server", "Server Name", "DESKTOP-U2P5V4F"),
              textInput("db_database", "Database Name", "eTBS_UTMA_Validation_V002a"),
              textInput("db_username", "Username", "vbuser"),
              passwordInput("db_password", "Password", "Th!nkvbuser"),
              div(
                class = "centered",
                actionButton("db_connect", "Connect", icon("database")),
                uiOutput("db_status")
              )
            )
          )
        ),
        column(
          9,
          tabBox(
            title = "Database Explorer",
            side = "right",
            width = NULL,
            selected = "Query Tool",
            tabPanel("Table List", DT::dataTableOutput("db_databases")),
            tabPanel(
              "Query Tool",
              box(
                solidHeader = T,
                collapsible  = F,
                width = NULL,
                textAreaInput(
                  "db_query",
                  NA,
                  placeholder = "Enter your query here...",
                  width = "100%",
                  height = "246px",
                  resize = "vertical"
                ),
                div(
                  class = "centered",
                  actionButton("db_execute", "Execute", icon("play")),
                  div(style = "margin: 0 5px 0 5px"),
                  actionButton("db_clear", "Clear", icon("eraser"))
                ),
                DT::dataTableOutput("db_output")
              )
            )
          )
        )
      )
    ),
    tabItem(
      "tab_pltmap",
      div(id = "pltmap_wrapper", style = "margin: -15px;", leafletOutput("pltmap", height="100%")),
      div(
        style = "position: absolute; top: 60px;",
        dropdown(
          pickerInput("pltmap_fpdate", "Select Date", NULL, multiple=T),
          pickerInput("pltmap_fpid", "Select FP ID", NULL, multiple=T, options = list(`actions-box` = TRUE)),
          # pickerInput("pltmap_legs", "Filter By Leg", NULL, multiple=T, options = list(`actions-box` = TRUE)),
          div(
            class = "centered",
            actionButton("pltmap_update", "Update", icon = icon("sync-alt"))
          ),
          hr(),
          pickerInput("pltmap_volumes", "Display Volumes", NULL, multiple=T, options = list(`actions-box` = TRUE)),
          style = "simple",
          icon = icon("filter"),
          tooltip = tooltipOptions(title = "Filtering Options", placement = "right")
        ),
        div(style = "height: 5px"),
        downloadButton("pltmap_screenshot", NULL, class = "bttn-simple"),
        div(style = "height: 5px"),
        uiOutput("plt_tracks_button")
      )
    ),
    tabItem(
      "tab_pltplot",
      fluidRow(
        column(
          3,
          box(
            solidHeader = T,
            collapsible  = F,
            width = NULL
            
          )
        ),
        column(
          9,
          box(
            solidHeader = T,
            collapsible  = F,
            width = NULL
            
          )
        )
      ),
      tabBox(
        title = "Tables",
        side = "right",
        width = NULL,
        selected = "Flight Plans",
        tabPanel("Legs", DT::dataTableOutput("db_legs")),
        tabPanel("Volumes", DT::dataTableOutput("db_volumes")),
        tabPanel("Flight Plans", DT::dataTableOutput("db_flightplans"))
      )
    ),
    tabItem("tab_ordcali", "ord calidation"),
    tabItem("tab_ordvali", "ord valibration")
  ),
  conditionalPanel(
    condition="$('html').hasClass('shiny-busy')",
    div(id="loadmessage", "Loading, please wait...")
  )
)

# ----------------------------------------------------------------------- #
# Page --------------------------------------------------------------------
# ----------------------------------------------------------------------- #

dashboardPage(skin = "red", header, sidebar, body)
