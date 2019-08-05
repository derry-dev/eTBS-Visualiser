# ----------------------------------------------------------------------- #
# Headers -----------------------------------------------------------------
# ----------------------------------------------------------------------- #

header <- dashboardHeader(
  title = "eTBS Visualiser",
  titleWidth = 250,
  tags$li(class = "dropdown header_button", id = "db_button", icon("database")),
  tags$li(class = "dropdown logo", style = "width: 125px; height: 50px; padding-left: 15px; padding-right: 15px;", imageOutput("think_logo"))
)

# ----------------------------------------------------------------------- #
# Sidebar Menu Items ------------------------------------------------------
# ----------------------------------------------------------------------- #

sidebar_tab_db <- menuItem(
  text = "Database Dashboard",
  tabName = "tab_db",
  icon = icon("database")
)

sidebar_tab_plt <- menuItem(
  text = "Path Leg Tracking",
  tabName = "tab_plt",
  icon = icon("map-marker-alt")
)

sidebar_tab_ord <- menuItem(
  text = "Optimised Runway Delivery",
  tabName = "tab_ord",
  icon = icon("plane-arrival")
)

# ----------------------------------------------------------------------- #
# Sidebar -----------------------------------------------------------------
# ----------------------------------------------------------------------- #

sidebar <- dashboardSidebar(
  collapsed	= F,
  width = 250,
  sidebarMenu(
    id = "tabs",
    sidebar_tab_db,
    sidebar_tab_plt,
    sidebar_tab_ord
  )
)

# ----------------------------------------------------------------------- #
# CSS/JS ------------------------------------------------------------------
# ----------------------------------------------------------------------- #

www <- tagList(
  tags$link(rel = "stylesheet", type = "text/css", href = "header_buttons.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "theme.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "wrappers.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "cheeky_tweaks.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "loading_v2.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "bttn.min.css"),
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$script(src = "pltmap.js")
)

# ----------------------------------------------------------------------- #
# Body Tab Items ----------------------------------------------------------
# ----------------------------------------------------------------------- #

body_tab_db <- tabItem(
  "tab_db",
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
          NULL,
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

body_tab_plt <- tabItem(
  "tab_plt",
  box(
    width = NULL,
    leafletOutput("pltmap"),
    uiOutput("pltmap_filters_ui")
  ),
  tabBox(
    title = "Tables",
    side = "right",
    width = NULL,
    selected = "Flight Plans",
    tabPanel("Legs", DT::dataTableOutput("plt_legs")),
    tabPanel("Volumes", DT::dataTableOutput("plt_volumes")),
    tabPanel("Flight Plans", DT::dataTableOutput("plt_flightplans")),
    tabPanel("Plotted Tracks", DT::dataTableOutput("plt_tracks"))
  )
)

body_tab_ord <- tabItem(
  "tab_ord",
  box(
    title = "ORD Calibration By Aircraft",
    width = NULL,
    solidHeader = T,
    collapsible = T,
    collapsed = T,
    uiOutput("ord_1")
  ),
  box(
    title = "ORD Calibration By Aircraft Type",
    width = NULL,
    solidHeader = T,
    collapsible = T,
    collapsed = T,
    uiOutput("ord_2")
  )
)

# ----------------------------------------------------------------------- #
# Loading Message ---------------------------------------------------------
# ----------------------------------------------------------------------- #

loading <- conditionalPanel(
  condition="$('html').hasClass('shiny-busy')",
  div(id="loadmessage", uiOutput("spinner"))
)

# ----------------------------------------------------------------------- #
# Body --------------------------------------------------------------------
# ----------------------------------------------------------------------- #

body <- dashboardBody(
  useShinyjs(),
  www,
  tabItems(
    body_tab_db,
    body_tab_plt,
    body_tab_ord
  ),
  loading
)

# ----------------------------------------------------------------------- #
# Page --------------------------------------------------------------------
# ----------------------------------------------------------------------- #

dashboardPage(
  skin = "red",
  header,
  sidebar,
  body
)
