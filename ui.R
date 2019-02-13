notification.tracks <- notificationItem(
  text = paste("Number of days in database:", dat$tracks$Track_Date %>% unique() %>% length()),
  icon = icon("calendar")
)

notification.flights <- notificationItem(
  text = paste("Number of flights in database:", dat$flightplan$Flight_Plan_ID %>% unique() %>% length()),
  icon = icon("plane")
)



menu.PLT <- menuItem(
  "PLT Analysis",
  tabName = "plt",
  icon = icon("location-arrow")
)

tab.PLT <- tabItem(
  tabName = "plt",
  div(style = "margin: -15px;", leafletOutput("plt_map", height="100%"))
)



menu.ORD_Val <- menuItem(
  "ORD Validation",
  tabName = "ordval",
  icon = icon("plane-arrival")
)

tab.ORD_Val <- tabItem(
  tabName = "ordval",
  "Things here"
)



menu.ORD_Cal <- menuItem(
  "ORD Calibration",
  tabName = "ordcal",
  icon = icon("cog")
)

tab.ORD_Cal <- tabItem(
  tabName = "ordcal",
  "Content here"
)



header <- dashboardHeader(
  title = title,
  dropdownMenu(
    type = "notifications",
    notification.tracks,
    notification.flights
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarmenu",
    menu.PLT,
    conditionalPanel(
      "input.sidebarmenu === 'plt'",
      tagList(
        pickerInput("plt_date", "Date", plt_date_choices),
        pickerInput("plt_leg", "Contains Path Leg(s)", plt_leg_choices, multiple = T, options = pickerOptions(actionsBox = T)),
        uiOutput("plt_callsign_ui"),
        pickerInput("plt_vol", "Displayed Volumes", choices = plt_vol_choices, multiple = T, options = pickerOptions(actionsBox = T))
      ),
      div(style="height:15px;")
    ),
    menu.ORD_Val,
    menu.ORD_Cal
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "script.js")
  ),
  tabItems(
    tab.PLT,
    tab.ORD_Val,
    tab.ORD_Cal
  )
)

ui <- dashboardPage(header,sidebar,body)