

source("global.R")
source("beranda_ui.R")
source("eksplorasi_ui.R")
source("managemen_ui.R")
source("asumsi_ui.R")
source("uji_rata_ui.R")
source("uji_proporsi_ui.R")
source("uji_varians_ui.R")
source("anova_ui.R")
source("regresi_ui.R")
source("insights_ui.R")

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "SINDRA"),
  
  # Sidebar default
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home & Metadata", tabName = "beranda", icon = icon("home")),
      menuItem("Data Management", tabName = "data_management", icon = icon("database")),
      menuItem("Exploratory Data", tabName = "eksplorasi", icon = icon("search")),
      menuItem("Assumption Test", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("Inferential Statistics", icon = icon("chart-line"),
               menuSubItem("Independent Mean Test", tabName = "uji_rata"),
               menuSubItem("Proportion Test", tabName = "uji_proporsi"),
               menuSubItem("Variance Test", tabName = "uji_varians"),
               menuSubItem("ANOVA", tabName = "anova")
      ),
      menuItem("Regression", tabName = "regresi", icon = icon("line-chart")),
      menuItem("Insights", tabName = "insights", icon = icon("line-chart"))
    )
  ),
  
  # Body (Main Page)
  dashboardBody(
    tabItems(
      tabItem(tabName = "beranda",beranda_ui("beranda")),
      tabItem(tabName = "data_management", data_management_ui("management")),
      tabItem(tabName = "eksplorasi",eksplorasi_ui("eksplor")),
      tabItem(tabName = "asumsi",asumsi_ui("asumsi")),
      tabItem(tabName = "uji_rata",uji_rata_ui("uji_rata2")),
      tabItem(tabName = "uji_proporsi",uji_proporsi_ui("uji_proporsi")),
      tabItem(tabName = "uji_varians",uji_varians_ui("uji_varians")),
      tabItem(tabName = "anova",anova_ui("anova")),
      tabItem(tabName = "regresi",regresi_ui("regresi")),
      tabItem(tabName = "insights",ins_ui("insights"))
      
    )
  )
)


server <- function(input, output, session) {
  
  data_management_server("management")
  eksplorasi_server("eksplor")
  asumsi_server("asumsi")
  uji_rata_server("uji_rata2")
  uji_proporsi_server("uji_proporsi")
  server_uji_varians(input, output, session)
  server_anova(input, output, session) 
  regresi_server("regresi")
  ins_server(input, output, session)
  
}
shinyApp(ui = ui, server = server)