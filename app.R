library(shinydashboard)
# library(shinydashboardPlus)
# library(leaflet)
# library(DT)
# library(plotly)
library(shinycssloaders)
library(dplyr)
# library(shinyWidgets)
# library(shinyjs)

header <- dashboardHeader(
    title = tagList(
        span(class = "logo-lg", "COVID-19"),
        img(src = "" )),
    titleWidth = 187
)

sidebar <- dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
        menuItem("Dashboard", tabName = "covid_statistics", icon = icon("dashboard"))
    )
)


body <- dashboardBody(
    tabItems(
        tabItem(
            "covid_statistics",
            fluidRow(
                infoBoxOutput("all_confirmed_info"),
                infoBoxOutput("all_death_info"),
                infoBoxOutput("all_death_ratio_info")
            ),
            fluidRow(
                box(
                    width = 12,
                    uiOutput("select_country") %>% withSpinner(color="#0dc5c1")
                )
            ),
            fluidRow(
                infoBoxOutput("n_confirmed_info"),
                infoBoxOutput("n_death_info"),
                infoBoxOutput("n_death_ratio_info")
            )
        )
    )
)

# Define UI for application that draws a histogram
ui <- fluidPage(
    shinyUI(
        dashboardPage(
            skin = "black",
            header,
            sidebar,
            body
        )
    )
)

variables <- reactiveValues(file_n_confirmed = NULL, 
                            file_n_death = NULL,
                            n_confirmed_country = 0,
                            n_death_country = 0,
                            all_confirmed = 0,
                            all_death = 0)

Download_Covid_Data <- function(url, output_file_name) {
    host <- "https://data.humdata.org"
    dest_file <- paste0("~/", output_file_name)
    file_path <- paste0(host, url)
    
    # if (!file.exists(dest_file)) {
    
    write.csv(data.frame(), output_file_name)
    download.file(file_path, dest_file)
    # }
    
    data <- read.csv(file_path, header = TRUE, sep = ",")
    
    return (data)
}

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$select_country <- renderUI({
        url_n_confirmed <- "/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_19-covid-Confirmed.csv&filename=time_series_2019-ncov-Confirmed.csv"
        url_n_death <- "/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_19-covid-Deaths.csv&filename=time_series_2019-ncov-Deaths.csv"
        
        n_confirmed_data <- Download_Covid_Data(url_n_confirmed, "n_confirmed.csv")
        variables$file_n_confirmed <- n_confirmed_data
        
        all_confirmed_data <- na.omit(n_confirmed_data[ncol(n_confirmed_data)])
        variables$all_confirmed <- sum(all_confirmed_data)
        
        n_death_data <- Download_Covid_Data(url_n_death, "n_death.csv")
        variables$file_n_death <- n_death_data
        
        all_death_data <- na.omit(n_death_data[ncol(n_death_data)])
        variables$all_death <- sum(all_death_data)
        
        countries <- unique(n_confirmed_data$Country.Region)
        
        selectInput("select_country", label = h4("Select country"),
                    choices = countries,
                    selected = 1, multiple = FALSE)
    })
    
    output$n_confirmed_info <- renderInfoBox({
        n_confirmed_country <- variables$n_confirmed_country
        infoBox(
            "Confirmed",
            paste0(n_confirmed_country),
            icon = icon("bug"),
            color = "orange", 
            fill = TRUE
        )
    })
    
    output$n_death_info <- renderInfoBox({
        n_death_country <- variables$n_death_country
        infoBox(
            "Death",
            paste0(n_death_country),
            icon = icon("skull"),
            color = "red", 
            fill = TRUE
        )
    })
    
    output$all_confirmed_info <- renderInfoBox({
        all_confirmed <- variables$all_confirmed
        infoBox(
            "World confirmed",
            paste0(all_confirmed),
            icon = icon("bug"),
            color = "orange", 
            fill = TRUE
        )
    })
    
    output$all_death_info <- renderInfoBox({
        all_death <- variables$all_death
        infoBox(
            "World death",
            paste0(all_death),
            icon = icon("skull"),
            color = "red", 
            fill = TRUE
        )
    })
    
    output$all_death_ratio_info <- renderInfoBox({
        all_confirmed <- variables$all_confirmed
        all_death <- variables$all_death
        all_death_ratio <- round((all_death/all_confirmed)*100, 2)
        infoBox(
            "World death ratio",
            paste0(all_death_ratio, "%"),
            icon = icon("percent"),
            color = "blue", 
            fill = TRUE
        )
    })
    
    output$n_death_ratio_info <- renderInfoBox({
        n_confirmed <- variables$n_confirmed_country
        n_death <- variables$n_death_country
        n_death_ratio <- round((n_death/n_confirmed)*100, 2)
        infoBox(
            "Death ratio",
            paste0(n_death_ratio, "%"),
            icon = icon("percent"),
            color = "blue", 
            fill = TRUE
        )
    })
    
    # ===================================================================
    
    observeEvent(input$select_country, {
        file_n_confirmed <- variables$file_n_confirmed
        file_n_death <- variables$file_n_death
        
        country <- input$select_country
        confirmed_country <- subset(file_n_confirmed, file_n_confirmed$Country.Region == country)
        death_country <- subset(file_n_death, file_n_death$Country.Region == country)
        
        n_confirmed_country <- confirmed_country[1, ncol(confirmed_country)]
        n_death_country <- death_country[1, ncol(death_country)]
        
        variables$n_confirmed_country <- n_confirmed_country
        variables$n_death_country <- n_death_country
        
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
