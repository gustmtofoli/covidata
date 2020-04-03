library(shinydashboard)
library(DT)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)

header <- dashboardHeader(
    title = "COVID-19",
    titleWidth = 187
)

sidebar <- dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
        menuItem("Dashboard", tabName = "covid_statistics", icon = icon("dashboard")),
        menuItem("System Info", tabName = "system_info", icon = icon("info"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(
          "system_info",
          box(
            width = 12,
            title = "Data",
            "The data used in this application is provided by John Hopkins CSSE. You can check the data by yourself visiting this url: https://github.com/CSSEGISandData/COVID-19"
          ),
          box(
            width = 12,
            title = "Project",
            "This is an open source project. Here's the url: https://github.com/gustmtofoli/covidata"
          )
        ),
        tabItem(
            "covid_statistics",
            fluidRow(
                infoBoxOutput("all_confirmed_info"),
                infoBoxOutput("all_recovered_info"),
                infoBoxOutput("all_active_cases_info"),
                infoBoxOutput("all_death_info"),
                infoBoxOutput("all_death_ratio_info")
            ),
            fluidRow(
                box(
                  width = 12,
                  collapsible = FALSE,
                  title = "Rank",
                  DT::dataTableOutput("country_rank")  %>% withSpinner(color="#0dc5c1")
                )
            ),
            fluidRow(
              box(
                width = 12,
                materialSwitch(inputId = "group_countries", label = "All countries: ", status = "primary", right = FALSE),
                conditionalPanel(
                  "!input.group_countries",
                  uiOutput("select_country") %>% withSpinner(color="#0dc5c1"),
                  plotlyOutput("confirmed_curve")  %>% withSpinner(color="#0dc5c1")
                ),
                conditionalPanel(
                  "input.group_countries",
                  plotlyOutput("curve_all_countries")  %>% withSpinner(color="#0dc5c1")
                )
                
              )
            )
        )
    )
)

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

# ======================================================================================

variables <- reactiveValues(file_n_confirmed = NULL, 
                            file_n_death = NULL,
                            file_n_recovered = NULL,
                            n_confirmed_country = 0,
                            n_death_country = 0,
                            n_recovered_country = 0,
                            all_confirmed = 0,
                            all_death = 0,
                            all_recovered = 0)

Download_Covid_Data <- function(url, output_file_name) {
    dest_file <- paste0("~/", output_file_name)
    write.csv(data.frame(), output_file_name)
    download.file(url, dest_file)
    data <- read.csv(url, header = TRUE, sep = ",")
    return (data)
}

server <- function(input, output) {

    output$select_country <- renderUI({
        url_n_confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
        url_n_death <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
        url_n_recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
        
        n_confirmed_data <- Download_Covid_Data(url_n_confirmed, "n_confirmed.csv")
        n_death_data <- Download_Covid_Data(url_n_death, "n_death.csv")
        n_recovered_data <- Download_Covid_Data(url_n_recovered, "n_recovered.csv")
        
        all_confirmed_data <- na.omit(n_confirmed_data[, ncol(n_confirmed_data)])
        variables$all_confirmed <- sum(all_confirmed_data)
        
        all_death_data <- na.omit(n_death_data[ncol(n_death_data)])
        variables$all_death <- sum(all_death_data)
        
        all_recovered_data <- na.omit(n_recovered_data[ncol(n_recovered_data)])
        variables$all_recovered <- sum(all_recovered_data)
        
        variables$file_n_confirmed <- n_confirmed_data
        variables$file_n_death <- n_death_data
        variables$file_n_recovered <- n_recovered_data
        
        countries <- unique(n_confirmed_data$Country.Region)
        
        selectInput("select_country", label = h5("Select country"),
                    choices = countries,
                    selected = 1, multiple = FALSE)
        
    })
    
    output$all_confirmed_info <- renderInfoBox({
        all_confirmed <- variables$all_confirmed
        infoBox(
            "World confirmed",
            paste0(all_confirmed),
            icon = icon("bug"),
            color = "blue", 
            fill = TRUE
        )
    })
    
    output$all_active_cases_info <- renderInfoBox({
      all_recovered <- variables$all_recovered
      all_death <- variables$all_death
      all_confirmed <- variables$all_confirmed
      all_active_cases <- all_confirmed - all_death - all_recovered
      infoBox(
        "World active cases",
        paste0(all_active_cases),
        icon = icon("bullseye"),
        color = "yellow", 
        fill = TRUE
      )
    })
    
    output$all_recovered_info <- renderInfoBox({
      all_recovered <- variables$all_recovered
      infoBox(
        "World recovered",
        paste0(all_recovered),
        icon = icon("check"),
        color = "green", 
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
            color = "navy",
            fill = TRUE
        )
    })
    
    output$country_rank <- DT::renderDataTable({
      df_confirmed <- variables$file_n_confirmed
      df_death <- variables$file_n_death
      df_recovered <- variables$file_n_recovered
      
      ctry = c()
      confirmed = c()
      death = c()
      recovered = c()
      death_ratio = c()
      active_cases = c()
      
      for (country in unique(df_confirmed$Country.Region)) {
        subset_country_confirmed <- subset(df_confirmed, df_confirmed$Country.Region == country)
        sum_confirmed <- sum(subset_country_confirmed[ncol(subset_country_confirmed)])
        
        subset_country_death <- subset(df_death, df_death$Country.Region == country)
        sum_death <- sum(subset_country_death[ncol(subset_country_death)])
        
        subset_country_recovered <- subset(df_recovered, df_recovered$Country.Region == country)
        sum_recovered <- sum(subset_country_recovered[ncol(subset_country_recovered)])
        
        ratio_country <- round((sum_death/sum_confirmed)*100, 2)
        
        ctry = c(ctry, country)
        confirmed = c(confirmed, sum_confirmed)
        death = c(death, sum_death)
        recovered = c(recovered, sum_recovered)
        death_ratio = c(death_ratio, ratio_country)
        active_cases = c(active_cases, sum_confirmed - sum_death - sum_recovered)
      }
      
      df_country_rank <- data.frame(country = ctry, confirmed = confirmed, death = death, active_cases = active_cases,recovered = recovered, death_ratio = death_ratio)
      df_country_rank
    })
    
    output$confirmed_curve <- renderPlotly({
      if (!is.null(variables$file_n_confirmed) && !is.null(variables$file_n_death) && !is.null(variables$file_n_recovered)) {
        
        n_confirmed_data <- variables$file_n_confirmed
        n_death_data <- variables$file_n_death
        n_recovered_data <- variables$file_n_recovered
        
        country <- input$select_country
        
        country_c <- subset(n_confirmed_data, n_confirmed_data$Country.Region == country)
        country_d <- subset(n_death_data, n_death_data$Country.Region == country)
        country_r <- subset(n_recovered_data, n_recovered_data$Country.Region == country)
      
        days <- colnames(n_confirmed_data)[5:ncol(country_c)]
        
        days_new <- c()
        for (label in days) {
          if (nchar(label) < 8) {
            label <- gsub("1.20", "01.20", label)
            label <- gsub("2.20", "02.20", label)
            label <- gsub("3.20", "03.20", label)
            label <- gsub("4.20", "04.20", label)
            label <- gsub("5.20", "05.20", label)
            label <- gsub("6.20", "06.20", label)
            label <- gsub("7.20", "07.20", label)
            label <- gsub("8.20", "08.20", label)
            label <- gsub("9.20", "09.20", label)
          }
          
          days_new <- c(days_new, label)
        }
        
        counts_c <- country_c[, 5: ncol(country_c)]
        counts_d <- country_d[, 5: ncol(country_d)]
        counts_r <- country_r[, 5: ncol(country_r)]
        
        n_c <- c()
        for (i in 1:length(counts_c)) {
          n_c <- c(n_c, sum(counts_c[, i]))
        }
        
        n_d <- c()
        for (i in 1:length(counts_d)) {
          n_d <- c(n_d, sum(counts_d[, i]))
        }
        
        n_r <- c()
        for (i in 1:length(counts_r)) {
          n_r <- c(n_r, sum(counts_r[, i]))
        }
        
        df_timeseries <- data.frame(day=days_new, n_c=n_c, n_d=n_d, n_r=n_r)
        
        plot <- plot_ly(data = df_timeseries, x = ~day, y = ~n_c, name = 'Confirmed', type = 'scatter', mode = 'lines+markers')
        plot <- plot %>% add_trace(y = ~n_d, name = 'Death', line = list(color = 'red'))
        plot <- plot %>% add_trace(y = ~n_r, name = 'Recovered', line = list(color = 'green'))
        plot <- plot %>% layout(yaxis = list(title="Occurrences"), hovermode = 'compare')
        plot
      }
    })
    
    output$curve_all_countries <- renderPlotly({
      if (!is.null(variables$file_n_confirmed) && !is.null(variables$file_n_death) && !is.null(variables$file_n_recovered)) {
        
        n_confirmed_data <- variables$file_n_confirmed
        n_death_data <- variables$file_n_death
        n_recovered_data <- variables$file_n_recovered
        
        days <- colnames(n_confirmed_data)[5:ncol(n_confirmed_data)]
        
        days_new <- c()
        for (label in days) {
          if (nchar(label) < 8) {
            label <- gsub("1.20", "01.20", label)
            label <- gsub("2.20", "02.20", label)
            label <- gsub("3.20", "03.20", label)
            label <- gsub("4.20", "04.20", label)
            label <- gsub("5.20", "05.20", label)
            label <- gsub("6.20", "06.20", label)
            label <- gsub("7.20", "07.20", label)
            label <- gsub("8.20", "08.20", label)
            label <- gsub("9.20", "09.20", label)
          }
          
          days_new <- c(days_new, label)
        }
        
        counts_c_all <- n_confirmed_data[, 5: ncol(n_confirmed_data)]
        counts_d_all <- n_death_data[, 5: ncol(n_death_data)]
        counts_r_all <- n_recovered_data[, 5: ncol(n_recovered_data)]
        
        n_c_all <- c()
        for (i in 1:length(counts_c_all)) {
          n_c_all <- c(n_c_all, sum(counts_c_all[, i]))
        }
        
        n_d_all <- c()
        for (i in 1:length(counts_d_all)) {
          n_d_all <- c(n_d_all, sum(counts_d_all[, i]))
        }
        
        n_r_all <- c()
        for (i in 1: length(counts_r_all)) {
          n_r_all <- c(n_r_all, sum(counts_r_all[, i]))
        }
        
        df_timeseries <- data.frame(day=days_new, n_c = n_c_all, n_d = n_d_all, n_r = n_r_all)
        
        plot <- plot_ly(data = df_timeseries, x = ~day, y = ~n_c, name = 'Confirmed', type = 'scatter', mode = 'lines+markers')
        plot <- plot %>% add_trace(y = ~n_d, name = 'Death', line = list(color = 'red'))
        plot <- plot %>% add_trace(y = ~n_r, name = 'Recovered', line = list(color = 'green'))
        plot <- plot %>% layout(yaxis = list(title="Occurrences"), hovermode = 'compare')
        plot
      }
    })
    
    observeEvent(input$select_country, {
        file_n_confirmed <- variables$file_n_confirmed
        file_n_death <- variables$file_n_death
        file_n_recovered <- variables$file_n_recovered
        
        country <- input$select_country
        confirmed_country <- subset(file_n_confirmed, file_n_confirmed$Country.Region == country)
        death_country <- subset(file_n_death, file_n_death$Country.Region == country)
        recovered_country <- subset(file_n_recovered, file_n_recovered$Country.Region == country)
        
        n_confirmed_country <- sum(confirmed_country[ncol(confirmed_country)])
        n_death_country <- sum(death_country[ncol(death_country)])
        n_recovered_country <- sum(recovered_country[ncol(recovered_country)])
        
        variables$n_confirmed_country <- n_confirmed_country
        variables$n_death_country <- n_death_country
        variables$n_recovered_country <- n_recovered_country
        
    })
  
}

shinyApp(ui = ui, server = server)
