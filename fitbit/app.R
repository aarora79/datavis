library(shinydashboard)
library(dplyr)
library(dygraphs)
library(lubridate)
library(xts)

ui <- dashboardPage(
  dashboardHeader(title = "Nidhi's Fitbit dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Temporal visualization made using R, Dygraph and the shinydashboard package."),
              h5("Data shown here is exported from Fitbit and represents fitbit tracked activity for Nidhi for October 2017.Timeseries plots for active minutes and steps count for the month of October 2017. Data obtained from Nidhi's Fitbit account (Fitbit only allows to download last 1 month of data). Click on the links below for access to code and data"),
              #h5("Code available here http://aa1603.georgetown.domains/ANLY503/Portfolio/live/Fitbit/fitbit.csv"),
              a(href="http://aa1603.georgetown.domains/ANLY503/Portfolio/live/Fitbit/app.R", "Code"),
              a(href="http://aa1603.georgetown.domains/ANLY503/Portfolio/live/Fitbit/fitbit.csv", ", Data")
              
      )
    ),
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(width=12,dygraphOutput("Steps_and_calories", height = 250))
    ),
    fluidRow(
      box(width=12,dygraphOutput("minutes", height = 250))
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  fb_df = read.csv("fitbit.csv")
  fb_df = fb_df %>%
    mutate(Steps = as.numeric(gsub(",", "", Steps)) ) %>%
    mutate(Activity.Calories = as.numeric(gsub(",", "", Activity.Calories)) ) %>%
    mutate(Minutes.Sedentary = as.numeric(gsub(",", "", Minutes.Sedentary)) ) %>%
    mutate(Minutes.Lightly.Active = as.numeric(gsub(",", "", Minutes.Lightly.Active)) ) %>%
    mutate(Minutes.Fairly.Active = as.numeric(gsub(",", "", Minutes.Fairly.Active)) ) %>%
    mutate(Minutes.Very.Active = as.numeric(gsub(",", "", Minutes.Very.Active)) ) %>%
    mutate(Date = mdy(Date))
  
  steps_and_calories =  fb_df %>%
                        dplyr::select(Activity.Calories, Steps)
  row.names(steps_and_calories) = fb_df$Date
  #, order.by = steps_and_calories$Date
  steps_and_calories_ts = as.xts(steps_and_calories)
  
  
  minutes = fb_df %>%
            dplyr::select(Minutes.Sedentary, Minutes.Lightly.Active,
                          Minutes.Fairly.Active, Minutes.Very.Active)
  row.names(minutes) = fb_df$Date
  minutes_ts = as.xts(minutes)
  
  
  output$Steps_and_calories <- renderDygraph({
    dygraph(steps_and_calories_ts, main = "Daily steps and calories burnt") %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE)
  })
  
  output$minutes <- renderDygraph({
    dygraph(minutes_ts, main = "Active Minutes") %>%
      dyHighlight(highlightSeriesOpts  = list(strokeWidth = 3))
  })
}

shinyApp(ui, server)
