#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(XML)
library(plotly)

stadiums <- read.csv("./data/stadiums.csv")

choices <- c("MLB", "AL", "NL", "AL West", "AL East", "AL Central", "NL West", "NL East", "NL Central", as.character(stadiums$TEAM))

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Baseball Infometric"),
    tabsetPanel(
        tabPanel("Application",
            wellPanel(
            fluidRow(
                column(width = 4,sliderInput("dateSlider", label = "Select Range of Years to use for Model Building and Data Table", min = 1970, max = 2016, value = c(1999, 2000), sep = "")),
                column(width = 4,selectInput("Subset", label = "Select Subset of MLB Teams", choices = choices)),
                column(width = 4,submitButton("Apply Changes"))

            )
            ),
            tabsetPanel(
                tabPanel("Predictions",
                         dataTableOutput("TeamBasedCurrentPredictions"),
                         h2("Amount of Increase in Win Percentage Per unit increase in Variable"),
                         fluidRow(
                             column(width = 8, align = "right", plotlyOutput("importancePlot"))
                         )
                         ),
                tabPanel("Stadium Map", leafletOutput("MLBMap")),
                tabPanel("Selected Subset in Training Data", dataTableOutput("AllData")),
                tabPanel("Current Year Data", dataTableOutput("CurrentYear")))),
        tabPanel("About",
                 h1("General overview"),
                 h3("This app allows the user to predict the final records of a given set of MLB teams based on a window of training data in MLB history from 1970 to 2016. Further for geographical reference
                    a tab shows the location of the teams listed in the subset that was chosen."),
                 br(),
                 h2("Inputs"),
                 p("The app has two available input widgets, the Slider for dates and the Subset drop down."),
                 p("- The slider allows you to choose the date window to use for the training data"),
                 p("- The Subset drop down allows you to choose the group of teams for which to predict the outcome, and view the stadium locations. These subsets are broken down by full MLB, the two leagues, all the divisions, and then each individual team "),
                 p("- The Apply changes button is there so that you can make changes to both before having to wait for the model to rebuild. Make sure to click apply changes to see changes in the data/map/predictions."),
                 br(),
                 h2("Predictions Tab"),
                 p("This tab shows the output of the model based predictions as well as the team's current win/loss record for comparison. At the bottom of this tab you can see a plot of the coefficients used in the model with their associated value. This chart shows how much the win percentage changes by a unit increase in that variable"),
                 br(),
                 h2("Stadium Map Tab"),
                 p("This tab shows a map of the locations of the stadiums in the selected subset. This map is interactive and you can zoom, pan, and click on logos of teams for more information"),
                 br(),
                 h2("Selected Subset in Training Data Tab"),
                 p("This tab shows the data for the teams in the selected subset in the years of the training data window selected. This may be useful if you want to see how a team performed in a specific window or how the league in general performed during a time period."),
                 br(),
                 h2("Current Year Data Tab"),
                 p("Tab shows the current year data for the teams in the selected subset.")
                 )

    )
  )
)
