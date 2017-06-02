#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
library(XML)
library(randomForest)
library(mboost)
library(ggplot2)
library(plotly)

set.seed(1234)

set.ALWest <- c("LAA", "TEX", "HOU", "SEA", "OAK")
set.ALEast <- c("NYY", "TBR", "TOR", "BOS", "BAL")
set.ALCentral <- c("CHW", "CLE", "MIN", "DET", "KCR")
set.NLWest <- c("SFG", "SDP", "COL", "LAD", "ARI")
set.NLEast <- c("NYM", "MIA", "PHI", "ATL", "WSN")
set.NLCentral <-c("STL", "CIN", "CHC", "MIL", "PIT")

set.NL <- c(set.NLCentral, set.NLEast, set.NLWest)
set.AL <- c(set.ALCentral, set.ALEast, set.ALWest)
set.MLB <- c(set.NL, set.AL)
stadiumfile <- file.path("./data", "stadiums.csv")
stadiums <- read.csv(stadiumfile)

logofile <- file.path("./data", "logos.csv")
logos <- read.csv(logofile)

baseballfile <- file.path("./data", "baseballstats5.csv")
allstats <- read.csv(baseballfile)

stadiums.logos <- cbind(stadiums, logos)

# Define server logic required to draw a histogram
picksubset <- function(subset)
{
    if (subset  == "MLB")
    {
        subset <- set.MLB
    }
    else{
        if (subset == "AL"){
            subset <- set.AL
        }
        else{
            if(subset == "NL"){
                subset <- set.NL
            }
            else{
                if(subset == "AL West")
                {
                    subset <- set.ALWest
                }
                else{
                    if(subset == "AL Central")
                    {
                        subset <- set.ALCentral
                    }
                    else{
                        if(subset == "AL East")
                        {
                            subset <- set.ALEast
                        }
                        else{
                            if(subset == "NL West")
                            {
                                subset <- set.NLWest
                            }
                            else{
                                if(subset == "NL East")
                                {
                                    subset <- set.NLEast
                                }
                                else{
                                    if(subset == "NL Central")
                                    {
                                        subset <- set.NLCentral
                                    }
                                    else{
                                        subset <- subset
                                    }
                                }
                            }
                        }
                    }


                }
            }
        }
    }
    return(subset)
}

mlbmap <- function(subset)
{
    subset <- picksubset(subset)

    useset <- stadiums.logos[which(stadiums.logos$TEAM %in% subset),]
    icons <- makeIcon(iconUrl = useset$link, iconWidth = 60, iconHeight = 60)
    stadiumlatlong <- data.frame(lat = useset$Latitude, lng = useset$Longitude)
    stadiumlatlong %>%
        leaflet() %>%
        addTiles(options = ) %>%
        addMarkers(icon = icons, popup =useset$NAME)
}

picktable <- function(subset, year1 = 2010, year2 = 2011)
{
    subset <- picksubset(subset)
    teamstable <- allstats[which(allstats$Tm %in% subset),]
    lessyears <- teamstable[teamstable$year <= year2,]
    finaltable <- lessyears[lessyears$year >= year1,]
    return(finaltable)

}



getmlbData <- function(year1, year2)
{
    data <- data.frame()
    for (i in 1:(year2 - year1))
    {
        #battinginfo
        year <- year1 + i - 1
        fileurl <- paste("http://www.baseball-reference.com/leagues/MLB/", year, ".shtml", sep = "")
        doc <- htmlTreeParse(fileurl, useInternal = TRUE)
        battingstats <- as.numeric(xpathSApply(doc, "//table[@id = 'teams_standard_batting']//td[@data-stat]", xmlValue))
        battingcolnames <- xpathSApply(doc, "//table[@id = 'teams_standard_batting']//th[@aria-label]", xmlValue)
        teams <- xpathSApply(doc, "//table[@id = 'teams_standard_batting']//th[@scope = 'row']", xmlValue)
        battingcolnames <- unique(battingcolnames)
        matrixversion <- matrix(battingstats, ncol = length(battingcolnames)-1, byrow = TRUE)
        battingdata <- as.data.frame(matrixversion)
        battingdata <- cbind(teams,battingdata)
        colnames(battingdata) <- battingcolnames
        battingdata<- battingdata[-c(length(battingdata$`R/G`) - 1, length(battingdata$`R/G`)),]


        #pitchinginfo
        pitchingurl <- paste("http://www.baseball-reference.com/leagues/MLB/",year,"-standard-pitching.shtml", sep = "")
        pitchingdoc <- htmlTreeParse(pitchingurl, useInternal = TRUE)
        pitchingstats <- as.numeric(xpathSApply(pitchingdoc, "//table[@id = 'teams_standard_pitching']//td[@data-stat]", xmlValue))
        pitchingcolnames <- xpathSApply(pitchingdoc, "//table[@id = 'teams_standard_pitching']//th[@aria-label]", xmlValue)
        teams <- xpathSApply(pitchingdoc, "//table[@id = 'teams_standard_pitching']//th[@scope = 'row']", xmlValue)
        pitchingcolnames <- unique(pitchingcolnames)
        matrixversion <- matrix(pitchingstats, ncol = length(pitchingcolnames)-1, byrow = TRUE)
        pitchingdata <- as.data.frame(matrixversion)
        pitchingdata <- cbind(teams, pitchingdata)
        colnames(pitchingdata)<- pitchingcolnames
        pitchingdata <- pitchingdata[-c(length(pitchingdata$PAge)-1, length(pitchingdata$PAge)),]

        alldata <- merge(battingdata, pitchingdata, by = "Tm")
        alldata$HRperPA <- alldata$HR.x/alldata$PA
        alldata$SOperPA <- alldata$SO.x/alldata$PA
        alldata$winP <- alldata$W/alldata$G.y
        alldata$RunScoredPerGame <- alldata$`R/G`
        alldata$SOperW <- alldata$`SO/W`
        alldata$RunAllowedPerGame <- alldata$`RA/G`
        alldata$year <- year

        data <- rbind(data, alldata)


    }
    return(data)

}

getCurrentData <- function(year = 2017)
{
    fileurl <- paste("http://www.baseball-reference.com/leagues/MLB/", year, ".shtml", sep = "")
    doc <- htmlTreeParse(fileurl, useInternal = TRUE)
    battingstats <- as.numeric(xpathSApply(doc, "//table[@id = 'teams_standard_batting']//td[@data-stat]", xmlValue))
    battingcolnames <- xpathSApply(doc, "//table[@id = 'teams_standard_batting']//th[@aria-label]", xmlValue)
    teams <- xpathSApply(doc, "//table[@id = 'teams_standard_batting']//th[@scope = 'row']", xmlValue)
    battingcolnames <- unique(battingcolnames)
    matrixversion <- matrix(battingstats, ncol = length(battingcolnames)-1, byrow = TRUE)
    battingdata <- as.data.frame(matrixversion)
    battingdata <- cbind(teams,battingdata)
    colnames(battingdata) <- battingcolnames
    battingdata<- battingdata[-c(length(battingdata$`R/G`) - 1, length(battingdata$`R/G`)),]


    #pitchinginfo
    pitchingurl <- paste("http://www.baseball-reference.com/leagues/MLB/",year,"-standard-pitching.shtml", sep = "")
    pitchingdoc <- htmlTreeParse(pitchingurl, useInternal = TRUE)
    pitchingstats <- as.numeric(xpathSApply(pitchingdoc, "//table[@id = 'teams_standard_pitching']//td[@data-stat]", xmlValue))
    pitchingcolnames <- xpathSApply(pitchingdoc, "//table[@id = 'teams_standard_pitching']//th[@aria-label]", xmlValue)
    teams <- xpathSApply(pitchingdoc, "//table[@id = 'teams_standard_pitching']//th[@scope = 'row']", xmlValue)
    pitchingcolnames <- unique(pitchingcolnames)
    matrixversion <- matrix(pitchingstats, ncol = length(pitchingcolnames)-1, byrow = TRUE)
    pitchingdata <- as.data.frame(matrixversion)
    pitchingdata <- cbind(teams, pitchingdata)
    colnames(pitchingdata)<- pitchingcolnames
    pitchingdata <- pitchingdata[-c(length(pitchingdata$PAge)-1, length(pitchingdata$PAge)),]

    alldata <- merge(battingdata, pitchingdata, by = "Tm")
    alldata$HRperPA <- alldata$HR.x/alldata$PA
    alldata$SOperPA <- alldata$SO.x/alldata$PA
    alldata$winP <- alldata$W/alldata$G.y
    alldata$RunScoredPerGame <- alldata$`R/G`
    alldata$SOperW <- alldata$`SO/W`
    alldata$RunAllowedPerGame <- alldata$`RA/G`
    alldata$year <- year
    return(alldata)
}

pickcurrentdata <- function(subset)
{
    currentdata <- getCurrentData()
    subset <- picksubset(subset)
    table <- currentdata[which(currentdata$Tm %in% subset),]
    return(table)
}

selectTrainData <- function(year1, year2)
{
    set.seed(1234)
    lessyears   <- allstats[allstats$year <= year2,]
    finaltable  <- lessyears[lessyears$year >= year1,]
    dependent   <- "winP"
    independent <- c("BatAge", "RunScoredPerGame", "BA", "OBP", "SLG", "OPS", "HRperPA", "SOperPA","PAge", "ERA", "RunAllowedPerGame", "FIP", "WHIP", "H9", "BB9", "SO9", "SOperW")
    inTrain <- createDataPartition(y = finaltable[,dependent[1]], p = .7, list = FALSE)
    trainData <- finaltable[inTrain,]
    testData <- finaltable[-inTrain,]

    modelinfo <- list(train = trainData, dvar = dependent, ind = independent)

    return(modelinfo)
}

buildModel <- function(trainingSet, dependentvar, independentvar)
{
    set.seed(1234)
    indlist <- paste(independentvar, collapse = "+")
    formula <- paste(dependentvar[1], indlist, sep = "~")
    model <- train(as.formula(formula), method = "glmboost", data = trainingSet)
    return(model)
}

predictstuff <- function(model, predictorSet)
{
    set.seed(1234)
    predictions <- predict(model, newdata = predictorSet)
    return(predictions)
}

buildfinalpredstable <- function(trainyear1, trainyear2, currentyear, subset)
{
    set.seed(1234)
    modelstuff <- selectTrainData(trainyear1, trainyear2)
    model <- buildModel(modelstuff$train, modelstuff$dvar, modelstuff$ind)
    subset <- picksubset(subset)
    currentdata <- getCurrentData()
    #return(currentdata)
    dataforpred <- currentdata[which(currentdata$Tm %in% subset),]
    #return(dataforpred)
    predictions <- predictstuff(model, dataforpred)
    finaltable <- data.frame(Team = dataforpred$Tm, PredictedWins = round(predictions*162), PredictedLoss = 162-round(predictions*162), currentwins = dataforpred$W, currentloss = dataforpred$L)
    colnames(finaltable) <- c("Team", "Predicted Wins", "Predicted Losses", "Current Wins", "Current Losses")

    return(finaltable)
}

getCoefficientPlot <- function(trainyear1, trainyear2)
{
    set.seed(1234)
    modelstuff <- selectTrainData(trainyear1, trainyear2)
    model <- buildModel(modelstuff$train, modelstuff$dvar, modelstuff$ind)
    coefficients <- as.data.frame(coef(model$finalModel))
    colnames(coefficients) <- "value"
    coefficients$variable <- row.names(coefficients)
    row.names(coefficients) <- 1:length(coefficients$variable)
    coefficients <- coefficients[-1,]
    plot_ly(y = coefficients$variable,
                 x = coefficients$value,
                 name = "Coefficient Value in Model for each used Variable",
                 type = "bar",
                 orientation = 'h'
                 ) %>%
        layout(margin = list(l = 150))
    #plot<- ggplot(coefficients, aes(x = value, y = variable)) + geom_bar(stat = "identity") + coord_flip() + ggtitle("Coefficient Value in Model for each used Variable") + xlab("Coefficient Value") + ylab("Variable")
    #ggplotly(plot)

}

shinyServer(function(input, output) {



  output$MLBMap <- renderLeaflet({
      mlbmap(input$Subset)
  })
  output$AllData <- renderDataTable({
      picktable(input$Subset, input$dateSlider[1], input$dateSlider[2])
  })

  output$TeamBasedCurrentPredictions <- renderDataTable({
      buildfinalpredstable(input$dateSlider[1], input$dateSlider[2], 2017, input$Subset)
  })

  output$CurrentYear <- renderDataTable({
      pickcurrentdata(input$Subset)
  })
  output$importancePlot<- renderPlotly({
      getCoefficientPlot(input$dateSlider[1], input$dateSlider[2])
  })

})
