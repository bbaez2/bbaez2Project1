# Brian Baez
# CS 424 - UIC Spring 2021

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

# read data from csv file
data <- read.csv("annual_generation_state.csv")

# edit column names to be more simple
colnames(data)[3] <- "TYPE"
colnames(data)[4] <- "SOURCE"
colnames(data)[5] <- "GENERATION"

# remove commas from generation values
data$GENERATION <-gsub(",", "", data$GENERATION)

# change generation values to numbers
data$GENERATION <- as.numeric(as.character(data$GENERATION)) / 1000

# capitalize all state names
data$STATE <- toupper(data$STATE)

# remove any empty rows with no state value
data <- subset(data, data$STATE != "  ")

# change state values to factors
data$STATE <- as.factor(data$STATE)

# remove all generatin values that are negative
data <- subset(data, data$GENERATION >= 0)

# remove any sources that are "Other", "Other Biomass", "Other Gases", and "Pumped Storage"
data <- subset(data, data$SOURCE != "Other" & data$SOURCE != "Other Biomass" & data$SOURCE != "Other Gases" & data$SOURCE != "Pumped Storage")

# remove any source levels not used
data$SOURCE <- droplevels(data$SOURCE)

# shorten the source names
levels(data$SOURCE)[2] <- "Geo"
levels(data$SOURCE)[3] <- "Hydro"
levels(data$SOURCE)[4] <- "Gas"
levels(data$SOURCE)[7] <- "Solar"
levels(data$SOURCE)[10] <- "Wood"

# create list of sources, omitting "Total"
sources <- levels(data$SOURCE)
sources <- sources[-8]

# data frame needed for table values
dataTotals <- subset(data, data$STATE == "US-TOTAL" & data$TYPE == "Total Electric Power Industry" & data$SOURCE != "Total")

# create shiny dashboard
ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = "Power and the Passion"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
    # checkboxes for sources
    checkboxGroupInput("sourceSelection", "Select the sources to visualize", choices=sources),
    checkboxInput("selectAll", label="Select/Deselect all", value = TRUE)
  ),
  dashboardBody(
    fluidRow(
      column(5,
        fluidRow(
          # stacked bar chart showing amount of energy source per year
          box(title="Energy Production by Source From 1990 to 2019", solidHeader = TRUE, status="primary",
          width=10, plotOutput("bar1", height=225))
        ),
        fluidRow(
          # stacked bar chart showing % of energy source per year
          box(title="Energy Production Percentage by Source From 1990 to 2019", solidHeader = TRUE, status="primary",
          width=10, plotOutput("bar2", height=225))
        )
      ),
      
      column(5,
        fluidRow(
          # line chart showing amount of energy source per year
          box(title="Energy Production by Source From 1990 to 2019", solidHeader = TRUE, status="warning",
          width=10, plotOutput("line1", height=225))
        ),
        fluidRow(
          # table showing amount of energy source per year
          box(title="Energy Production by Source From 1990 to 2019", solidHeader = TRUE, status = "danger",
          width = 12, dataTableOutput("table1", height = 225))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # used for select/deselect all checkboxes
  observe({
    updateCheckboxGroupInput(session, "sourceSelection", choices = sources,
    selected = if(input$selectAll) sources)
  })

  # create stacked bar chart for amount of energy
  output$bar1 <- renderPlot({
    ggplot(subset(data, data$SOURCE != "Total" & data$STATE == "US-TOTAL"
    & data$TYPE == "Total Electric Power Industry"), aes(fill=SOURCE, x=YEAR, y=GENERATION)) +
    geom_bar(stat="identity") + xlab("Year") + ylab("Generation (Gigawatthours")
  })
  
  # create stacked bar chart for % amount of energy
  output$bar2 <- renderPlot({
    ggplot(subset(data, data$SOURCE != "Total" & data$STATE == "US-TOTAL"
    & data$TYPE == "Total Electric Power Industry"), aes(fill=SOURCE, x=YEAR, y=GENERATION)) +
    geom_bar(position="fill", stat="identity") + xlab("Year") + ylab("Generation (%)")
  })
  
  # create line chart for amount of energy
  output$line1 <- renderPlot({
    ggplot(subset(data, data$SOURCE != "Total" & data$STATE == "US-TOTAL" 
    & data$TYPE == "Total Electric Power Industry"), aes(x=YEAR, y=GENERATION, color=SOURCE)) +
    geom_line() + geom_point() + xlab("Year") + ylab("Generation (Gigawatthours)")
  })

  # create table for amount of energy
  output$table1 <- DT::renderDataTable({
    dataTotals
  }
  )
}

shinyApp(ui, server)