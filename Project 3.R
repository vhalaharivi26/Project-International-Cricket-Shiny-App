
cricket = read.csv("most_runs_in_cricket.csv")
library(shiny)
library(shinyWidgets)
ui <- fluidPage(
  #set background color of app
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),
  
  # App title
  titlePanel('Distributing Batting Averages App'),
  
  # Sidebar and main panels 
  sidebarLayout(
    sidebarPanel(
      
      #Select which variable to graph
      selectInput("selectvar", label = h3("Choose a variable to graph"), choices = list("Strike Rate" = 1, "Total Runs" = 2, "Number of Centuries" = 3, "Number of 50s" = 4, "Number of 6s Hit" = 5), selected = 1),
      
      #Select color of graph
      selectInput("colorvar", label = h3("Choose a color for the graph"), choices = list("Blue" = 1, "Red"=2, "Orange"= 3, "Purple"=4, "Green"= 5)),
      
      #Choose number of bins with slider
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      

      #Checkbox to display mean
      checkboxInput("checkbox1", label="Display mean", value=FALSE),
      
      
      
      #checkbox to display median
      checkboxInput("checkbox2", label="Display median", value=FALSE),
    ),
    
    mainPanel(
      
      plotOutput("distPlot"),
      hr(),
      p('Mean:'),
      fluidRow(column(5, verbatimTextOutput("mean"))),
      p('Median:'),
      fluidRow(column(5, verbatimTextOutput("median"))),
      
    )
    
  )
)

#PART 2
# Define server function 
server <- function(input, output) {
  #Plot output
  output$distPlot = renderPlot(
    if(input$selectvar == 1){
      if(input$colorvar == 1){
        hist(cricket$SR, breaks = input$bins, main = "Distribution of Strike Rate by player", xlab = "Strike Rate", col = "blue")
      }else if(input$colorvar == 2){
        hist(cricket$SR, breaks = input$bins, main = "Distribution of Strike Rate by player", xlab = "Strike Rate", col = "red")
      }else if(input$colorvar == 3){
        hist(cricket$SR, breaks = input$bins, main = "Distribution of Strike Rate by player", xlab = "Strike Rate", col = "orange")
      }else if(input$colorvar == 4){
        hist(cricket$SR, breaks = input$bins, main = "Distribution of Strike Rate by player", xlab = "Strike Rate", col = "purple")
      }else if(input$colorvar == 5){
        hist(cricket$SR, breaks = input$bins, main = "Distribution of Strike Rate by player", xlab = "Strike Rate", col = "green")
      }
    }
    else if(input$selectvar == 2){
      if(input$colorvar == 1){
        hist(cricket$Runs, breaks = input$bins,main = "Distribution of Total Runs by player", xlab= "Total Runs", col = "blue")
      }else if(input$colorvar == 2){
        hist(cricket$Runs, breaks = input$bins,main = "Distribution of Total Runs by player", xlab= "Total Runs", col = "red")
      }else if(input$colorvar == 3){
        hist(cricket$Runs, breaks = input$bins,main = "Distribution of Total Runs by player", xlab= "Total Runs", col = "orange")
      }else if(input$colorvar == 4){
        hist(cricket$Runs, breaks = input$bins,main = "Distribution of Total Runs by player", xlab= "Total Runs", col = "purple")
      }else if(input$colorvar == 5){
        hist(cricket$Runs, breaks = input$bins,main = "Distribution of Total Runs by player", xlab= "Total Runs", col = "green")
      }
    }
    else if(input$selectvar == 3){
      if(input$colorvar == 1){
        hist(cricket$X100,breaks = input$bins, main = "Distribution of Centuries by player", xlab = "Centuries scored", col = "blue")
      }else if(input$colorvar == 2){
        hist(cricket$X100,breaks = input$bins, main = "Distribution of Centuries by player", xlab = "Centuries scored", col = "red")
      }else if(input$colorvar == 3){
        hist(cricket$X100,breaks = input$bins, main = "Distribution of Centuries by player", xlab = "Centuries scored", col = "orange")
      }else if(input$colorvar == 4){
        hist(cricket$X100,breaks = input$bins, main = "Distribution of Centuries by player", xlab = "Centuries scored", col = "purple")
      }else if(input$colorvar == 5){
        hist(cricket$X100,breaks = input$bins, main = "Distribution of Centuries by player", xlab = "Centuries scored", col = "green")
      }
    }
    else if(input$selectvar == 4){
      if(input$colorvar == 1){
        hist(cricket$X50,breaks = input$bins, main = "Distriubtion of 50s hit by player", xlab = "50s scored", col = "blue")
      }else if(input$colorvar == 2){
        hist(cricket$X50,breaks = input$bins, main = "Distriubtion of 50s hit by player", xlab = "50s scored", col = "red")
      }else if(input$colorvar == 3){
        hist(cricket$X50,breaks = input$bins, main = "Distriubtion of 50s hit by player", xlab = "50s scored", col = "orange")
      }else if(input$colorvar == 4){
        hist(cricket$X50,breaks = input$bins, main = "Distriubtion of 50s hit by player", xlab = "50s scored", col = "purple")
      }else if(input$colorvar == 5){
        hist(cricket$X50,breaks = input$bins, main = "Distriubtion of 50s hit by player", xlab = "50s scored", col = "green")
      }
    }
    else if(input$selectvar == 5){
      if(input$colorvar == 1){
        hist(cricket$X6s,breaks = input$bins, main = "Distribution of total 6s hit by player", xlab = "6s hit", col = "blue")
      }else if(input$colorvar == 2){
        hist(cricket$X6s,breaks = input$bins, main = "Distribution of total 6s hit by player", xlab = "6s hit", col = "red")
      }else if(input$colorvar == 3){
        hist(cricket$X6s,breaks = input$bins, main = "Distribution of total 6s hit by player", xlab = "6s hit", col = "orange")
      }else if(input$colorvar == 4){
        hist(cricket$X6s,breaks = input$bins, main = "Distribution of total 6s hit by player", xlab = "6s hit", col = "purple")
      }else if(input$colorvar == 5){
        hist(cricket$X6s,breaks = input$bins, main = "Distribution of total 6s hit by player", xlab = "6s hit", col = "green")
      }
      
    }
  )
  
  #output mean
  output$mean = renderPrint({
    if(input$checkbox1 == TRUE & input$selectvar == 1){
      mean(cricket$SR, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 2) {
      mean(cricket$Runs, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 3) {
      mean(cricket$X100, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 4){
      mean(cricket$X50, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 5) {
      mean(cricket$X6s, na.rm=TRUE)}
    
  })
  
  #output median
  output$median = renderPrint({
    if(input$checkbox2 == TRUE & input$selectvar == 1){
      median(cricket$SR, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 2) {
      median(cricket$Runs, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 3) {
      median(cricket$X100, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 4){
      median(cricket$X50, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 5) {
      median(cricket$X6s, na.rm=TRUE)}
  })
  
}

#PART 3
# Run the app
shinyApp(ui = ui, server = server)



