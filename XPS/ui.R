
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("XPS visualizer and converter"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput('file_to_open', 'Choose .xy File', accept=c('text/csv','text/comma-separated-values,text/plain', '.csv', '.xy', '.txt')),
      radioButtons("entype", "Set desired energy type:",
                   c("Kinetic Energy" = "ke",
                     "Binding Energy" = "be"),
                   inline = TRUE),
      numericInput("exe", "Excitation energy, eV:", 770, min = 0, max = 10000),
      actionButton("go", "Upload!"),
      uiOutput("range_slider"),
      h5("The app was created by Nikolay Falaleev"),
      h5(tags$i("e-mail: falaleevn@yandex.ru"))
    ),
    
    mainPanel(
       plotOutput("distPlot"),
       plotOutput("sumPlot"),
       downloadButton('download_all', 'Download all spectra'),
       downloadButton('download_roi', 'Download ROI spectra'),
       downloadButton('download_sum', 'Download cumulative spectrum')
    )
  )
))
