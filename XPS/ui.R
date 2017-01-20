library(shiny)
# Define UI for the application
shinyUI(fluidPage(
  # Application title
  titlePanel("XPS visualizer and converter"),
  h5(tags$i("ver. 1.1.0")),
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      fileInput('file_to_open', 'Choose .xy File', accept=c('.xy')), #file input
      radioButtons("entype", "Set desired energy type:",
                   c("Kinetic Energy" = "ke",
                     "Binding Energy" = "be"), #Energy type select radiobutton
                   inline = TRUE),
      numericInput("exe", "Excitation energy, eV:", 770, min = 0, max = 10000, step=0.1),
      actionButton("go", "Upload!"),
      uiOutput("range_slider"),
      uiOutput("angle_slider"),
      uiOutput("help_text"),
      uiOutput("batchfiles"),
      fluidRow(
        column(6, uiOutput("preview_button")),
        column(6, uiOutput("process_button"))
      ),
      h5("The app was created by Nikolay Falaleev"),
      h5(tags$i("e-mail: falaleevn@yandex.ru")),
      helpText(a("See source code",href="https://github.com/NikolasEnt/XPS-visualizer-and-converter"))
    ),
    #Main window with plots
    mainPanel(
       plotOutput("distPlot"), #3D plot
       plotOutput("sumPlot"), #Cumulative plot
       radioButtons("septype", "Set desired decimal separator:",
                    c("comma (,)" = "c",
                      "period (.)" = "p"),
                    inline = TRUE),
       downloadButton('download_sum', 'Download cumulative spectrum'),
       downloadButton('download_roi', 'Download ROI spectra'),
       downloadButton('download_all', 'Download all spectra'),
       plotOutput("preview_plot")
    )
  )
))
