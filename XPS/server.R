library(shiny)
library(ggplot2)
library(reshape2)
library(scatterplot3d)


shinyServer(function(input, output) {
  #Define default values and useful functions
  data_spec <- NULL
  number_of_rows <- 1
  number_of_columns <- 1
  xlabel <- "Kinetic Energy (K.E.), eV"
  sum <- 0
  max_rows <- 0
  decs <- "," #default ecimal separator
  #Function for .xy file reading and parsing
  read_file <- function(filename){
    d <- read.table(filename, sep=" ", fill=FALSE, strip.white=TRUE)
    df <- data.frame(d[,1], d[,3])
    number_of_rows2 <- 1
    number_of_columns2 <- 1
    while((df[number_of_rows2+1,1]-df[number_of_rows2,1])>0){
      number_of_rows2 <- number_of_rows2+1
    }
    number_of_columns2 <- dim(df)[1]/number_of_rows2
    if(input$entype=="be"){
      xlabel <<- "Binding Energy (B.E.), eV"
      data_spec <- data.frame(input$exe-df[1:number_of_rows2,1])
    }
    else{
      xlabel <<- "Kinetic Energy (K.E.), eV"
      data_spec <- data.frame(df[1:number_of_rows2,1])
    }
    colnames(data_spec)[1] <- xlabel
    pointer <- 1
    for (i in c(2:(number_of_columns2+1))){
      data_spec[1:number_of_rows2,i] <- df[pointer:(pointer+number_of_rows2-1),2]
      pointer <- pointer+number_of_rows2
      colnames(data_spec)[i] <- paste("Curve ", toString(i-2))
    }
    return(data_spec)
  }
  in_data <- reactive({
    inFile <- input$file_to_open
    if (is.null(inFile))
      return(NULL)
    #Read input file
    data_spec <<- read_file(inFile$datapath)
    number_of_rows <<- dim(data_spec)[1]
    number_of_columns <<- dim(data_spec)[2]-1
    #Render interactive UI elements
    output$range_slider <- renderUI({
      sliderInput("Range_Slider",label="Choose the ROI",min = 1, max = number_of_columns,
                  value = c(5,number_of_columns-5), step=1)
    })
    output$angle_slider <- renderUI({
      sliderInput("angle", label = "Rotate angle of the 3D plot", min=0, max=90, value = 75, step=1)
    })
    output$help_text <- renderText( "Upload a batch of files in order to process them by subsampling the ROI and computing cumulative spectra. Use \"Process!\" button to download a .csv file with the spectra.")
    output$batchfiles <- renderUI({
      fileInput('files_to_open', 'Choose .xy files to process with the selected above settings', accept=c('.xy'), multiple = TRUE)
    })
    output$preview_button <- renderUI({
      actionButton("preview", "Preview")
    })
    output$process_button <- renderUI({
      downloadButton("process", "Process!")
    })
    if(input$entype=="be"){
      xlabel <<- "Binding Energy (B.E.), eV"
    }
    else{
      xlabel <<- "Kinetic Energy (K.E.), eV"
    }
  })
observeEvent(input$entype,{
  in_data()
})
observeEvent(input$go,{
  output$distPlot <- renderPlot({
    in_data()
    colMax <- function(data) sapply(data, max, na.rm = TRUE)
    rowMax <- function(data) do.call(pmax, data)
    data_spec_sm <- data_spec
    data_spec_sm[,(2:(input$Range_Slider[1]))] <- 0
    data_spec_sm[,((input$Range_Slider[2]+2):(number_of_columns+1))] <- 0
    ang <- input$angle
    x <- data_spec[,1]
    xlimit <- range(data_spec[,1])
    #Draw 3D plot
    f <- function(i, j) { r <- data_spec_sm[i,j] }
    z <- f(1:number_of_rows,2:(number_of_columns+1))
    y <- seq(1,number_of_columns,length=number_of_rows)
    maxcol=col(data_spec[, 2:(number_of_columns+1)])[data_spec[, 2:(number_of_columns+1)]==max(data_spec[, 2:(number_of_columns+1)])]
    za <- data_spec[,maxcol]
    lab <- seq(1,length(x), length(x)/5)
    ma.x <- matrix(x,nrow=length(x),ncol=1)
    if(input$entype=="be"){
      x=rev(x)
      ma.x <- rev(matrix(x,nrow=length(x),ncol=1))
    }
    scatterplot3d(x, y, za, type = "n", box = FALSE,
                 angle = ang, xlim=xlimit, x.ticklabs = NA, xlab = xlabel, ylab = "Channel #",
                 zlab = "Cps", main = paste("Plot of the input file ", toString(input$file_to_open[1])))->sp
    text(sp$xyz.convert(ma.x[lab], rep(min(y), length(lab)), rep(min(za) - (max(za)-min(za))/25,length(lab))), labels=paste(x[lab], sep=" "), cex=.8)
    for (i in c(2:(number_of_columns+1))){
      sp$points3d(x, y = rep.int(i-1,number_of_rows), z=data_spec_sm[,i], type = "l", lwd=1, xlim=xlimit)
    }
    sp$points3d(x, y=rep.int(input$Range_Slider[1], number_of_rows), z=data_spec_sm[, (input$Range_Slider[1]+1)],
                type = "l", lwd=1, col="red", xlim=xlimit)
    sp$points3d(x, y=rep.int(input$Range_Slider[2], number_of_rows), z=data_spec_sm[, (input$Range_Slider[2]+1)],
                type = "l", lwd=1, col="red", xlim=xlimit)
    max_z <- colMax(data_spec_sm[,2:(number_of_columns+1)])
    sp$points3d(x=rep.int(range(data_spec[,1])[1], number_of_columns),
                y=seq(1,number_of_columns,length=number_of_columns), z=max_z,
                type = "l", lwd=1, col="blue", xlim=xlimit)
    max_zp <- rowMax(data_spec_sm[,2:(number_of_columns+1)])
    sp$points3d(x, y=rep.int(number_of_columns, number_of_rows),z=max_zp,
                type = "l", lwd=1, col="blue", xlim=xlimit)
  })
  #Render cumulative plot
  output$sumPlot <- renderPlot({
    sum <<- rowSums(data_spec[c(colnames(data_spec)[(input$Range_Slider[1]+1):(input$Range_Slider[2]+1)])])/1000
    g1 <- ggplot(data_spec, aes(x=data_spec[,1], y=sum))+geom_line()+xlab(xlabel)+ylab("Cps, 1000*")+ggtitle("Cumulative spectrum of the ROI")
    small_data_spec <- melt(data_spec[,c(1,(input$Range_Slider[1]+1):(input$Range_Slider[2]+1))], id.vars = 1)
    if(input$entype=="be"){
      g1 <- g1+scale_x_reverse()
    }
    g1
  })
})
#Set decimal separator
observeEvent(input$septype, {
  if (input$septype=="c"){
    decs <<- ","
  }
  else{
    decs <<- "."
  }
})
#prepare downloadable files  
output$download_all <- downloadHandler(
  filename = function() { paste(gsub( ".xy", "", toString(input$file_to_open[1] )), '_all.csv', sep='') },
  content = function(file) {
    colnames(data_spec)[1] <- xlabel
    write.table(data_spec, file,row.names=FALSE, sep=";", dec=decs)
  }
)
output$download_sum <- downloadHandler(
  filename = function() { paste(gsub( ".xy", "", toString(input$file_to_open[1] )), '_sum.csv', sep='') },
  content = function(file) {
    sumdf <- data.frame(data_spec[,1], sum)
    colnames(sumdf)[1] <- xlabel
    colnames(sumdf)[2] <- "cps, 1000*"
    write.table(sumdf, file,row.names=FALSE, sep=";", dec=decs)
  }
)
output$download_roi <- downloadHandler(
  filename = function() { paste(gsub( ".xy", "", toString(input$file_to_open[1] )), '_roi.csv', sep='') },
  content = function(file) {
    roidf <- data_spec[,c(1,(input$Range_Slider[1]+1):(input$Range_Slider[2]+1))]
    colnames(roidf)[1] <- xlabel
    write.table(roidf, file,row.names=FALSE, sep=";", dec=decs)
  }
)
observeEvent(input$preview, {
  #Render preview for multiple .xy file input
  rows <- (length(input$files_to_open[,1])%/%2+length(input$files_to_open[,1])%%2)
  output$preview_plot <- renderPlot({
    par(mfrow=c(rows,3))
    for (i in 1:length(input$files_to_open[,1])){
      inFile <- input$files_to_open[[i, 'name']]
      data_spec <- read_file(input$files_to_open[[i, 'datapath']])
      x <- data_spec[,1]
      y <- rowSums(data_spec[c(colnames(data_spec)[(input$Range_Slider[1]+1):(input$Range_Slider[2]+1)])])/1000
      xlimit <- range(x)
      if(input$entype=="be"){
        xlimit <- rev(xlimit)
      }
      plot(x, y, xlim = xlimit, main=inFile[1], xlab=xlabel, ylab = "Cps, 1000*", type="l")
    }
  }, width = 720, height = rows*240 ) #
})
#Downloader for the batch processed files
output$process <- downloadHandler(
  filename = function() {
    paste(gsub( ".xy", "", toString(input$files_to_open[[1, 'name']])),
          '_seria_', toString(input$Range_Slider[1]), '_',
          toString(input$Range_Slider[2]), '.csv', sep='')
    },
  content = function(file) {
    ldf <- data.frame(a=NA, b=NA)[numeric(0), ]
    max_rows <- 0
    for (i in 1:length(input$files_to_open[,1])){ 
      inFile <- input$files_to_open[[i, 'name']]
      data_spec <- read_file(input$files_to_open[[i, 'datapath']])
      if (dim(data_spec)[1] > max_rows){
        max_rows <- dim(data_spec)[1]
      }
      x <- data_spec[,1]
      y <- rowSums(data_spec[c(colnames(data_spec)[(input$Range_Slider[1]+1):(input$Range_Slider[2]+1)])])
      ldf_sm <- data.frame(matrix(c(rep.int(NA,max_rows*2)), nrow=max_rows, ncol=2))
      if (length(x) < max_rows){
        x <- c(x, rep(c(rep.int(NA,(max_rows-length(x))*2))))
        y <- c(y, rep(c(rep.int(NA,(max_rows-length(y))*2))))
      }
      ldf_sm[1:max_rows, 1:2] <- data.frame(cbind(x,y))
      colnames(ldf_sm) <- c(paste(xlabel, sep = " "), toString(inFile))
      if (length(ldf[,1])<max_rows){
        df_na <- data.frame(matrix(c(rep.int(NA,length(ldf))), nrow=(max_rows-length(ldf[,1])), ncol=length(ldf)))
        colnames(df_na) <- colnames(ldf)
        ldf <- rbind(ldf, df_na)
      }
      if (i>1){
        ldf <- cbind(ldf, ldf_sm)
      }
      else{
        colnames(ldf)[1:2] <- c(paste(xlabel, sep = " "), toString(input$files_to_open[[1, 'name']]))
        ldf[1:max_rows, 1:2] <- ldf_sm
      }
    }
    write.table(ldf[,1:length(ldf)], file,row.names=FALSE, sep=";", dec=decs, na="")
  })
})


