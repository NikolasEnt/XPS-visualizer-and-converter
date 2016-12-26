
library(shiny)
library(ggplot2)
library(reshape2)
require(gridExtra)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  data_spec <- NULL
  number_of_rows <- 1
  number_of_columns <- 1
  xlabel <- "Kinetic Energy (K.E.), eV"
  sum <- 0
  max_rows<-0
  decs <- ","
  
  in_data<-reactive({
    inFile <- input$file_to_open
    
    if (is.null(inFile))
      return(NULL)
    
    d = read.table(inFile$datapath, sep=" ", fill=FALSE, strip.white=TRUE)
    df<-data.frame(d[,1], d[,3])
    # number_of_rows <- 1
    # number_of_columns <- 1
    while((df[number_of_rows+1,1]-df[number_of_rows,1])>0){
      number_of_rows<<-number_of_rows+1
    }
    number_of_columns<<-dim(df)[1]/number_of_rows
    
    output$range_slider <- renderUI({
      sliderInput("Range_Slider",label="Choose the ROI",min = 1,max = number_of_columns,value = c(5,number_of_columns-5), step=1)
    })
    output$angle_slider <- renderUI({
      sliderInput("angle", label = "Rotate ange of the 3D plot", min=0, max=90, value = 75, step=1)
    })
    output$help_text <- renderText( "Upload a batch of files in order to process them by subsampling the ROI and computing cumulative spectra")
    
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
      data_spec<<-data.frame(input$exe-df[1:number_of_rows,1])
    }
    else{
      xlabel <<- "Kinetic Energy (K.E.), eV"
      data_spec<<-data.frame(df[1:number_of_rows,1])
    }
    
    
    colnames(data_spec)[1]<-xlabel
    pointer=1
    for (i in c(2:(number_of_columns+1))){
      data_spec[1:number_of_rows,i]<<-df[pointer:(pointer+number_of_rows-1),2]
      pointer<-pointer+number_of_rows
      colnames(data_spec)[i]<<-paste("Curve ", toString(i-2))
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
    
    data_spec_sm<-data_spec
    data_spec_sm[,(2:(input$Range_Slider[1]))] <- 0
    data_spec_sm[,((input$Range_Slider[2]+2):(number_of_columns+1))] <- 0
    
    ang <- input$angle
    cpr <- number_of_columns
    if(input$entype=="be"){
      #ang <- ang+90
      #cpr <- 1
    }
    
    x=data_spec[,1]
    xlimit=range(data_spec[,1])
    
    
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
                 angle = ang, xlim=xlimit, x.ticklabs = NA, xlab = xlabel, ylab = "Channel #", zlab = "Cps", main = "Plot of the input file")->sp
    text(sp$xyz.convert(ma.x[lab], rep(min(y),length(lab)), rep(min(za) - (max(za)-min(za))/25,length(lab))), labels=paste(x[lab], sep=" "), cex=.8)
    for (i in c(2:(number_of_columns+1))){
      sp$points3d(x, y = rep.int(i-1,number_of_rows), z=data_spec_sm[,i], type = "l", lwd=1, xlim=xlimit)
    }
    sp$points3d(x, y=rep.int(input$Range_Slider[1],number_of_rows),z=data_spec_sm[,(input$Range_Slider[1]+1)],
                type = "l", lwd=1, col="red", xlim=xlimit)
    sp$points3d(x, y=rep.int(input$Range_Slider[2],number_of_rows),z=data_spec_sm[,(input$Range_Slider[2]+1)],
                type = "l", lwd=1, col="red", xlim=xlimit)
    max_z <- colMax(data_spec_sm[,2:(number_of_columns+1)])
    sp$points3d(x=rep.int(range(data_spec[,1])[1], number_of_columns), y=seq(1,number_of_columns,length=number_of_columns),z=max_z,
                type = "l", lwd=1, col="blue", xlim=xlimit)
    max_zp <- rowMax(data_spec_sm[,2:(number_of_columns+1)])
    sp$points3d(x, y=rep.int(cpr,number_of_rows),z=max_zp,
                type = "l", lwd=1, col="blue", xlim=xlimit)
  })
  
  output$sumPlot <- renderPlot({
    sum<<-rowSums(data_spec[c(colnames(data_spec)[(input$Range_Slider[1]+1):(input$Range_Slider[2]+1)])])/1000
    g1<-ggplot(data_spec, aes(x=data_spec[,1], y=sum))+geom_line()+xlab(xlabel)+ylab("Cps, 1000*")+ggtitle("Cumulative spectrum of the ROI")
    
    small_data_spec<-melt(data_spec[,c(1,(input$Range_Slider[1]+1):(input$Range_Slider[2]+1))], id.vars = 1)

    if(input$entype=="be"){
      g1 <- g1+scale_x_reverse()
    }
    g1
  })
  
})

observeEvent(input$septype, {
  if (input$septype=="c"){
    decs<<-","
  }
  else{
    decs<<-"."
  }
})
  
output$download_all <- downloadHandler(
  
  filename = function() { paste(gsub( ".xy", "", toString(input$file_to_open[1] )), '_all.csv', sep='') },
  content = function(file) {
    
    colnames(data_spec)[1]<-xlabel
    write.table(data_spec, file,row.names=FALSE, sep=";", dec=decs)
  }
)

output$download_sum <- downloadHandler(

  filename = function() { paste(gsub( ".xy", "", toString(input$file_to_open[1] )), '_sum.csv', sep='') },
  content = function(file) {

    sumdf <- data.frame(data_spec[,1], sum)
    colnames(sumdf)[1]<-xlabel
    colnames(sumdf)[2]<-"cps, 1000*"
    write.table(sumdf, file,row.names=FALSE, sep=";", dec=decs)
  }
)

output$download_roi <- downloadHandler(
  
  filename = function() { paste(gsub( ".xy", "", toString(input$file_to_open[1] )), '_roi.csv', sep='') },
  content = function(file) {

    roidf <- data_spec[,c(1,(input$Range_Slider[1]+1):(input$Range_Slider[2]+1))]
    colnames(roidf)[1]<-xlabel
    write.table(roidf, file,row.names=FALSE, sep=";", dec=decs)
  }
)


observeEvent(input$preview, { 

  output$preview_plot<-renderPlot({
    par(mfrow=c((length(input$files_to_open[,1])%/%2+length(input$files_to_open[,1])%%2),3))
    for (i in 1:length(input$files_to_open[,1])){
      inFile <- input$files_to_open[[i, 'name']]
      d = read.table(input$files_to_open[[i, 'datapath']], sep=" ", fill=FALSE, strip.white=TRUE)
      df<-data.frame(d[,1], d[,3])
      number_of_rows2 <- 1
      number_of_columns2 <- 1
      while((df[number_of_rows2+1,1]-df[number_of_rows2,1])>0){
        number_of_rows2<-number_of_rows2+1
      }
      number_of_columns2<-dim(df)[1]/number_of_rows2
      
      if(input$entype=="be"){
        xlabel <- "Binding Energy (B.E.), eV"
        data_spec<-data.frame(input$exe-df[1:number_of_rows2,1])
      }
      else{
        xlabel <- "Kinetic Energy (K.E.), eV"
        data_spec<-data.frame(df[1:number_of_rows2,1])
      }
      colnames(data_spec)[1]<-xlabel
      pointer=1
      for (i in c(2:(number_of_columns2+1))){
        data_spec[1:number_of_rows2,i]<-df[pointer:(pointer+number_of_rows2-1),2]
        pointer<-pointer+number_of_rows2
        colnames(data_spec)[i]<<-paste("Curve ", toString(i-2))
      }
      x=data_spec[,1]
      y<-rowSums(data_spec[c(colnames(data_spec)[(input$Range_Slider[1]+1):(input$Range_Slider[2]+1)])])/1000
      xlimit=range(x)
      if(input$entype=="be"){
        xlimit<-rev(xlimit)
      }
      plot(x, y, xlim=xlimit, main=inFile[1], xlab=xlabel, ylab = "Cps, 1000*", type="l")
    }
  
    
   
  })
  })

output$process <- downloadHandler(
  
  filename = function() {
    
    paste(gsub( ".xy", "", toString(input$files_to_open[[1, 'name']])), '_seria.csv', sep='')
    },
  content = function(file) {
    ldf<-data.frame()
    max_rows<-0
    x_list<-list()
    y_list<-list()
    for (i in 1:length(input$files_to_open[,1])){ 
      inFile <- input$files_to_open[[i, 'name']]
      d = read.table(input$files_to_open[[i, 'datapath']], sep=" ", fill=FALSE, strip.white=TRUE)
      df<-data.frame(d[,1], d[,3])
      number_of_rows2 <- 1
      number_of_columns2 <- 1
      while((df[number_of_rows2+1,1]-df[number_of_rows2,1])>0){
        number_of_rows2<-number_of_rows2+1
      }
      number_of_columns2<-dim(df)[1]/number_of_rows2
      if (number_of_rows2>max_rows){
        max_rows<-number_of_rows2
      }
      if(input$entype=="be"){
        xlabel <- "Binding Energy (B.E.), eV"
        data_spec<-data.frame(input$exe-df[1:number_of_rows2,1])
      }
      else{
        xlabel <- "Kinetic Energy (K.E.), eV"
        data_spec<-data.frame(df[1:number_of_rows2,1])
      }
      colnames(data_spec)[1]<-xlabel
      pointer=1
      for (i in c(2:(number_of_columns2+1))){
        data_spec[1:number_of_rows2,i]<-df[pointer:(pointer+number_of_rows2-1),2]
        pointer<-pointer+number_of_rows2
        colnames(data_spec)[i]<<-paste("Curve ", toString(i-2))
      }
      x_list[i]<-data_spec[,1]
      y_list[i]<-rowSums(data_spec[c(colnames(data_spec)[(input$Range_Slider[1]+1):(input$Range_Slider[2]+1)])])/1000
    }
    
    for (i in 1:length(input$files_to_open[,1])){
      #x_list[[i]] <- c(x_list[[i]], rep(NA, max_rows - length(x_list[[i]])))
      #y_list[[i]] <- c(y_list[[i]], rep(NA, max_rows - length(y_list[[i]])))
      print((x_list[i]))
      print((y_list[i]))
      #ldf<-cbind(ldf, x_list[[i]], y_list[[i]])
    }
    
    # roidf <- data_spec[,c(1,(input$Range_Slider[1]+1):(input$Range_Slider[2]+1))]
    # colnames(roidf)[1]<-xlabel
    write.table(ldf, file,row.names=FALSE, sep=";", dec=decs)
  }
)

  
})


