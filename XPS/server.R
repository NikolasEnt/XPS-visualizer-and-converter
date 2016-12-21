
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
  
  in_data<-reactive({
    inFile <- input$file_to_open
    
    if (is.null(inFile))
      return(NULL)
    
    d = read.table(inFile$datapath, sep=" ", fill=FALSE, strip.white=TRUE)
    df<-data.frame(d[,1], d[,3])
    
    while((df[number_of_rows+1,1]-df[number_of_rows,1])>0){
      number_of_rows<<-number_of_rows+1
    }
    number_of_columns<<-dim(df)[1]/number_of_rows
    
    output$range_slider <- renderUI({
      sliderInput("Range_Slider",label="Choose the ROI",min = 1,max = number_of_columns,value = c(5,number_of_columns-5), step=1)
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

  g<-eventReactive(input$go, {
    in_data()
    data_spec.melted<-melt(data_spec, measure.vars = colnames(data_spec)[2:(number_of_columns+1)])
    
    ggplot(data_spec.melted, aes(data_spec.melted[,1], data_spec.melted[,2], fill = value))+geom_tile()+
    scale_fill_gradient(low = "black", high = "white", limit = c(0,max(data_spec[,2:(number_of_columns+1)])), name="cps")+
    xlab(xlabel)+ylab("Curve")+xlim(data_spec[1,1], data_spec[number_of_rows,1])+theme(axis.text.y=element_blank())
  })
  
observeEvent(input$entype,{
  in_data()
})


observeEvent(input$go,{
  output$distPlot <- renderPlot({
    g()+geom_hline(aes(yintercept = input$Range_Slider[1]),colour="red")+
      geom_hline(aes(yintercept = input$Range_Slider[2]),colour="red")
  })
  
  output$sumPlot <- renderPlot({
    sum<<-rowSums(data_spec[c(colnames(data_spec)[(input$Range_Slider[1]+1):(input$Range_Slider[2]+1)])])/1000
    g1<-ggplot(data_spec, aes(x=data_spec[,1], y=sum))+geom_line()+xlab(xlabel)+ylab("cps, 1000*")+ggtitle("Cumulative spectrum of the ROI")
    
    small_data_spec<-melt(data_spec[,c(1,(input$Range_Slider[1]+1):(input$Range_Slider[2]+1))], id.vars = 1)
    g2<-ggplot(small_data_spec, aes(x=small_data_spec[,1], y=value, group=small_data_spec[,2]))+
      geom_line()+xlab(xlabel)+ylab("cps")+ggtitle("Individual spectra of the ROI")
    if(input$entype=="be"){
      g1 <- g1+scale_x_reverse()
      g2 <- g2+scale_x_reverse()
    }
    grid.arrange(g1, g2, ncol=2)
  })
  
})

  
  
output$download_all <- downloadHandler(
  
  filename = function() { paste(gsub( ".xy", "", toString(input$file_to_open[1] )), '_all.csv', sep='') },
  content = function(file) {
    colnames(data_spec)[1]<-xlabel
    write.table(data_spec, file,row.names=FALSE, sep=";", dec=",")
  }
)

output$download_sum <- downloadHandler(
  
  filename = function() { paste(gsub( ".xy", "", toString(input$file_to_open[1] )), '_sum.csv', sep='') },
  content = function(file) {
    
    sumdf <- data.frame(data_spec[,1], sum)
    colnames(sumdf)[1]<-xlabel
    colnames(sumdf)[2]<-"cps, 1000*"
    write.table(sumdf, file,row.names=FALSE, sep=";", dec=",")
  }
)

output$download_roi <- downloadHandler(
  
  filename = function() { paste(gsub( ".xy", "", toString(input$file_to_open[1] )), '_roi.csv', sep='') },
  content = function(file) {
    
    roidf <- data_spec[,c(1,(input$Range_Slider[1]+1):(input$Range_Slider[2]+1))]
    colnames(roidf)[1]<-xlabel
    write.table(roidf, file,row.names=FALSE, sep=";", dec=",")
  }
)
  
})
