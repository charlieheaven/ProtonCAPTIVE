require(ggplot2)
require(shiny)

#Read file
data <- read.delim("CombinedData.txt",header=T,stringsAsFactors=FALSE)

#Remove data from dataframe which does not include LET, DPC and Dose
data <- data[!isTRUE((is.na(data$LET) | is.na(data$DicentricsPerCell) | is.na(data$Dose) |is.na(data$ParticleEnergy))),]

#Function to check inputted data including number of columns and that it contains 
#values for LET, DPC and Dose, otherwise reject the data
checkdata = function(df) {
  df<- as.data.frame(df)
  if(!isTRUE(identical(ncol(df),ncol(data)))){
    return(FALSE)
  }
  else if((anyNA(df$LET)| anyNA(df$DicentricsPerCell) | anyNA(df$Dose))==TRUE | anyNA(data$ParticleEnergy)){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}
#Function which can read text or csv files as type
readfile<- function(f){
  if(f$type == "text/plain"){
    read.delim(f$datapath,header=T,stringsAsFactors=FALSE)
  }
  #Some csvs can be misidentified as excel files so it will accept these and try to read
  #as csvs
  else if(f$type == "text/csv" | f$type == "application/vnd.ms-excel"){
    read.csv(f$datapath,header=T,stringsAsFactors=FALSE)
  }
}

ui <- fluidPage(
  headerPanel(
    h2("CAPTIVE (Chromsome Aberrations from Proton Therapy for In Vitro Experiments)")),
  #Build panels and place inputs
  sidebarPanel(
    width=3,
    fluidRow(selectInput(inputId = "xval",label="Select X Variable",
              choice=c("LET","Dose"),selected="LET")),
    fluidRow(checkboxGroupInput(inputId = "partselect",label= "Particle Type",
                                choices= unique(data$ParticleType),selected=unique(data$ParticleType))),
    fluidRow(sliderInput(inputId = "doseselect",label="Dose Range to Display",
                         min=0,max=max(data$Dose),value=c(0,max(data$Dose)))),
    fluidRow(sliderInput(inputId = "LETselect",label="LET Range to Display", min=min(data$LET),
                         max=max(data$LET),value=c(min(data$LET),max(data$LET)))),
    fluidRow(sliderInput(inputId = "Energyselect",label="Particle Energy Range to Display", min=min(data$ParticleEnergy),
                         max=max(data$ParticleEnergy),value=c(min(data$ParticleEnergy),max(data$ParticleEnergy)))),
    fluidRow(checkboxGroupInput(inputId = "orgselect",label= "Organism Data to Display",
                                choices= unique(data$Animal),selected=unique(data$Animal))),
    fluidRow(checkboxGroupInput(inputId = "paperselect",label= "Paper",
                                choices=unique(data$Paper),selected=unique(data$Paper))),
    fluidRow(fileInput(inputId= "fileload",label="Upload a file into the data set: \n(Please ensure your file formatting is correct)",
              multiple=FALSE,buttonLabel="Browse",accept = c("text/plain", "text/csv","application/vnd.ms-excel"))),
    fluidRow(actionButton(inputId = "writefile", label = "Submit input file to directory"))
  ),
  mainPanel(
    width=9, 
    
    tags$head(
      tags$style(HTML("pre, table.table {
                    font-size: smaller;
                    }"))),
    fluidRow(
      #Used plot_click, could also use dblclick or hoverOpts for future developments but
      #plot click is the easiest and works for what we want.
      plotOutput("plot1", click = "plot_click")),
    fluidRow(
      #This gives the information about each point when clicked
      verbatimTextOutput("click_info")),
    fluidRow(
      #This is used to alert the user a file has been added or saved
      verbatimTextOutput("Files"))
    )
  )

server <- function(input, output,session) {
  #This sets up an empty data frame which will contain our selected point from the plot click
  selected_points <- data[0, ]
  #Names of each column the app will display on plot click
  data_names<- c("Paper","LET","Dose","Cells Scored","Dicentrics per Cell",
                 "No of Chromosomes","Particle Type","Energy of Particle","Organism",
                 "Cell Type","Cell Line","Gender")
  #Units for each point, note for greek letters, unicode must be used
  data_units<- c(""," KeV/\u03BCm"," Gy","","","","", "MeV",
                 "","","","")
  observe({
    #If no file, use data uploaded
    if(is.null(input$fileload)){
      data <= data
    }
    #Check correct file type
    else if(!isTRUE(input$fileload$type %in% c("text/plain", "text/csv", 
            "application/vnd.ms-excel"))){
      data <= data
    }
    else {
      #read in file
      newdata<- readfile(input$fileload)
      newdata<- as.data.frame(newdata)
      #check data is correct once read in
      if(checkdata(newdata) == TRUE){
      #Match column names
        if(!isTRUE(identical(colnames(data),colnames(newdata)))){
          colnames(newdata) <- colnames(data)
          }
        #Add in new data from file and update option buttons
        data2<-rbind(data,newdata)
        data <<- data <- data2
        updateCheckboxGroupInput(session, "paperselect", choices = unique(data$Paper),
                                 selected=unique(data$Paper))
        updateCheckboxGroupInput(session,inputId = "partselect",label= "Particle Type",
                                choices= unique(data$ParticleType),selected=unique(data$ParticleType))
        updateSliderInput(session,inputId = "doseselect",label="Dose Range to Display",
                          min=min(data$Dose),max=max(data$Dose),value=c(min(data$Dose),max(data$Dose)))
        updateSliderInput(session,inputId = "Energyselect",label="Particle Energy Range to Display", 
                          min=min(data$ParticleEnergy), max=max(data$ParticleEnergy),
                          value=c(min(data$ParticleEnergy),max(data$ParticleEnergy)))
        updateSliderInput(session,inputId = "LETselect",label="LET Range to Display", min=min(data$LET),
                          max=max(data$LET),value=c(min(data$LET),max(data$LET)))
        updateCheckboxGroupInput(session,inputId = "orgselect",label= "Organism Data to Display",
                                 choices= unique(data$Animal),selected=unique(data$Animal))
        observeEvent(input$writefile,{
          #Creates a time stamp for saving files under a file name of current time
          Date <- Sys.Date()
          Time <- paste0(format(Sys.time(), "%d-%b-%Y %H.%M"))
          Name <- paste0(Date,"_",Time,".csv")
          if(is.null(input$fileload)){
            return(NULL)
          }
          else{
            write.table(newdata,Name,sep=",")
            output$Files<- renderPrint({
              paste("Data saved in ",getwd()," as ",Name)
          })
          }
        })
      }
    }
  })
  #Store plot click into the selected points data frame so that we can do stuff with it
  selected <- reactive({
    #add clicked
    selected_points <<- rbind(selected_points,nearPoints(data, input$plot_click))
    #Remove old clicks so only one circled point appears at a time
    if(nrow(selected_points)>1){
      selected_points<<- selected_points[-c(1),]
    }
    #Initialises this at the first line in the data file 
    else if(nrow(selected_points)==0){
      selected_points<- data[1,]
    }
    return(selected_points)
  })
  output$plot1 <- renderPlot({
    #Selects only data according to the user selections
    data<- data[data$Animal %in% input$orgselect,]
    data<- data[data$ParticleType %in% input$partselect,]
    data<- data[data$ParticleEnergy >= input$Energyselect[1] & data$ParticleEnergy <= input$Energyselect[2],]
    data<- data[data$Paper %in% input$paperselect,]
    data<- data[data$Dose >= input$doseselect[1] & data$Dose <= input$doseselect[2],]
    data<- data[data$LET >= input$LETselect[1] & data$LET <= input$LETselect[2],]
    #If x value is selected as LET or dose, change the graph
    if(input$xval=="LET"){
      ggplot(data,aes(x=LET,y=DicentricsPerCell))+
        geom_point(size=3, aes(colour=Dose,shape=Paper))+
        scale_colour_gradientn(colours=rainbow(7))+
        scale_shape_manual(values = c(15,16,17,19,0,1,2,3,4,5))+
        #This is the selection of the points based on plot click
        geom_point(data = selected(), colour = "red", shape=1, size = 8)
    }
    else if(input$xval=="Dose"){
    ggplot(data,aes(x=Dose,y=DicentricsPerCell))+
      geom_point(size=3, aes(colour=LET,shape=Paper))+
      scale_colour_gradientn(colours=rainbow(7))+
      scale_shape_manual(values = c(15,16,17,19,0,1,2,3,4,5))+
      geom_point(data = selected(), colour = "red", shape=1, size = 8)
  }})
  #Print plot click info
  output$click_info<- renderPrint({
    paste0(data_names,":  ",selected(),data_units)
  })
  #To check files have been uploaded
  output$Files<- renderPrint({
    if(is.null(input$fileload)){
      paste("No new file uploaded")
    }
    #If file has failed to upload warn user
    else if(checkdata(readfile(input$fileload))==FALSE){
      paste(input$fileload$name, " cannot be added to dataframe. Please check file and try again")
    }
    else {
      #If file has uploaded tell user
      paste(input$fileload$name, " added to data frame")
    }
  })
}

shinyApp(ui, server)