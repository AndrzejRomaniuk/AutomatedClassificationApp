################
## LIBRARIES  ##
################
#### LIBRARIES CHECK (IF ABSENT - INSTALL) ####
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("e1071")) install.packages("e1071")
if (!require("caret")) install.packages("caret")
if (!require("earth")) install.packages("earth")
if (!require("mda")) install.packages("mda")
if (!require("glmnet")) install.packages("glmnet")
if (!require("shiny")) install.packages("shiny")
if (!require("DT")) install.packages("DT")
if (!require("shinythemes")) install.packages("shinythemes")

#### LIBRARIES TO LOAD ####
library(ggplot2)
library(e1071)
library(caret)
library(earth)
library(mda)
library(glmnet)
library(shiny)
library(DT)
library(shinythemes)

###################
## APP STRUCTURE ##
###################
#### USER INTERFACE ####
ui <- fluidPage(
  theme = shinytheme("yeti"),                                                   #A specific theme selected
  h1("Automated Testing, Data Clasiffication",                                  #App title, centrally aligned
     align = "center"),                   
  sidebarLayout(
    sidebarPanel(
      h3("Step 1: Data"),
      fileInput(                                                                #Input should be a csv dataset
        "Dataset",                                                     
        "Input dataset",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
          )
        ),
      textInput(                                                                #Column used as a classifying factor
        "ColumnFactor",
        "Factor column name",
        value = NULL
        ),
      textInput(                                                                #Columns used as data for classification
        "ColumnsData",
        "Data columns names (space separated)",
        value = NULL
        ),
      h3("Step 2: Method"),
      selectInput(                                                              #Choice of a method
        "Method",                                                               #(only 5 implemented so far)
        "Method choice",                                                        #(only metric used is Accuracy/Kappa)
        choices = list(
          "knn" = "knn",
          "svmRadial" = "svmRadial",
          "lda" = "lda",
          "fda" = "fda",
          "glmnet" = "glmnet"
          ), 
        selected = NULL),
      sliderInput(                                                              #Elements to be fed to TrainControl()
        "NumberNo",                                                             #function
        "Train Control: number",
        min = 1, 
        max = 100, 
        value = 10
        ),
      sliderInput(
        "RepeatNo", 
        "Train Control: repeats",
        min = 1, 
        max = 10, 
        value = 3
        ),
      textInput(                                                                #Random sed
        "SeedNo",
        "RandomSeed",
        value = NULL
      ),
      checkboxInput(                                                            #Should function normalize data
        "checkbox",                                                             #before testing (for algorithms
        "Normalize data prior?",                                                #without in-built function)
        value = FALSE),
      h3("Step 3: Results"),
      downloadButton("downloadData", "Console printout"),                       #Downloading test printouts
      downloadButton("downloadData2", "Box plots"),                             #Downloading boxplots 
      downloadButton("downloadData3", "Violin plots"),                          #Downloading violin plots
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About the app",
          p(" "),
          strong("An example of a working Shiny app"),
          p(" "),
          p("This app was written as an 
             example how a specific element of R usage, 
            here testing the applicability of specific 
            classsifyig algorithms to numerical data,
            can be wrapped in a GUI provided by the Shiny library.
            Apart from testing proper, with visualisation of key metrics,
            the app allows testing data inputted in a CSV file as well as
            saving printouts from the training as txt and visualisations as
            png (visualisatins available about ~15s after data genearation)."),
          p(" "),
          p("To run this app from GitHub 
            locally use the code below:"),
          code('shiny::runGitHub("AutomatedClassificationApp","AndrzejRomaniuk", 
               ref = "main")'),
          p(" "),
          p("See the link below for the GitHub page"),
          tags$a(href="https://github.com/AndrzejRomaniuk/AutomatedClassificationApp", 
                 "github.com/AndrzejRomaniuk/AutomatedClassification"),
          p(" "),
                 
        ),
        
        tabPanel(
          "Dataset",  
          dataTableOutput(                                                      #Whole input dataset
            'InputTable') 
        ),
        
        tabPanel(
          "Dataset (selected)",  
          dataTableOutput(                                                      #Dataset, only selected columns
            'InputTable2') 
        ),
        tabPanel(                                                               #Equation to be used
          "Equation",   
          verbatimTextOutput(              
            "EquationFinal"
            )                 
        ),
        tabPanel(                                                               #How initial test looks like
          "Model, details",   
          verbatimTextOutput(              
            "Model"
          )                 
        ),
        tabPanel(
          "Box plots",                                                          #Range of Accuracy and Kappa obtained
          plotOutput(
            "ResultsVisualisation"
          )
        ),
        tabPanel(
          "Violin plots",                                                       #Range of Accuracy and Kappa obtained 2
          plotOutput(
            "ResultsVisualisation2"
          )
        )
        
      )  
    )
  )
)

#### SERVER SIDE ####
server <- function(input, output, session) {
  
  dataReactive <- reactive({                                                    #Raw dataset
    Input <- input$Dataset
    
    if (is.null(Input)){
      return(NULL)
    } else{
      DatasetUsed <- as.data.frame(read.csv(Input$datapath))
      return(DatasetUsed)
    }
    })
  
  dataReactive2 <- reactive({                                                   #Dataset, only selected
    Input <- input$Dataset
    
    if (is.null(Input)){
      return(NULL)
    } else{
      DatasetUsed <- as.data.frame(read.csv(Input$datapath))
      if (input$ColumnsData == "" & input$ColumnFactor == "") {
        return(NULL)
      } else { 
      DatasetUsed <- DatasetUsed[stringReactive()]
      DatasetUsed[,input$ColumnFactor] <- as.factor(
        DatasetUsed[,input$ColumnFactor])
      }
      return(DatasetUsed)
    }
  })
  
  stringReactive <- reactive({                                                  #All columns considered
    StringsUsed <- c(
      input$ColumnFactor,strsplit(
        input$ColumnsData, "\\s+"
        )[[1]]
      )
    return(StringsUsed)
  })
  
  Equation <- reactive({                                                        #Creating an equation for Train() function
    if (input$ColumnsData == "" | input$ColumnFactor == "") {
      return(NULL)
    } else {    
      FactorSep  <- paste(input$ColumnFactor,"~",sep = "")
      ColumnsNames<- paste(c(strsplit(input$ColumnsData, "\\s+")[[1]]), 
                         collapse= "+")
      EquationUsed <- eval(parse(text=paste(FactorSep,ColumnsNames,sep = "")))
      return(EquationUsed)
    }
  })
  
  Control <- reactive({                                                         #Compiling data for tainControl() function
    FinalControl <- trainControl(
      method="repeatedcv", 
      number=input$NumberNo, 
      repeats=input$RepeatNo
      )
    return(FinalControl)
  })
  
  Model <- reactive({                                                           #Training Models
    if(input$SeedNo == ""){
      return(NULL)
    } else {
      set.seed(input$SeedNo)    
      if (input$checkbox == TRUE) {
        fitting <-  train(
          Equation(), 
          data=dataReactive2(), 
          method=input$Method, 
          metric="Accuracy", 
          trControl=Control(), 
          preProc=c("center", "scale")
          )      
      } else {
        fitting <-  train(
          Equation(), 
          data=dataReactive2(), 
          method=input$Method, 
          metric="Accuracy", 
          trControl=Control()
          )  
      }
      return(fitting)  
    }
  })
  
  Summary <- reactive({                                                         #Summary of Accuracy and Kappa
    DataSummary1 <- data.frame(
      matrix(
        ncol = 2, 
        nrow = length(
          Model()$resample$Accuracy
          )
        )
      )
    colnames(DataSummary1) <- c("Type","Value") 
    DataSummary1$Type <- "Accuracy"
    DataSummary1$Value <- Model()$resample$Accuracy
    
    DataSummary2 <- data.frame(
      matrix(
        ncol = 2, 
        nrow = length(
          Model()$resample$Kappa
          )
        )
      )
    colnames(DataSummary2) <- c("Type","Value") 
    DataSummary2$Type <- "Kappa"
    DataSummary2$Value <- Model()$resample$Kappa
    
    DataSummary <- rbind(DataSummary1, DataSummary2)
    
    return(DataSummary)
  })
  
  output$InputTable <- renderDataTable(                                         #Rending a raw dataset
    dataReactive(),                                                             
    options = list(dom = 'ltp'),
    rownames= FALSE                                                             
  )
  
  output$InputTable2 <- renderDataTable(                                        #Rending data selected
    dataReactive2(),                                                             
    options = list(dom = 'ltp'), 
    rownames= FALSE                                                             
  )
  
  output$EquationFinal <- renderPrint(                                          #Rendering how the final equation
    {Equation()}                                                                #Looks like
  )
  
  output$Model <- renderPrint(                                                  #Rendering how the final traincontrol()
    {Model()}                                                                   #Looks like
  )

  vals <- reactiveValues()                                                      #External storage to ggplots (for later)
  
  output$ResultsVisualisation <- renderPlot({                                   #Boxplots for accuracy and kappa
    ggpl1 <- ggplot(Summary(), aes(x = Type, y = Value, fill = Type)) + 
      geom_boxplot() + theme_classic() + theme(legend.position = "none")
    vals$ggpl1 <- ggpl1
    print(ggpl1)
  })
  
  output$ResultsVisualisation2 <- renderPlot({                                  #Violin plots for accuracy and kappa
    ggpl2 <- ggplot(Summary(), aes(x = Type, y = Value, fill = Type)) + 
      geom_violin() + theme_classic() + theme(legend.position = "none")
    vals$ggpl2 <- ggpl2
    print(ggpl2)
  })
  
  output$downloadData <- downloadHandler(                                       #Creating a txt file to download
    filename = function(){
      paste("results.txt")
    },
    content = function(file) {
      writeLines(paste(capture.output( {Model()} )), file)                                                                 
    }
  )
  
  output$downloadData2 <- downloadHandler(                                      #Creating a png file (first) to download
    filename = function() {"Boxplot.png"},
    content = function(file) {
      png(file)
      print(vals$ggpl1)
      dev.off()
    }, contentType = 'image/png'
    )    
  
  output$downloadData3 <- downloadHandler(                                      #Creating a png file (second) to download
    filename = function() {"Violinplot.png"},
    content = function(file) {
      png(file)
      print(vals$ggpl2)
      dev.off()
    },contentType = 'image/png'
    )   
}

#### COMBINING UI AND SERVER ####
shinyApp(ui, server)