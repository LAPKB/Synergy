library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(basictabler)



# UI ----------------------------------------------------------------------

# Define UI for application that assists with hollow fiber set up
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("Drug Interaction"),
  
  #layout
  sidebarLayout(
    sidebarPanel(width = 3,
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv')),
                 numericInput('headerStart',"Header start row",value = 3),
                 numericInput('headerEnd',"Header end row",value = 9),
                 numericInput('dataStart',"Data table header row", value = 14),
                 textInput('rowDrug',"Row Drug Name", "Drug A"),
                 textInput('colDrug',"Column Drug Name", "Drug B"),
                 textInput('rowConc',"Drug A (Row) Concentrations", value = paste(c(64,32,16,8,4,2,1,0), collapse = ",")),
                 textInput('colConc',"Drug B (Column) Concentrations", value = paste(c(0,5,10,15,25,35,50,60,75,80,90,100), collapse = ",")),
                 helpText("Enter values separated by commas")
                 
                 
    ), #end sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Raw Data",
          basictabler::basictablerOutput("raw")
        ),
        tabPanel(
          "Processed",
          basictabler::basictablerOutput("adj"),
          br(),
          h3("Key"),
          uiOutput("micUI"),
          br(),
          h3("Override MIC"),
          uiOutput("micAOver"),
          uiOutput("micBOver")
          
        ),
        tabPanel(
          "FICI",
          basictabler::basictablerOutput("fici"),
          uiOutput("ficiKey")
        )
      ) #end tabsetPanel
      
    ) #end mainPanel
    
  ) #end sidebarLayout
  
) #end fluidPage

server <- (function(input, output) {
  
  #formatting
  green <- "#73a839"
  yellow <- "#FFEB9C"
  orange <- "#dd5600"
  red <- "#c71c22"
  blue <- "#2fa4e7"
  white <- "#FDFEFE"
  
  colConcs <- reactive(as.numeric(stringr::str_split(input$colConc, ",")[[1]]))
  rowConcs <- reactive(as.numeric(stringr::str_split(input$rowConc, ",")[[1]]))
  
  mydata <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tbl <- read.csv(inFile$datapath, skip=input$dataStart-3, nrows = 8, 
                    row.names = 1, 
                    col.names = c("",paste0("B_",colConcs())))
    rownames(tbl) <- paste0("A_",rowConcs())
    round(tbl,3)    
    return(tbl)
  })
  
  output$raw <- basictabler::renderBasictabler({
    if(!is.null(mydata())){
      f_tbl <- qtbl(mydata(), rowNamesAsRowHeaders = T)
      basictabler::basictabler(f_tbl)
    }
  })
  
  
  #find growth control
  row0 <- reactive(which(rowConcs() == 0)[1])
  col0 <- reactive(which(colConcs() == 0)[1])
  ODgc <- reactive(mydata()[row0(), col0()])
  
  #percent OD relative to growth control
  mydata2 <- reactive({
    round(mydata()/ODgc(), 3)
  })
  
  
  
  
  #MIC of drug A (rows)
  micA <- reactive(
    min(rowConcs()[which(mydata2()[,col0()] <= 0.2)])
  )
  
  #MIC of drug B (columns)
  micB <- reactive(
    min(colConcs()[which(mydata2()[row0(),] <= 0.2)])
  )
  
  #format
  f_mydata2 <- reactive({
    mydata2() %>% 
      datatable(rownames = T) %>%
      formatStyle(columns = )
  })
  
  #https://bootswatch.com/3/cerulean/bootstrap.css
  
  output$adj <- basictabler::renderBasictabler({
    if(!is.null(mydata2())){
      f_tbl2 <- qtbl(mydata2(), rowNamesAsRowHeaders = T)
      #growth control
      f_tbl2$setStyling(1+row0(), 1+col0(), declarations=list("background-color"=green,
                                                              "color"=white)) 
      #mic A
      micA_row <- max(which(mydata2()[,col0()] <= 0.2))
      f_tbl2$setStyling(1+micA_row, 1+col0(), declarations=list("background-color"=blue, 
                                                                "color"=white))
      #mic B
      micB_col <- min(which(mydata2()[row0(),] <= 0.2))
      f_tbl2$setStyling(1+row0(), 1+micB_col, declarations=list("background-color"=red, 
                                                                "color"=white))
      basictabler::basictabler(f_tbl2)
    }
  })
  
  
  output$MIC_A <- renderText(micA())
  output$MIC_B <- renderText(micB())
  
  output$micUI <- renderUI({
    p(
      span(style = paste0("background-color:",green,"; color:",white),"Growth control"),
      br(),
      span(style = paste0("background-color:",blue,"; color:",white),input$rowDrug,"MIC: ",micA()),
      br(),
      span(style = paste0("background-color:",red,"; color:",white),input$colDrug,"MIC: ",micB())
    )
  })
  
  output$micAOver <- renderUI({
    numericInput("overA",input$rowDrug,"")
  })
  output$micBOver <- renderUI({
    numericInput("overB",input$colDrug,"")
  })
  
  #calculate FICI
  mydata3 <- reactive({
    fici <- matrix(NA,nrow = 8, ncol = 12)
    for(i in 1:8){
      for(j in 1:12){
        concA <- rowConcs()[i]
        concB <- colConcs()[j]
        if(mydata2()[i,j] <= 0.2){
          fici[i,j] <- concA/micA() + concB/micB()
        }
      }
    }
    fici <- data.frame(fici)
    #zero out 
    fici[row0(),] <- NA
    fici[,col0()] <- NA
    
    names(fici) <- names(mydata2())
    fici2 <- data.frame(map_df(fici,~ifelse(is.na(.x),"",round(.x,2))))
    rownames(fici2) <- rownames(mydata2())
    fici2

  })
  
  
  
  output$fici <- basictabler::renderBasictabler({
    if(!is.null(mydata3())){
      f_tbl3 <- qtbl(mydata3(), rowNamesAsRowHeaders = T)
      synCells <- f_tbl3$findCells(maxValue = 0.5, rowNumbers = 2:8, columnNumbers = 3:13, rowColumnMatchMode = "combinations")
      addCells <- f_tbl3$findCells(valueRanges = "0.5<v<=1", rowNumbers = 2:8, columnNumbers = 3:13, rowColumnMatchMode = "combinations")
      indCells <- f_tbl3$findCells(valueRanges = "1<v<4", rowNumbers = 2:8, columnNumbers = 3:13, rowColumnMatchMode = "combinations")
      antCells <- f_tbl3$findCells(valueRanges = "v>=4", rowNumbers = 2:8, columnNumbers = 3:13, rowColumnMatchMode = "combinations")
      f_tbl3$setStyling(cells = synCells, declarations=list("background-color"=green, "color"=white))
      f_tbl3$setStyling(cells = addCells, declarations=list("background-color"=yellow))
      f_tbl3$setStyling(cells = indCells, declarations=list("background-color"=orange, "color"=white))
      f_tbl3$setStyling(cells = antCells, declarations=list("background-color"=red, "color"=white))
      basictabler::basictabler(f_tbl3)
    }
  })
  
  output$ficiKey <- renderUI({
    tagList(
      br(),
      h3("Key"),
      p(span(style = paste0("background-color:",green,"; color:",white),"Synergistic"),
        "<=0.5"),
      p(span(style = paste0("background-color:",yellow),"Additive"),
        ">0.5 - 1"),
      p(span(style = paste0("background-color:",orange,"; color:",white),"Indifferent"),
        ">1 - 4"),
      p(span(style = paste0("background-color:",red,"; color:",white),"Antagonistic"),
        ">4")
    )
  })
  
  
  
  
  
})
# Shiny App ---------------------------------------------------------------

# runGadget(shinyApp(ui = ui, server = server), viewer = browserViewer())
shinyApp(ui = ui, server = server)


