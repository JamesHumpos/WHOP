########################################### Necessary packages #################################################
library(shiny)
library(DiagrammeR)
library(V8)
library(rsvg)
library(stringr)
library(dplyr)
library(data.table)
library(tidyr)
####################### Diagramming functions - in an attached source file currently #########################
source('functions.R')


      ################################ Shiny-Specific functions - can be hidden ####################################

## Creates the graph itself, assigns an empty string to the returned value if no file ('x' -which is referenced later) 
## is yet input, then tries to clean the data set into a readable format accepted by function 'as.data.model'.
## This step requires that the original data has fields FK_TABLE_CODE and COLUMN_CODE it creates
## dummy vars 'key' and 'ref_col', the former is boolean - derived by assuming that fields empty for column 
## FK_TABLE_CODE are not foreign keys. Those who are FKs are given true and column ref_col is copied

get_graph <- function(x, rankdir = "RL", view_type = "all",focus = NULL) {
  ret <- ""
  try({
    rawdata <- x()
    df <- rawdata
    reds <- df[df$TABLE_CODE %like% "RAW_", ]
    greens <- df[df$TABLE_CODE %like% "REF_", ]
    greys <- df[df$TABLE_CODE %like% "CONVERT_", ]
    blues <- df[df$TABLE_CODE %like% "COVAX_", ]
    yellows <- df[df$TABLE_CODE %like% "FACT_", ]
    blues = unique(unlist(blues$TABLE_CODE))
    reds = unique(unlist(reds$TABLE_CODE))
    yellows = unique(unlist(yellows$TABLE_CODE))
    greens = unique(unlist(greens$TABLE_CODE))
    greys = unique(unlist(greys$TABLE_CODE))
    rawdata[rawdata==""]<-NA
    rawdata$key <- with(rawdata, ifelse(is.na(FK_TABLE_CODE),FALSE,TRUE))
    rawdata$ref_col <- with(rawdata, ifelse(is.na(FK_TABLE_CODE), NA ,COLUMN_CODE))
    colnames(rawdata) <- c("table","column","ref","key","ref_col")
    modelleddata <- as.data_model(rawdata)
    dm <- dm_set_display(modelleddata, display = list(
      accent1 = blues,
      accent2 = reds,
      accent3 = yellows,
      accent4 = greens,
      accent6 = greys)
    )
    graph <- dm_create_graph(dm, rankdir = rankdir, view_type = view_type,focus = focus)
    ret <- graph$dot_code
  })
  ret
}

## As above, though instead of returning 'ret' which is a vector containing drawing coordinates, it
## returns an object that can be passed to download button functions

get_graph_download <- function(x, rankdir = "RL", view_type = "all",focus = NULL) {
  ret <- ""
  try({
    rawdata <- x()
    rawdata[rawdata==""]<-NA
    rawdata$key <- with(rawdata, ifelse(is.na(FK_TABLE_CODE),FALSE,TRUE))
    rawdata$ref_col <- with(rawdata, ifelse(is.na(FK_TABLE_CODE), NA ,COLUMN_CODE))
    colnames(rawdata) <- c("table","column","ref","key","ref_col")
    modelleddata <- as.data_model(rawdata)
    
    graph <- dm_create_graph(modelleddata, rankdir = rankdir, view_type = view_type,focus = focus)
  })
  graph
}



                          ################################ UI PORTION ###################################


ui = shinyUI(fluidPage(
  fluidRow(
    column(2, HTML("<h2>DBD <b>xMART </b></h2>"))
  ),
  fluidRow(
    column(8,
           grVizOutput('diagram', height = 600)
    ),
    column(4,
           fileInput("file1", "Choose CSV File",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
           selectInput("rankdir", label = "Graph direction",  choices = list(
             "Right-left" = "RL",
             "Bottom-top" = "BT",
             "Left-right" = "LR",
             "Top-bottom" = "TB"), selected = "RL"),
           hr(),
           selectizeInput("tables", "Select DB objects to focus on", choices = NULL, multiple = T),
           selectInput("view_type", label = "Show columns",  choices = list(
             "All columns" = "all",
             "Keys only" = "keys_only",
             "No columns" = "title_only"), selected = "keys_only"),
           conditionalPanel(condition = "input$subsetprefixask == 1",
                            selectizeInput("prefixes", "Select prefixes to subset by", choices = NULL, multiple = T),
                            selectizeInput("suffixes", "Select suffixes to subset by", choices = NULL, multiple = T)),
           downloadButton(outputId = 'downloadDataPng', label = 'Download diagram PNG')

    )
  )
))


                          ################################ SERVER PORTION ###################################



server = function(input, output, session){

  mydf <- reactive({
    req(input$file1)
    df = read.csv(input$file1$datapath)
    if(is.null(input$prefixes) & is.null(input$suffixes)) {
      return(df)
    }
    else if(is.null(input$suffixes)){
      df2 <- df[df$TABLE_CODE %like% paste(input$prefixes, collapse = "|"), ]
      return(df2)
    }
    else if(is.null(input$prefixes)){
      df3 <- df[df$TABLE_CODE %like% paste(input$suffixes, collapse = "|"), ]
      return(df3)
    }
    else{
      df4 <- df[df$TABLE_CODE %like% paste(input$prefixes, collapse = "|"), ]
      df4 <- df[df$TABLE_CODE %like% paste(input$suffixes, collapse = "|"), ]
      return(df4)
    }
  })
  
  prefstr <- reactive({
    req(input$file1)
    df = read.csv(input$file1$datapath)
    prefixstring <- sapply(str_split(df$TABLE_CODE, "_"), `[`, 1)
    prefixstring <- unique(prefixstring)
    prefixstring=paste0(prefixstring,"_")
    return(prefixstring)
  })
  
  suffstr <- reactive({
    req(input$file1)
    df = read.csv(input$file1$datapath)
    suffixstr<-as.data.frame(unique(stringr::str_match(df$TABLE_CODE, '(.*)_(.*)')[, -1]))
    suffixstr<-unique(suffixstr$V2)
    return(suffixstr)
  })
  
  
  observeEvent(input$file1, {
    req(mydf())
    updateSelectizeInput(session, "tables","Select DB objects to focus on", choices = mydf()$TABLE_CODE)
    req(prefstr())
    updateSelectizeInput(session, "prefixes","Select prefixes to subset to", choices = prefstr())
    req(suffstr())
    updateSelectizeInput(session, "suffixes","Select prefixes to subset to", choices = suffstr())
  })
  
  output$diagram <- renderGrViz({
    grViz(
      get_graph(mydf, rankdir = input$rankdir, view_type = input$view_type,focus = list(tables=c(input$tables)))
    )
  })
  
  output$downloadDataPng <- downloadHandler(
    filename = "export.png",
    content = function(file) {
      graph <- get_graph_download(input$file1, rankdir = input$rankdir, view_type = input$view_type,focus = list(tables=c(input$tables)))
      dm_export_graph(graph, file, file_type = "png");
    },
    contentType = "image/png"
  )
  
  
  #  info <- eventReactive(input$field1, {
  #    # https://stackoverflow.com/questions/67248364/shiny-not-reacting-to-uploaded-csv-to-allow-user-to-select-columns
  #    req(mydf())
  #    f <- mydf()
  #    f
  #  })
  
}

                            ################################ RUN APP ###################################


shinyApp(ui = ui, server = server)



