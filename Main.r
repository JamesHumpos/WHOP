library(shiny)
library(shinyAce)
library(DiagrammeR)
library(V8)
library(rsvg)


################################

get_data <- function(datainput){
  inFile <- datainput
  if (is.null(inFile))
    return(NULL)
  rawdata <- read.csv(file = inFile$datapath)
  return(rawdata)
}




get_graph <- function(x, rankdir = "RL", view_type = "all") {
  
  ret <- ""
  try({
    rawdata <- get_data(x)
    rawdata[rawdata==""]<-NA
    rawdata$key <- with(rawdata, ifelse(is.na(FK_TABLE_CODE),FALSE,TRUE))
    rawdata$ref_col <- with(rawdata, ifelse(is.na(FK_TABLE_CODE), NA ,COLUMN_CODE))
    colnames(rawdata) <- c("table","column","ref","key","ref_col")
    modelleddata <- as.data_model(rawdata)
    graph <- dm_create_graph(modelleddata, rankdir = rankdir, view_type = view_type)
    ret <- graph$dot_code
  })
  ret
}

# Set the default YAML to example.yml
f_name <- system.file("samples/example.yml", package = "datamodelr")
default_yaml <- paste0(readLines(f_name), collapse   = "\n")

ui = shinyUI(fluidPage(
  tags$head(tags$script(src = "message-handler.js")),
  fluidRow(
    column(2, HTML("<h2>Data<b>Diagram</b>xMart</h2>"))
  ),
  fluidRow(
    column(4,
           aceEditor("ace", mode = "yaml", value=default_yaml, height = 600, fontSize = 15)
    ),
    column(6,
           grVizOutput('diagram', height = 600)
    ),
    column(2,
           fileInput("file1", "Choose CSV File",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
           selectInput("view_type", label = "Show columns",  choices = list(
             "All columns" = "all",
             "Keys only" = "keys_only",
             "No columns" = "title_only"), selected = "all"),
           selectInput("rankdir", label = "Graph direction",  choices = list(
             "Right-left" = "RL",
             "Bottom-top" = "BT",
             "Left-right" = "LR",
             "Top-bottom" = "TB"), selected = "RL"),
           hr(),
           downloadButton(outputId = 'downloadData', label = 'Download SVG'),
           downloadButton(outputId = 'downloadDataPng', label = 'Download PNG')
    )
  )
))

server = function(input, output, session){

  output$diagram <- renderGrViz({
    grViz(
      get_graph(input$file1, rankdir = input$rankdir, view_type = input$view_type)
    )
    
  })
  output$downloadData <- downloadHandler(
    filename = "export.svg",
    content = function(file) {
      dm <- dm_read_yaml(text = input$ace)
      graph <- dm_create_graph(dm, rankdir = input$rankdir, view_type = input$view_type)
      dm_export_graph(graph, file, file_type = "svg");
    },
    contentType = "image/svg+xml"
  )
  output$downloadDataPng <- downloadHandler(
    filename = "export.png",
    content = function(file) {
      dm <- dm_read_yaml(text = input$ace)
      graph <- dm_create_graph(dm, rankdir = input$rankdir, view_type = input$view_type)
      dm_export_graph(graph, file, file_type = "png");
    },
    contentType = "image/png"
  )
}


shinyApp(ui = ui, server = server)
