require(shiny)
require(dplyr)
require(tools)
require(DBI)
require(DT)
require(shinyjs)
require(utils)
require(e1071)
require(httr)
require(reshape2)
require(data.table)

source("list.R")
source("WSUWSCpredict.R")

ui <- shinyUI(
  fluidPage(
    theme = "bootstrap.css",
    title = "",
    windowTitle = "Phenolics Gateway",
    navbarPage(
      position = 'static-top',
      img(
        src = "Digital_world.jpg",
        height = "100px",
        width = "125px",
        style = " margin-left: 25px;
                             margin-right: 160px; margin-top: -5px;
                              border-radius: 50%;"
      ),
      tabPanel(
        h3("Make a New Prediction"),
        sidebarLayout(
          sidebarPanel(
            style = "background-color: #000820; color: #ffffff",
            tags$head(tags$style(
              ".progress-bar{background-color: #000820;}"
            )),
            tags$head(tags$style(".btn{background-color: #000820;}")),
            tags$head(tags$style(
              ".btn:hover{background-color: #000820;}"
            )),
            fileInput(
              "newfile",
              "Upload spectra",
              accept = c(
                "text/csv",
                "text/comma-separated-values",
                "text/tab-separated-values",
                "text/plain",
                ".xlsx",
                ".csv",
                ".tsv"
              )
            ),
            tags$head(
              tags$style(
                ".predictNew{background-color: #000820;}
                                                                             .predictNew{color: white;}
                                                                             .predictNew{border: none;}
                                                                             .predictNew:hover{background-color: #000820;}"
              )
            )
          ),
          mainPanel(tabPanel(' ', DT::dataTableOutput('table')))
        )
      ),
      tabPanel(
        h3('Vintage History'),
        sidebarLayout(
          sidebarPanel(
            style = "background-color: #000820; color: #ffffff",
            selectizeInput(
              'vintage',
              'Vintage',
              choices = vintage,
              options = list(
                placeholder = ' ',
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            downloadButton('download', 'Save vintage history', class = 'mybutton'),
            tags$head(
              tags$style(
                ".mybutton{background-color:#000820;}
                                                                             .mybutton{color: white;}
                                                                             .mybutton{border: none;}
                                                                             .mybutton:hover{background-color: #000820;}"
              )
            )
          ),
          mainPanel(tabPanel(' ', DT::dataTableOutput('table1')))
        )
      )
    )
  )
)

server <- shinyServer(function(input, output, session) {
  users_data <- data.frame(START = Sys.time())
  
  session$onSessionEnded(function() {
    users_data$END <- Sys.time()
    write.table(
      x = users_data,
      file = file.path(getwd(), "data/users_data.txt"),
      append = TRUE,
      row.names = FALSE,
      col.names = FALSE,
      sep = "\t"
    )
  })
  
  newTable <- reactive({
    input$refresh
    invalidateLater(1000 * 60 * 5, session)
    
    startTime <- format(Sys.time(), '%b-%d, %I:%M')
    
    temp <- tempdir()
    
    if (is.null(input$newfile)) {
      return(NULL)
    }
    
    initData <-
      read.csv(input$newfile$datapath,
               header = T,
               check.names = F)
    newData  <-
      data.frame(
        format(Sys.time(), "%Y"),
        format(Sys.time(), "%b %d"),
        format(as.POSIXct(Sys.time()), "%H:%M"),
        initData
      )
    colnames(newData) <-
      c("Vintage", "Date", "Time", "Tank ID", colnames(initData[,-1]))
    newData_pred <- newData[, -1:-4]
    newData.df <-
      data.frame(newData[, 1],
                 newData[, 2],
                 newData[, 3],
                 newData[, 4],
                 pred(newData_pred))
    rownames(newData.df) <- NULL
    colnames(newData.df) <-
      c(
        "Vintage",
        "Analysis Date",
        "Analysis Time",
        "Tank ID",
        "Anthocyanins (mg/L Malv EQ)",
        "Tannins (mg/L CE)",
        "Total Iron Reactive Phenolics (mg/L CE)"
      )

    if (file.exists(
      paste(
        "Vintage Logs/",
        as.character(newData[1, 1]),
        '.csv',
        sep = ""
      )
    )) {
      oldData <-
        read.csv(
          paste(
            "Vintage Logs/",
            as.character(newData[1, 1]),
            '.csv',
            sep = ""
          )
        )
      names(newData.df) <- names(oldData)
      merged <- rbind(oldData, newData.df)
      write.csv(merged,
                file = (
                  paste(
                    "Vintage Logs/",
                    as.character(newData[1, 1]),
                    '.csv',
                    sep = ""
                  )
                ),
                row.names = FALSE)
    } else {
      write.csv(
        newData.df,
        file = paste(
          "Vintage Logs/",
          as.character(newData[1, 1]),
          '.csv',
          sep = ""
        ),
        row.names = FALSE
      )
    }
    
    pred(newData_pred)
  })
  
  output$table <- DT::renderDataTable({
    datatable(newTable(), rownames = T)
  })
  
  historyTable <- reactive({
    input$refresh
    invalidateLater(1000 * 60 * 5, session)
    
    history <-
      read.csv(
        paste(
          "Vintage Logs/",
          as.character(input$vintage),
          '.csv',
          sep = ""
        )
      )
    dataHistory <- data.frame(history)
    dataHistory1 <- dataHistory[, 1:4]
    dataHistory2 <- dataHistory[, 5:7]
    dataHistory3 <- data.frame(format(dataHistory2, nsmall = 1))
    dataHistoryFinal <- cbind(dataHistory1, dataHistory3)
    colnames(dataHistoryFinal) <-
      c(
        "Vintage",
        "Analysis Date",
        "Analysis Time",
        "Tank ID",
        "Anthocyanins (mg/L Malv EQ)",
        "Tannins (mg/L CE)",
        "Total Iron Reactive Phenolics (mg/L CE)"
      )
    
    dataHistoryFinal
    
  })
  
  output$table1 <- renderDataTable({
    validate(need(input$vintage != '', 'Please Select a Vintage'))
    datatable(historyTable(), rownames = F)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("phenolics",  " ", input$vintage, '/', Sys.time(), '.csv')
    },
    content = function(file) {
      write.csv(historyTable(), file, row.names = FALSE)
    }
  )
  
  
})

shinyApp(ui, server)
