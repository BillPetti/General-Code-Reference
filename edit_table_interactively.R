library(rhandsontable)
library(shiny)
library(readr)

edit_data_frame <- function(DF, 
                            open_browser = TRUE, 
                            outdir=getwd()){
  # code is adapted from this post: 
  # http://stla.github.io/stlapblog/posts/shiny_editTable.html
  
  ui <- shinyUI(fluidPage(
    
    titlePanel("Edit and save a table"),
    sidebarLayout(
      sidebarPanel(
        helpText("Right-click on the table to delete/insert rows or columns.", 
                 "Double-click on a cell to edit", 
                 "Values can be copied down and across cells by dragging."),
        
        # wellPanel(
        #   h3("Table options"),
        #   radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
        # ),
        br(), 
        
        radioButtons('fileType', 
                     label = "Save table as", 
                     choices = c("ASCII", 
                                 "CSV", 
                                 "RDS"), 
                     selected = "CSV"), 
        br(), 
        textInput('file_name_input', 
                  label = "Enter the name you want to save the file as",
                  value = 'table'),
        actionButton("save", "Save"), 
        width = 2),
      
      mainPanel(
        wellPanel(
          uiOutput("message", inline=TRUE)
        ),
        
        actionButton("cancel", "Cancel last action"),
        
        br(), br(), 
    
        rHandsontableOutput("hot"),
        br()
  )
)
))
  
  server <- shinyServer(function(input, output) {
    
    values <- reactiveValues()
    
    ## Handsontable
    observe({
      if (!is.null(input$hot)) {
        values[["previous"]] <- isolate(values[["DF"]])
        DF = hot_to_r(input$hot)
      } else {
        if (is.null(values[["DF"]]))
          DF <- DF
        else
          DF <- values[["DF"]]
      }
      values[["DF"]] <- DF
    })
    
    output$hot <- renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
        rhandsontable(DF, width = 1100, 
                      height = 700,
                      useTypes = FALSE) %>%
        hot_cols(fixedColumnsLeft = 1) %>%
        hot_rows(fixedRowsTop = 1) %>%
        hot_context_menu(allowRowEdit = TRUE)
    })
    
    ## Save 
    observeEvent(input$save, {
      fileType <- isolate(input$fileType)
      finalDF <- isolate(values[["DF"]])
      if(fileType == "ASCII"){
        
        dput(finalDF, file=file.path(outdir, sprintf("%s.txt", input$file_name_input)))
        
      } else if (fileType == "CSV"){
        
        readr::write_csv(finalDF, path=file.path(outdir, sprintf("%s.csv", input$file_name_input)))
        
      } else {
        
        saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", input$file_name_input)))
      }
    }
    )
    
    ## Cancel last action    
    observeEvent(input$cancel, {
      if(!is.null(isolate(values[["previous"]]))) values[["DF"]] <- isolate(values[["previous"]])
    })
    
    ## Add column
    output$ui_newcolname <- renderUI({
      textInput("newcolumnname", "Name", sprintf("newcol%s", 1+ncol(values[["DF"]])))
    })
    observeEvent(input$addcolumn, {
      DF <- isolate(values[["DF"]])
      values[["previous"]] <- DF
      newcolumn <- eval(parse(text=sprintf('%s(nrow(DF))', isolate(input$newcolumntype))))
      values[["DF"]] <- setNames(cbind(DF, newcolumn, stringsAsFactors=FALSE), c(names(DF), isolate(input$newcolumnname)))
    })
    
    ## Message
    output$message <- renderUI({
      if(input$save==0){
        helpText(sprintf("This table will be saved in folder \"%s\" once you press the Save button.", outdir))
      } else {
        outfile <- ifelse(isolate(input$fileType)=="ASCII", paste0(input$file_name_input, ".txt"),
                          ifelse(isolate(input$fileType)=="CSV", paste0(input$file_name_input, ".csv"), 
                                 paste0(input$file_name_input, ".rds")))
        
        fun <- ifelse(isolate(input$fileType)=="ASCII", "dget", 
                      ifelse(isolate(input$fileType)=="CSV", "read_csv", "readRDS"))
        
        list(helpText(sprintf("File saved: \"%s\".", file.path(outdir, outfile))),
             helpText(sprintf("Type %s(\"%s\") to get it.", fun, outfile)))
      }
    })
    
  })
  
  ## run app 
  runApp(list(ui=ui, server=server), launch.browser = open_browser)
  return(invisible())
}
