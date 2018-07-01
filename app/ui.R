ui = fluidPage(
  # Some custom CSS
  tags$head(
    tags$style(HTML("
                    /* Smaller font for preformatted text */
                    pre, table.table {
                    font-size: smaller;
                    }
                    
                    body {
                    min-height: 2000px;
                    }
                    
                    .option-group {
                    border: 1px solid #ccc;
                    border-radius: 6px;
                    padding: 0px 5px;
                    margin: 5px -10px;
                    background-color: #f5f5f5;
                    }
                    
                    .option-header {
                    color: #79d;
                    text-transform: uppercase;
                    margin-bottom: 5px;
                    }
                    "))
    ),
  
  
  fluidRow(
    column(width=3,
           div(class = "option-group",
               div(class = "option-header", "Data source"),
               radioButtons("DS_bUpload", "Upload your data?", c("Yes", "No")),
               conditionalPanel("input.DS_bUpload === 'Yes'",
                                fileInput("DS_upload", "Upload data")),
               conditionalPanel("input.DS_bUpload === 'No'",
                                selectInput("DS_our", "Data available",c("search data dir"))),
                                
               numericInput("DS_bin_width", "Bin width", value = 10, min = 1),
               numericInput("DS_step_size", "Step size", value = 1, min = 1),
               
               selectInput("DS_type", "Decoding type", c("basic_DS","generalization_DS")),
               selectInput("DS_specific_var", "Specifc variable", c("search raster var")),
               
               conditionalPanel("input.DS_type === 'basic_DS'",
                                selectInput("DS_label", "Labels", c("search raster labels"), multiple = TRUE)),
               conditionalPanel("input.DS_type === 'generalization_DS'",
                                selectInput("DS_training_label", "Training labels", c("search raster labels"), multiple = TRUE),
                                selectInput("DS_training_label", "Testing labels", c("search raster labels"), multiple = TRUE)
               )
               
                                
           )
    )
  )
)
               
               
               