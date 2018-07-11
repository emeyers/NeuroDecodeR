library(shinydashboard)
# library(semantic.dashboard)


ui <- dashboardPage(
  dashboardHeader(title = "NDTr"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bin", tabName = "bin"),
      menuItem("Decode", tabName = "decode")
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "bin",
              fluidPage(
               
                fluidRow(
                  column(width = 4,
                         box(
                           radioButtons("bin_bUpload", "Choose raster data", c("Upload your own", "Use ours")),
                           conditionalPanel("input.bin_bUpload === 'Upload your own'",
                                            fileInput("bin_raster_upload", "Upload data")),
                           conditionalPanel("input.bin_bUpload === 'Use ours'",
                                            selectInput("bin_raster_our", "Data available",c("search data dir"))),
                           
                           numericInput("bin_width", "Bin width", value = 10, min = 1),
                           numericInput("step_size", "Step size", value = 1, min = 1)
                         )
                             
                         )
                  
                )
                )
              
      ),
      tabItem(tabName = "decode",
             fluidPage(
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
    
  )
)


