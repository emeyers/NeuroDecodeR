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
                           conditionalPanel(condition = "input.bin_bUpload == 'Upload your own'",
                                            fileInput("bin_raster_upload", "Upload data")),
                           conditionalPanel(condition = "input.bin_bUpload == 'Use ours'",
                                            selectInput("bin_raster_our", "Data available",c("search data dir"))),
                           
                           numericInput("bin_width", "Bin width", value = 10, min = 1),
                           numericInput("step_size", "Step size", value = 1, min = 1)
                         )
                         
                  ),
                  column(width = 6,
                         box(width = NULL,
                             title = "raster plot",
                             color = "green", ribbon = TRUE, title_side = "top right",
                             
                             plotOutput("plot1")
                             
                         ),
                         box(width = NULL,
                             title = "PSTH",
                             color = "red", ribbon = TRUE, title_side = "top right",
                             
                             plotOutput("plot2")
                             
                         )
                  )
                )
              )
              
      ),
      tabItem(tabName = "decode",
              fluidPage(
                
                
                
                fluidRow(
                  column(width = 2,
                         box(title = "Data source",
                             width = NULL,
                             radioButtons("DS_bUpload", "Upload your data?", c("Yes", "No")),
                             conditionalPanel(condition = "input.DS_bUpload == 'Yes'",
                                              fileInput("DS_upload", "Upload data")),
                             conditionalPanel(condition = "input.DS_bUpload == 'No'",
                                              selectInput("DS_our", "Data available",c("search data dir"))),
                             
                             numericInput("DS_bin_width", "Bin width", value = 10, min = 1),
                             numericInput("DS_step_size", "Step size", value = 1, min = 1),
                             
                             selectInput("DS_type", "Decoding type", c("basic_DS","generalization_DS")),
                             selectInput("DS_specific_var", "Specifc variable", c("search raster var")),
                             
                             conditionalPanel(condition = "input.DS_type == 'basic_DS'",
                                              selectInput("DS_label", "Labels", c("search raster labels"), multiple = TRUE)),
                             conditionalPanel(condition = "input.DS_type == 'generalization_DS'",
                                              selectInput("DS_training_label", "Training labels", c("search raster labels"), multiple = TRUE),
                                              selectInput("DS_training_label", "Testing labels", c("search raster labels"), multiple = TRUE)
                             )
                         )
                         
                         
                         
                         
                  ),
                  column(width = 2,
                         box(title = "Classifier",
                             width = NULL,
                             selectInput("CL", "Classifier", c("maximum correlation", "support vecotor machine", "poisson naive bayes"))),
                         box(title = "Feature preprocessors",
                             width = NULL,
                             conditionalPanel(condition  = "input.CL == 'poisson naive bayes'",
                                              selectInput("FP", "Feature Preprocessor", c("select_pvalue_significant_features","select or exclude top k...
                                                                         features"), multiple = TRUE)),
                             conditionalPanel(condition  = "input.CL == 'support vecotor machine'",
                                              selectInput("FP", "Feature Preprocessor", c("select_pvalue_significant_features","select or exclude top k...
                                                                                          features", "zscore_normalize"), multiple = TRUE)),
                             conditionalPanel(condition  = "input.CL == 'maximum correlation'",
                                              selectInput("FP", "Feature Preprocessor", c("select_pvalue_significant_features","select or exclude top k...
                                                                                          features", "zscore_normalize"), multiple = TRUE))
                             
                             
                         ),
                        
                         box(title = "Cross validator",
                             width = NULL,
                             numericInput("CV_repeat", "# of repeats", value = 2, min = 1),
                             numericInput("CV_split", "# of cv splits", value = 5, min = 2),
                             numericInput("CV_resample", "# of resampling runs", value = 20, min = 1),
                             radioButtons("bCV_diag", "test only at training times?", c("Yes", "No"))
                         ),
                         actionButton("run_decoding", "Run Decoding")
                  ),
                  column(width = 8,
                         #issue cannot make use of the large blank on the right 
                         tabBox(
                           title = "Result plot",
                           tabPanel("timeplot", 
                                    selectInput("Plot.TCT_result_type_to_plot", "Type of result to plot",
                                                c("Zero-one loss", "Rank results", "Decision Values")),
                                    plotOutput("timeplot")
                                    ),
                           tabPanel("TCT heatmap",
                                    selectInput("Plot.basic_result_type_to_plot", "Type of result to plot",
                                                c("Zero-one loss", "Rank results", "Decision Values")),
                                    plotOutput("tct")
                                    )
                         ))
                )
              )
              
              
              
              
      )
    ) 
  )
)

