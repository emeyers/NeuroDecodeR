library(shinydashboard)
# library(shinyAce)
# library(semantic.dashboard)


ui <- dashboardPage(
  dashboardHeader(title = "NDTr"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bining the raster data", tabName = "bin"),
      menuItem("Population Decoding", tabName = "decode"),
      menuItem("Single Neuron Analysis", tabName = "single")
      
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "bin",
              fluidPage(
                
                fluidRow(
                  column(width = 4,
                         box(width = NULL,
                             # radioButtons("bin_bUpload", "Choose raster data", c("Upload new data", "Use existing data")),
                             # conditionalPanel(condition = "input.bin_bUpload == 'Upload new data'",
                             #                  fileInput("bin_uploaded_raster", "Upload data", multiple = TRUE)),
                             # conditionalPanel(condition = "input.bin_bUpload == 'Use existing data'",
                             #                  uiOutput("bin_list_of_raster_files")),
                             fileInput("bin_uploaded_raster", "Upload new raster data (optional)", multiple = TRUE),
                             uiOutput("bin_list_of_raster_files"),
                             numericInput("bin_width", "Bin width", value = 10, min = 1),
                             numericInput("step_size", "Step size", value = 1, min = 1)
                         ),
                         actionButton("bin_bin_data", "Bin the data")
                         
                  ),
                  column(width = 8,
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
              navbarPage(title = "",
                         tabPanel(
                           title = "Specifing decoding papameters",
                           fluidPage(
                             
                             
                             
                             fluidRow(
                               column(width = 6,
                                      tabBox(width = 12,
                                             height = 1000,
                                             tabPanel("Twitch it here",
                                                      tabBox(width = 12,
                                                             height = 1000,
                                                             tabPanel(
                                                               "Main",
                                                               column(width = 6,
                                                                      box(title = "Data source",
                                                                          width = NULL,
                                                                          solidHeader = TRUE, status = "primary",
                                                                          # radioButtons("DS_bUpload", "Choose binned data", c("Upload new data", "Use existing data")),
                                                                          # conditionalPanel(condition = "input.DS_bUpload == 'Upload new data'",
                                                                          #                  fileInput("DS_uploaded_bin", "Upload data", multiple = TRUE)),
                                                                          # conditionalPanel(condition = "input.DS_bUpload == 'Use existing data'",
                                                                          #                  uiOutput("DS_list_of_binned_files")),
                                                                          fileInput("DS_uploaded_bin", "Upload new binned data (optional)", multiple = TRUE),
                                                                          uiOutput("DS_list_of_binned_files"),
                                                                          
                                                                          selectInput("DS_type", "Decoding type", c("basic_DS","generalization_DS")),
                                                                          
                                                                          
                                                                          
                                                                          conditionalPanel(condition = "input.DS_type == 'basic_DS'",
                                                                                           uiOutput("DS_basic_list_of_var_to_decode"),
                                                                                           
                                                                                           radioButtons("DS_bUse_all_labels", "Use all the labels?", c("Yes", "No"))
                                                                          ),
                                                                          conditionalPanel(condition = "input.DS_bUse_all_labels == 'No' && input.DS_type == 'basic_DS'",
                                                                                           uiOutput("DS_basic_list_of_levels_to_use")),
                                                                          
                                                                          
                                                                          conditionalPanel(condition = "input.DS_type == 'generalization_DS'",
                                                                                           uiOutput("DS_gen_list_of_var_to_decode"),
                                                                                           uiOutput("DS_gen_list_of_var_to_use"),
                                                                                           uiOutput("DS_gen_select_num_training_level_groups"),
                                                                                           uiOutput("DS_gen_list_of_training_level_groups"),
                                                                                           uiOutput("DS_gen_list_of_testing_level_groups")
                                                                          )
                                                                          
                                                                      )
                                                                      
                                                                      
                                                               ),
                                                               column(width = 6,
                                                                      box(title = "Classifier",
                                                                          width = NULL,
                                                                          solidHeader = TRUE, status = "primary",
                                                                          selectInput("CL", "Classifier", c("maximum correlation", "support vecotor machine", "poisson naive bayes"))),
                                                                      box(title = "Feature preprocessors",
                                                                          width = NULL,
                                                                          solidHeader = TRUE, status = "primary",
                                                                          radioButtons("FP_bUse", "Preprocessing Features?", c("Yes", "No")),
                                                                          conditionalPanel(condition = "input.FP_bUse == 'Yes'",
                                                                                           
                                                                                           conditionalPanel(condition  = "input.CL == 'poisson naive bayes'",
                                                                                                            selectInput("FP", "Feature Preprocessor", c("select_pvalue_significant_features","select or exclude top k...
                                                                                                                                                        features"), multiple = TRUE, selected = "select_pvalue_significant_features")),
                                                                                           conditionalPanel(condition  = "input.CL == 'support vecotor machine'",
                                                                                                            selectInput("FP", "Feature Preprocessor", c("select_pvalue_significant_features","select or exclude top k...
                                                                                                                                                        features", "zscore_normalize"), multiple = TRUE, selected = "select_pvalue_significant_features")),
                                                                                           conditionalPanel(condition  = "input.CL == 'maximum correlation'",
                                                                                                            selectInput("FP", "Feature Preprocessor", c("select_pvalue_significant_features","select or exclude top k...
                                                                                                                                                        features", "zscore_normalize"), multiple = TRUE, selected = "select_pvalue_significant_features"))
                                                                          )
                                                                          
                                                                      ),
                                                                      
                                                                      box(title = "Cross validator",
                                                                          width = NULL,
                                                                          solidHeader = TRUE, status = "primary",
                                                                          numericInput("CV_repeat", "# of repeats", value = 2, min = 1),
                                                                          numericInput("CV_split", "# of cv splits", value = 5, min = 2),
                                                                          numericInput("CV_resample", "# of resampling runs", value = 20, min = 1),
                                                                          radioButtons("bCV_diag", "test only at training times?", c("Yes", "No"))
                                                                      ),
                                                                      actionButton("DC_gen_decoding_script", "Generate script")
                                                               )
                                                             )
                                                             ,
                                                             
                                                             tabPanel("Set advanced parameters (optional)",
                                                                      
                                                                      conditionalPanel(condition  = "input.CL == 'support vecotor machine'",
                                                                                       selectInput("CL_SVM_kernel",
                                                                                                   "Kernel",
                                                                                                   c("linear", "polynomial", "radial", "sigmoid"),
                                                                                                   selected = "radial"),
                                                                                       numericInput("input.CL_SVM_cost",
                                                                                                    "Cost", # of constraints violation / inverse of regularization constant",
                                                                                                    1,
                                                                                                    min = 0
                                                                                       )),
                                                                      conditionalPanel(condition ="(input.CL == 'support vecotor machine')&&(input.CL_SVM_kernel == 'polynomial')",
                                                                                       numericInput("input.CL_SVM_degree",
                                                                                                    "Degree of polynomial",
                                                                                                    3,
                                                                                                    min = 2,
                                                                                                    max  = 10)),
                                                                      # numericInput("input.CL_SVM_coef0",
                                                                      #              "Set Constant in the kernel",
                                                                      #              0)),
                                                                      
                                                                      conditionalPanel(condition = "(input.CL == 'support vecotor machine')&&((input.CL_SVM_kernel == 'radial')|(input.CL_SVM_kernel == 'polynomial'))",
                                                                                       numericInput("input.CL_SVM_coef0",
                                                                                                    "Coef0", # Constant in the kernel function",
                                                                                                    0)),
                                                                      conditionalPanel(condition = "(input.CL == 'support vecotor machine')&&((input.CL_SVM_kernel == 'radial')|(input.CL_SVM_kernel == 'polynomial')|(input.CL_SVM_kernel == 'sigmoid'))",
                                                                                       uiOutput("CL_choose_gamma")
                                                                      )
                                                                      
                                                             ))),
                                             
                                             tabPanel("Use a script",
                                                      # radioButtons("DC_bUpload", "Choose decoding script", c("Upload new script", "Use existing script")),
                                                      # conditionalPanel(condition = "input.DC_bUpload == 'Upload new script'",
                                                      #                  fileInput("DC_upload", "Upload new script (optional)")),
                                                      # conditionalPanel(condition = "input.DC_bUpload == 'Use existing script'",
                                                      #                  selectInput("DC_our", "script available",c("search script dir"))),
                                                      fileInput("DC_upload", "Upload new script (optional)", multiple = TRUE),
                                                      uiOutput("DS_list_of_scripts"),
                                                      actionButton("DC_show", "Show script")
                                             )
                                             
                                             
                                             
                                      )),
                               column(width = 6,
                                      # htmlOutput(input$DS_script)
                                      uiOutput("DC_script_to_show")
                                      ,
                                      
                                      actionButton("DC_run_decoding", "Run Decoding")                                      
                               )
                             )
                           )
                         ),
                         tabPanel(
                           title = "Result plot",
                           
                           column(width = 12,
                                  #issue cannot make use of the large blank on the right 
                                  tabBox(width = 12,
                                         # title = "Result plot",
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
