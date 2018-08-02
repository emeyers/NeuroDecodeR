library(shinydashboard)
library(shinyAce)
library(shinyFiles)
# library(semantic.dashboard)


ui <- dashboardPage(
  dashboardHeader(title = "NDTr"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bining the raster data", tabName = "bin"),
      menuItem("Population Decoding", tabName = "decode")#,
      # menuItem("Single Neuron Analysis", tabName = "single")
      
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "bin",
              navbarPage(title = "",
                         tabPanel(
                           title = "Choose raster data",
                           fluidPage(
                             fluidRow(
                               column(width = 4,
                                      box(width = NULL,
                                          shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE),
                                          
                                          fileInput("bin_uploaded_raster", lLabel$bin_uploaded_raster, multiple = TRUE),
                                          textInput("bin_raster_base_dir", lLabel$bin_raster_base_dir, 'data/raster/'),
                                          uiOutput("bin_list_of_raster_dirs"),
                                          checkboxInput("bin_bPlot", lLabel$bin_bPlot),
                                          conditionalPanel(condition = "input.bin_bPlot",
                                                           actionButton("bin_pre_neuron", lLabel$bin_pre_neuron),
                                                           actionButton("bin_next_neuron", lLabel$bin_next_neuron),
                                                           textOutput("bin_cur_neuron")),
                                          dataTableOutput('where')
                                      )
                               ),
                               column(width = 8,
                                      
                                      
                                      box(width = NULL,
                                          title = "Raster plot",
                                          color = "green", ribbon = TRUE, title_side = "top right",
                                          conditionalPanel(condition = "input.bin_bPlot",
                                                           
                                                           plotOutput("bin_raster_plot"))
                                          
                                      ),
                                      box(width = NULL,
                                          title = "PSTH (Peristimulus time histogram)",
                                          color = "red", ribbon = TRUE, title_side = "top right",
                                          conditionalPanel(condition = "input.bin_bPlot",
                                                           
                                                           plotOutput("bin_PSTH"))
                                          
                                      )
                               )
                             )
                           )
                         ),
                         
                         
                         
                         tabPanel(
                           title = "Specifing binnnig parameters",
                           fluidPage(
                             
                             fluidRow(
                               column(width = 8,
                                      box(width = NULL,
                                          uiOutput("bin_offer_create_raster"),
                                          conditionalPanel(condition = "input.bin_bCreate_raster",
                                                           textInput("bin_new_raster", lLabel$bin_prefix_of_new_raster),
                                                           actionButton("bin_create_raster", lLabel$bin_create_raster)),
                                          numericInput("bin_bin_width", lLabel$bin_bin_width, value = 10, min = 1),
                                          numericInput("bin_step_size", lLabel$bin_step_size, value = 1, min = 1),
                                          numericInput("bin_start_ind", lLabel$bin_start_ind, value = NULL),
                                          numericInput("bin_end_ind", lLabel$bin_end_ind, value = NULL),
                                          actionButton("bin_bin_data", lLabel$bin_bin_data)
                                      )
                                      
                                      
                                      
                               )
                               
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
                                             
                                             
                                             tabPanel(
                                               title = "Data source",
                                               width = NULL,
                                               solidHeader = TRUE, status = "primary",
                                               fileInput("DS_uploaded_bin", lLabel$DS_uploaded_bin, multiple = TRUE),
                                               uiOutput("DS_list_of_binned_files"),
                                               
                                               selectInput("DS_type", lLabel$DS_type, c("basic_DS","generalization_DS")),
                                               
                                               
                                               
                                               conditionalPanel(condition = "input.DS_type == 'basic_DS'",
                                                                uiOutput("DS_basic_list_of_var_to_decode"),
                                                                
                                                                checkboxInput("DS_bUse_all_levels", "Use all the levels of this variable?", TRUE)
                                               ),
                                               conditionalPanel(condition = "!input.DS_bUse_all_levels && input.DS_type == 'basic_DS'",
                                                                uiOutput("DS_basic_list_of_levels_to_use")),
                                               
                                               
                                               conditionalPanel(condition = "input.DS_type == 'generalization_DS'",
                                                                uiOutput("DS_gen_list_of_var_to_decode"),
                                                                uiOutput("DS_gen_list_of_var_to_use"),
                                                                uiOutput("DS_gen_select_num_of_groups"),
                                                                uiOutput("DS_gen_list_of_training_level_groups"),
                                                                uiOutput("DS_gen_list_of_testing_level_groups")
                                               )
                                             ),
                                             
                                             tabPanel(
                                               title = "Classifier",
                                               width = NULL,
                                               solidHeader = TRUE, status = "primary",
                                               selectInput("CL", "Classifier", all_cl),
                                               box(
                                                 width = NULL,
                                                 title = "Optional setting",
                                                 conditionalPanel(condition  = "input.CL == 'support vecotor machine'",
                                                                  selectInput("CL_SVM_kernel",
                                                                              lLabel$CL_SVM_kernel,
                                                                              c("linear", "polynomial", "radial", "sigmoid"),
                                                                              selected = "radial"),
                                                                  numericInput("CL_SVM_cost",
                                                                               lLabel$CL_SVM_cost, # of constraints violation / inverse of regularization constant",
                                                                               1,
                                                                               min = 0
                                                                  ),
                                                                  conditionalPanel(condition ="input.CL_SVM_kernel == 'polynomial'",
                                                                                   numericInput("CL_SVM_degree",
                                                                                                lLabel$CL_SVM_degree,
                                                                                                3,
                                                                                                min = 2,
                                                                                                max  = 10)),
                                                                  
                                                                  
                                                                  conditionalPanel(condition = "input.CL_SVM_kernel == 'radial'|input.CL_SVM_kernel == 'polynomial'",
                                                                                   numericInput("CL_SVM_coef0",
                                                                                                lLabel$CL_SVM_coef0, # Constant in the kernel function",
                                                                                                0)),
                                                                  conditionalPanel(condition = "input.CL_SVM_kernel == 'radial'|input.CL_SVM_kernel == 'polynomial'|input.CL_SVM_kernel == 'sigmoid'",
                                                                                   numericInput("CL_SVM_gamma",
                                                                                                lLabel$CL_SVM_gamma,
                                                                                                NULL) 
                                                                  )
                                                 ))
                                               
                                             ),
                                             tabPanel(
                                               title = "Feature preprocessors",
                                               width = NULL,
                                               solidHeader = TRUE, status = "primary",
                                               
                                               uiOutput("FP_check_fp"),
                                               uiOutput("FP_select_k_features"),
                                               uiOutput("FP_exclude_k_features")
                                               
                                               
                                             ),
                                             tabPanel(
                                               title = "Cross validator",
                                               width = NULL,
                                               solidHeader = TRUE, status = "primary",
                                               numericInput("CV_repeat", lLabel$CV_repeat, value = 2, min = 1),
                                               numericInput("CV_split", lLabel$CV_split, value = 5, min = 2),
                                               numericInput("CV_resample", lLabel$CV_resample, value = 20, min = 1),
                                               checkboxInput("CV_bDiag", lLabel$CV_bDiag,TRUE)
                                             ),
                                             tabPanel("Script",
                                                      fileInput("DC_upload", lLabel$DC_upload, multiple = TRUE),
                                                      # script will show upon chosen
                                                      uiOutput("DC_list_of_scripts"),
                                                      actionButton("DC_scriptize", "generate script from gui configuration"),
                                                      uiOutput("DC_scriptize_error")
                                             )
                                             
                                             
                                             
                                      )),
                               
                               
                               
                               
                               
                               
                               
                               
                               column(width = 6,
                                      uiOutput("DC_ace"),
                                      
                                      actionButton("DC_run_decoding", "Run Decoding"),
                                      # textinput of filename to be saved if not existing and to be saved as if existing; 
                                      actionButton("DC_save_script", "Save the script")
                                      
                               )
                               
                               
                             )
                           )
                           
                         ),
                         tabPanel(
                           title = "Plot decoding results",
                           
                           column(width = 12,
                                  #issue cannot make use of the large blank on the right 
                                  tabBox(width = 12,
                                         # title = "Result plot",
                                         tabPanel("timeplot", 
                                                  selectInput("Plot_TCT_result_type_to_plot", lLabel$Plot_TCT_result_type_to_plot,
                                                              c("Zero-one loss", "Rank results", "Decision Values")),
                                                  plotOutput("timeplot")
                                         ),
                                         tabPanel("TCT heatmap",
                                                  selectInput("Plot_basic_result_type_to_plot", lLabel$Plot_basic_result_type_to_plot,
                                                              c("Zero-one loss", "Rank results", "Decision Values")),
                                                  plotOutput("tct")
                                         )
                                         
                                  )
                           )
                         )
              ) 
      )
      
    )
    
    
  )
  
)

