library(shiny)

shinyUI(navbarPage(id = "NDT_UI", title = "NDT",

  tabPanel(title = "Run decoding",
            fileInput("upload_data", "Upload data"),
            uiOutput("list_of_binned_files"),
            actionButton("runDecoding", "Run Decoding"),
            br(), br(),
            htmlOutput("display_results")
            #tableOutput("display_results")
  ),
  
                                      
  tabPanel(title = "Data Source",
      selectInput("DS.name", "Data source name", c("basic_DS")),     
      uiOutput("list_of_binned_label_names"),
      #textInput("DS.specific_binned_label_names", "Binned label names"),
      numericInput("DS.num_cv_splits", "Number of CV splits", value = 5, min = 2),
      numericInput("DS.num_repeats_per_cv_split", "Number of repeated labels per split", value = 1, min = 1)
  ),

  
  tabPanel(title = "Feature Preprocessors",
           numericInput("FP.number", "Number of Feature Preprocessors", 1, min = 0),
           uiOutput("feature_preprocessor_panel")
  ),  
  
  tabPanel(title = "Classifiers",
           #selectInput("CL.name", "Classifier Type", c("Maximum Correlation", "Poisson Naive Bayes", "Support Vector Machine")),
           selectInput("CL.name", "Classifier Type", c("Maximum Correlation", "Poisson Naive Bayes")),
           uiOutput("additional_classifier_parameters")
           
  ),    
  
  tabPanel(title = "Cross-Validators",
      numericInput("CV.num_resample_runs", "Number of resample runs", value = 2, min = 1)    
  ), 
  
  
 navbarMenu("Plot Results",
   
   tabPanel(title = "TCT plots",
           selectInput("Plot.TCT_result_file", "File to plot results", c("A", "B")),
           selectInput("Plot.TCT_result_type_to_plot", "Type of result to plot", c("Zero-one loss", "Rank results", "Decision Values")),
           imageOutput("tct_plot", width = 500, height = 500)
  ), 
 
  tabPanel(title = "Plots as function of time",
           selectInput("Plot.basic_result_file", "File to plot results", c("A", "B")),
           selectInput("Plot.basic_result_type_to_plot", "Type of result to plot", c("Zero-one loss", "Rank results", "Decision Values")),
           imageOutput("function_of_time_plot", width = 500, height = 500)
                   
  )
  
  
 )
  
                   
))





