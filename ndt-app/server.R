# server.R


source("run_decoding.R")
source('create_decoding_markdown_file.R')


require('dplyr')
require('fields')
require('ggplot2')

# library('R6')    
# library('dplyr')
# library('reshape2')
# library('stringr')



# helper function to create multiple FP inputs
generate.FP.panel <- function(panel.number) {
  wellPanel(
    
    selectInput(paste0("FP.name", panel.number), "Feature preprocessor name", 
                c("zscore_FP", "select_k_features"))  #, 

    # conditionalPanel(
    #   condition = "input$FP.name1 == 'select_k_features'",
    #   numericInput(paste0("FP.num_features_to_use", 1), 
    #                "Number of features to use", value = -1) 
    # )
    #uiOutput("expand_feature_processor_parms")

    # conditionalPanel(
    #   condition = "input.FP.name1 == 'select_k_features'", 
    #   numericInput(paste0("FP.num_features_to_use", 1), 
    #                "Number of features to use", value = -1) 
    # )
    # 
    
    # come back to this later...
    
  )
}





shinyServer(function(input, output) {

  
  # run the decoding
  
  get_decoding_params <- eventReactive(input$runDecoding, {
    
    
    # do the decoding analysis...
    #run_decoding(input) 
    
    create_markdown_file(input)
    rmarkdown::render("markdown_files/decoding_markdown.Rmd")
    
    
    decoding.parameters <- data.frame(input$DS.name, input$DS.num_cv_splits)
    
    # as a test case, write the parameters to a file...
    
    #save("decoding.parameters", file = "results/testing_saving_parameters_junk_file.Rda")
    
    
  })
  
  
  output$display_results = renderText({
    
    get_decoding_params()
    
    return(paste('<iframe style="height:600px; width:100%" src="', "http://asterius.hampshire.edu:3838/research/NDTr_pdfs/decoding_markdown.pdf", '"></iframe>', sep = ""))
    
  })
  
  
  # output$display_results = renderTable({
  #  
  #  get_decoding_params()
  #  
  #})
  
  
  
  
  # ouputs for the Data Source
  
  # select the binned file to use
  output$list_of_binned_files = renderUI(
    selectInput("DS.binned_data_name", 
                "Binned data file name",  
                list.files('../data/', "*.Rda"), 
                selected = "ZD_binned_data_150ms_bins_50ms_sampled.Rda",
                selectize = FALSE)
  )
  
  
  # select the specific labels to use
  output$list_of_binned_label_names = renderUI({
   
    load(paste0('../data/', input$DS.binned_data_name))
    
    
    selectInput("DS.specific_binned_label_names", 
                "Binned label names", 
                sub("labels.", "", names(select(binned.data, starts_with("labels")))), 
                selectize = FALSE)
    
  })
  
  
  
  
  

  # outputs for the feature preprocessors
  
  output$feature_preprocessor_panel = renderUI({


    panel_string <- 'wellPanel('
    
    for (i in 1:input$FP.number){   
      panel_string <- paste0(panel_string, paste0('generate.FP.panel(', i, '),'))
    }
    
    
    # if no feature preprocessors are selected
    if (input$FP.number == 0)
      panel_string <- "No Feature preprocessors are being used"
    else{
      panel_string <- paste0(substr(panel_string, 1, nchar(panel_string) - 1), ')')
      eval(parse(text = panel_string))
    }
    

  })
  
  
  output$expand_feature_processor_parms = renderUI({
  
    
   #  
   # # if (paste0(input$FP.name, 1) == "select_k_features") {
   #  
   #    numericInput(paste0("FP.num_features_to_use", 1), 
   #             "Number of features to use", value = -1),    
   # 
   #    numericInput(paste0("FP.num_features_to_exclude", 1), 
   #             "Number of features to exclude", value = 0)
   #  
   #  #}
   #  
    
    
    numericInput(paste0("FP.num_features_to_use", 1), 
                 "Number of features to use", value = -1) 
    
  })
  
  
  
  
  # outputs for classifiers
  

  # SVM isn't currently implemented so dont' really need this yet...  
  output$additional_classifier_parameters = renderUI({
    
    if(input$CL.name == "Support Vector Machine") {
      selectInput("CL.kernal", "Kernel Type", c("Linear", "Polynomial", "RBF"))
    }

  })
  
  
  
  
  # outputs to plot the results
  
  
  output$tct_plot = renderPlot({
    
    load('results/curr_temp_results.Rda')
    
    
    if (input$Plot.TCT_result_type_to_plot == "Zero-one loss"){
      all.results <- DECODING_RESULTS$zero.one.loss.results
      labels_names <- "Classification Accuracy"
    }
    
    if (input$Plot.TCT_result_type_to_plot == "Rank results"){
      all.results <- DECODING_RESULTS$decision.value.results
      labels_names <- "Normalized rank"
    }
    
    if (input$Plot.TCT_result_type_to_plot == "Decision Values"){
      all.results <- DECODING_RESULTS$rank.results
      labels_names <- "Decision values"
    }
    
    
    
    # get the mean over CV splits
    mean.results <- colMeans(all.results)
    time.bin.names <- get.center.bin.time(dimnames(all.results)[[3]])
    
    
    # plot full TCT plot
    image.plot(time.bin.names, time.bin.names, mean.results, 
               legend.lab = labels_names, xlab = "Test time (ms)", 
               ylab = "Train time (ms)")
    abline(v = 0)
    
    
  })
  
  
  
  
  output$function_of_time_plot = renderPlot({
    
    #load('results/curr_temp_results.Rda')
    load('markdown_files/markdown_results/curr_temp_results.Rda')
    
    if (input$Plot.basic_result_type_to_plot == "Zero-one loss"){
      all.results <- DECODING_RESULTS$zero.one.loss.results
      labels_names <- "Classification Accuracy"
    }
    
    if (input$Plot.basic_result_type_to_plot == "Rank results"){
      all.results <- DECODING_RESULTS$decision.value.results
      labels_names <- "Normalized rank"
    }
    
    if (input$Plot.basic_result_type_to_plot == "Decision Values"){
      all.results <- DECODING_RESULTS$rank.results
      labels_names <- "Decision values"
    }
    
    
    
    # get the mean over CV splits
    mean.results <- colMeans(all.results)
    time.bin.names <- get.center.bin.time(dimnames(all.results)[[3]])
    
    
    # plot results as a function of time
    plot(time.bin.names, diag(mean.results), type = "l", xlab = "Time (ms)", ylab = labels_names)
    abline(v = 0)
    
    
    # darn, for some reason I am getting a memory error when I run this (works fine outside of shiny)
    
    # results_df <- data.frame(time = time.bin.names, results = diag(mean.results))
    
    # ggplot(results_df, aes(x = time, y = results)) + 
    #   geom_line()  + 
    #   xlab("Time (ms)") + 
    #   ylab(labels_names)
    
    
  })

  
  
  
})  # end shiny server code