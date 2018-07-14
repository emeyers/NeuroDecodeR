require('dplyr')
require('fields')
require('ggplot2')
require('stringr')


function(input, output, session) {
  
  observe({
    setwd("C:/Users/14868/Documents/GitHub/NDTr")
    load('data/binned/ZD_binned_data_150ms_bins_50ms_sampled.Rda')
    tempA <- input$DS_training_labels
a <- getwd()
print(a)
# tempB <- reactive_all_levels_of_var_to_use()
    if (is.null(tempA))
      tempA <- character(0)
    updateSelectInput(session,
                      "DS_testing_label",
                      label = "Testing labels",
                      choices  = tempA
                      # choices = tempB[!str_detect(tempB, tempA)]
                      # str_remove(reactive_all_levels_of_var_to_use(),temp)
    )
  })
  
  reactive_all_var <- reactive({
    sub("labels.", "", names(select(binned_data, starts_with("labels"))))
  })
  
  reactive_all_levels_of_var_to_use <- reactive({
    levels(factor(binned_data[[paste0("labels.",input$DS_var_to_use)]]))
    
  })
  # # labels has to be renamed stimulus.postiiton -> position
  # reactive_potential_training_var <- reactive({
  #   temp = reactive_all_var()
  #   temp[grep(paste0(".",".",input$DS_var,"."), reactive_all_var())
  #        ]
  # })
  # 
  output$bin_list_of_raster_files = renderUI(
    selectInput("bin_chosen_raster",
                "Choose your raster data",
                list.dirs('data/raster/', full.names = FALSE),
                selected = "Zhang_Desimone_7objects_R_raster_data"
                
    ))
  
  
  output$DS_list_of_binned_files = renderUI(
    selectInput("DS_chosen_bin",
                "Choose your binned data",
                list.files('data/binned/', "*.Rda"), 
                selected = ""
                
    ))
  
  
  output$DS_list_of_var = renderUI({
    a = getwd()
    print(a)
    print(input$DS_chosen_bin)
    input$DS_chosen_bin
    load(paste0('data/binned/', input$DS_chosen_bin))
    selectInput("DS_var",
                "Variable to decode",
                reactive_all_var())
    
  })
  
  output$DS_list_of_labels = renderUI({
    # load(paste0('data/binned/', input$DS_chosen_bin))
    selectInput("DS_label_to_use",
                "Labels to use",
                levels(factor(binned_data[[paste0("labels.",input$DS_var)]])),
                multiple = TRUE)
    
  })
  
  output$DS_list_of_potential_training_var = renderUI({
    selectInput("DS_var_to_use",
                "Variable to train with",
                reactive_all_var())
  })
  
  output$DS_list_of_training_labels = renderUI({
    selectInput("DS_training_label",
                "Training labels",
                reactive_all_levels_of_var_to_use(),
                multiple = TRUE
    )
  })
  
  # output$DS_list_of_testing_labels = renderUI({
  #   selectInput("DS_testing_label",
  #               "Testing labels",
  #               reactive_all_levels_of_var_to_use(),
  #               # str_replace(reactive_all_levels_of_var_to_use(),input$DS_training_labels, ""),
  #               multiple = TRUE)
  # })
}

