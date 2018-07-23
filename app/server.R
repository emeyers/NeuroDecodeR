
require('dplyr')
require('fields')
require('ggplot2')
require('stringr')

setwd("C:/Users/14868/Documents/GitHub/NDTr")


function(input, output, session) {
  
  # observe({
  #   setwd("C:/Users/14868/Documents/GitHub/NDTr")
  #   # load('data/binned/ZD_binned_data_150ms_bins_50ms_sampled.Rda')
  #   # print(head(binned_data))
  #   tempA <- input$DS_training_labels
  #   a <- getwd()
  #   print(paste0("a", a))
  #   # tempB <- reactive_all_levels_of_var_to_use()
  #   if (is.null(tempA))
  #     tempA <- character(0)
  #   updateSelectInput(session,
  #                     "DS_testing_label",
  #                     label = "Testing labels",
  #                     choices  = tempA
  #                     # choices = tempB[!str_detect(tempB, tempA)]
  #                     # str_remove(reactive_all_levels_of_var_to_use(),temp)
  #   )
  # })
  reactive_data_dim <- reactive({
    binned_data = reactive_binned_data()
    nrow(binned_data)
  })
  
  reactive_maximum_num_of_levels_in_all_var <- reactive({
    binned_data = reactive_binned_data()
    temp_all = apply(select(binned_data, starts_with("labels"))[,],2, function(x) length(levels(as.factor(x))))
    max(temp_all)
    
  })
  reactive_binned_data <- reactive({
    # if(!is.null(input$DS_chosen_bin)){
    # the above was commented out because every function calls me has either done the above check if it's initilized...
    # before the "select_input$DS_chosen_bin" funciton was called or it's not initialized until that thing is called...
    # The reason input$DS_chosen_bin is initialized as null is that thing is not on the gui when the app starts and..
    # not called until it shows on the gui
    get(load(paste0('data/binned/', input$DS_chosen_bin)))
    
    # }
    
  })
  
  reactive_all_var <- reactive({
    if(!is.null(input$DS_chosen_bin)){
      binned_data = reactive_binned_data()
      
      sub("labels.", "", names(select(binned_data, starts_with("labels"))))
    }
    
  })
  
  reactive_all_levels_of_basic_var_to_decode <- reactive({
    if(!is.null(input$DS_chosen_bin)){
      
      binned_data = reactive_binned_data()
      print(head(binned_data))
      print(input$DS_var_to_decode)
      levels(factor(binned_data[[paste0("labels.",input$DS_basic_var_to_decode)]]))
      
    }
  })
  
  reactive_all_levels_of_gen_var_to_use <- reactive({
    if(!is.null(input$DS_chosen_bin)){
      
      binned_data = reactive_binned_data()
      print(head(binned_data))
      print(input$DS_var_to_decode)
      levels(factor(binned_data[[paste0("labels.",input$DS_gen_var_to_use)]]))
      
    }
  })
  
  # observeEvent(input$DS_chosen_bin,{
  #   # tempA <- 
  #   # print(paste0('tempA', tempA))
  #   print(paste0("HERRE", input$DS_chosen_bin))
  #   print(is.null(input$DS_chosen_bin))
  #   
  #   if (!is.null(input$DS_chosen_bin)){
  #     # binned_data = reactive_binned_data()
  #     # print( tempA)
  #     # print(paste0("raster", input$DS_bin_chosen_raster))
  #     # print(is.null(tempA))
  #     # print(input$DS_testing_label)
  #     # print(list.dirs('data/raster/'))
  #     updateSelectInput(session,
  #                       "DS_var_to_decode",
  #                       choices = reactive_all_var()
  #     )
  # 
  # 
  # 
  # }
  # })
  #
  
  # observeEvent(input$DS_var_to_decode,{
  #   if (!is.null(input$DS_chosen_bin)){
  #     
  #   updateSelectInput(session,
  #                     "DS_label_to_use",
  #                     choices = reactive_all_levels_of_var_to_use())
  # }
  # )
  
  
  
  
  
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
                list.files('data/binned/', "*.Rda")
                # selected = "ZD_binned_data_150ms_bins_50ms_sampled.Rda"
                
    ))
  
  
  output$DS_basic_list_of_var_to_decode = renderUI({
    selectInput("DS_basic_var_to_decode",
                "Variable to decode/use",
                reactive_all_var()
                # c("")
                
    )
    
  })
  
  output$DS_gen_list_of_var_to_decode = renderUI({
    selectInput("DS_gen_var_to_decode",
                "Variable to decode",
                reactive_all_var()
                # c("")
                
    )
    
  })
  
  output$DS_basic_list_of_levels_to_use = renderUI({
    # load(paste0('data/binned/', input$DS_chosen_bin))
    
    selectInput("DS_basic_level_to_use",
                "Labels to use",
                reactive_all_levels_of_basic_var_to_decode(),
                multiple = TRUE)
    
  })
  # 
  output$DS_gen_list_of_var_to_use = renderUI({
    selectInput("DS_gen_var_to_use",
                "Variable to train with",
                reactive_all_var())
  })
  
  output$DS_gen_select_num_training_level_groups = renderUI({
    temp_max <- reactive_maximum_num_of_levels_in_all_var()
    numericInput("DS_gen_num_training_level_groups",
                 "How many training level groups you will use?",
                 1,
                 min = 1,
                 max  = temp_max
    )
    # print(temp_max)
    })
  output$DS_gen_list_of_training_level_groups = renderUI({
    temp_num <- input$DS_gen_num_training_level_groups
    # print(temp_num)
    if(!is.null(temp_num)){
      lapply(1:temp_num, function(i){
        selectInput(paste0("DS_training_level_group_", i),
                    paste("Training level group", i),
                    reactive_all_levels_of_gen_var_to_use(),
                    multiple = TRUE    
        )
        
      })
    }
 

  })
  
  output$DS_gen_list_of_testing_level_groups = renderUI({
    selectInput("DS_gen_testing_level_group",
                "Testing level group",
                reactive_all_levels_of_gen_var_to_use(),
                multiple = TRUE)
  })
  

  
  output$CL_choose_gamma = renderUI({
    numericInput("CL_SVM_gamma",
                 "Gamma",
                 1/reactive_data_dim())
    
  })
  
  output$DS_list_of_scripts = renderUI({
    selectInput("DC_script",
                "Chosse your script",
                list.files('tests', ".R")
    )
  })
  output$DC_script_to_show = renderUI({
    htmlOutput("input$DC_script")
  })
}
