


function(input, output, session) {
  
  # shinyFileChoose(input, 'files', root=c(root='.'), filetypes=c('', 'txt'))
  # shinyDirChoose(input, "bin_chosen_raster", roots = c(wd='.'))
  
  # shinyDirChoose(input, "bin_chosen_raster")
  
  # , filetypes = c("mat", "Rda")
  
  raster_base_dir <- 'data/raster'
  rv <- reactiveValues()
  
  rv$raster_base_dir <- raster_base_dir
  rv$raster_cur_dir_name <- NULL
  rv$raster_cur_neuron <- 1
  rv$raster_num_neuron <- NA
  rv$raster_cur_file_name <- NULL
  rv$raster_cur_data <- NULL
  rv$raster_bRda <- FALSE
  rv$raster_bMat <-FALSE
  
  
  shinyDirChoose(input, "bin_chosen_raster", roots = c(wd=raster_base_dir))
  
  # observe({
  #   if(!is.null(input$upload)){
  #     file.copy(input$upload$datapath, "tests")
  #     print("copy")
  #     
  #   }
  # })
  # 
  observe({
    req(input$bin_chosen_raster)
    # if(input$bin_bPlot){
    
    rv$raster_cur_dir_name <- parseDirPath(c(wd= file.path(eval(getwd()), rv$raster_base_dir)),input$bin_chosen_raster)
    temp_names_of_all_mat_files_in_raster_dir <- 
      list.files(rv$raster_cur_dir_name, pattern = "*.mat")
    # browser()
    if(length(temp_names_of_all_mat_files_in_raster_dir) > 0){
      rv$raster_bMat <- TRUE
      # print(isolate(rv$raster_bMat))
      print(rv$raster_bMat)
      # browser()
      
    } else {      
      temp_names_of_all_rda_files_in_raster_dir <- 
        list.files(rv$raster_cur_dir_name, pattern = "*.Rda")
      rv$raster_num_neuron <- length(temp_names_of_all_rda_files_in_raster_dir)
      print(c("yes", rv$raster_num_neuron))
      if(rv$raster_num_neuron > 0){
        rv$raster_bRda <- TRUE
      } else{
        validate("Only accept raster data in .mat or .Rda format")
      }
      print("yes1")
      rv$raster_cur_file_name <- temp_names_of_all_rda_files_in_raster_dir[rv$raster_cur_neuron]
      load(file.path(rv$raster_cur_dir_name, rv$raster_cur_file_name))
      rv$raster_cur_data <- select(raster_data, starts_with("time."))
      print("yes1")
    }
    
    
    
    
    # }
    
    
  })
  

  observeEvent(input$bin_bin_data,{
    if(rv$raster_bRda){
      print(input$bin_start_ind)
      temp_call = paste0("create_binned_data(input$bin_chosen_raster(),",
                         "input$bin_prefix_of_binned_file_name,",
                         "input$bin_bin_width, input$bin_step_size")
      if(!is.na(input$bin_start_ind)){
        temp_call = paste0(temp_call, ",input$bin_start_ind")
      }
      if(!is.na(input$bin_end_ind)){
        temp_call = paste0(temp_call, ",input$bin_end_ind")
      }
      temp_call = paste0(temp_call,")")
      print(temp_call)
      eval(parse(text = temp_call))
    } else{
      create_binned_data_from_matlab_raster_data(input$bin_chosen_raster(),
                                                 input$bin_prefix_of_binned_file_name,
                                                 input$bin_bin_width, input$bin_step_size)
      
    }
    
  })
  
  observeEvent(input$bin_create_raster,{
    create_raster_data_from_matlab_raster_data(rv$raster_cur_dir_name, input$bin_new_raster)
  })
  observeEvent(input$DC_scriptize,{
    temp_decoding_paras_id <<- c("CL", "CL_SVM_coef0", "CL_SVM_cost", "CL_SVM_degree",
                                 "CL_SVM_gamma", "CL_SVM_kernel", "CV_bDiag", "CV_repeat", "CV_resample",
                                 "CV_split", "CV_repeat", "CV_resample", "CV_split", "DS_basic_level_to_use", "DS_basic_var_to_decode", "DS_bUse_all_levels",
                                 "DS_chosen_bin", "DS_gen_num_training_level_groups", "DS_gen_var_to_decode",
                                 "DS_gen_var_to_use", "DS_type","FP", "FP_excluded_k",
                                 "FP_selected_k")
    if(!is.null(input$DS_gen_num_training_level_groups)){
      temp_training_level_groups <<- paste0("input$DS_training_level_group_", c(1:input$DS_gen_num_training_level_groups))
      temp_testing_level_groups <<- paste0("input$DS_testing_level_group_", c(1:input$DS_gen_num_testing_level_groups))
      temp_decoding_paras_id <<- c(temp_decoding_paras_id, trainin_level_groups, testing_level_groups)
    } 
    
    
    # my_decoding_paras <<- paste0("my_",decoding_paras)
    # all_input <<- names(input)
    
    #   temp_need = lapply(req_dc_para, function(i){
    #     print(eval(parse(text = paste0("!is.null(input$",i,")"))))
    #     # when needed thing exist, it returns NULL
    #     # https://github.com/rstudio/shiny/blob/master/R/utils.R
    #     eval(parse(text = paste0("need(!is.null(input$",i,"),'bang')")))
    #   })
    #   
    #   output$DC_scriptize_error <- renderText({
    #     # do.call(validate, temp_need)
    #     # eval(parse(text = temp_val))
    #     rv$script <- create_script(input)
    #     print(rv$script)
    #   })
    #   
    #   # do.call(validate, temp)
    #   
    temp_decoding_paras <- lapply(temp_decoding_paras_input_id, function(i){
      eval(parse(text = i))
    })
    
    print(temp_decoding_paras)
    lDecoding_paras <<- as.list(temp_decoding_paras)
    lDecoding_paras <<- setNames(lDecoding_paras, temp_decoding_paras_id)
    
    print(lDecoding_paras)
    print(lDecoding_paras$CL)
    rv$script <- create_script(lDecoding_paras)
    
  })
  
  
  
  observeEvent(input$DC_run_decoding,{
    eval(parse(text = rv$script))
  })
  
  observeEvent(input$bin_pre_neuron,{
    if(rv$raster_cur_neuron > 1){
      rv$raster_cur_neuron <- rv$raster_cur_neuron - 1
      # print("pre")
      # print(rv$raster_cur_neuron)
      
    }
    
  })
  
  observeEvent(input$bin_next_neuron,{
    if(rv$raster_cur_neuron < rv$raster_num_neuron){
      rv$raster_cur_neuron <- rv$raster_cur_neuron + 1
      # print(rv$raster_num_neuron)
      # print("next")
      # print(rv$raster_cur_neuron)
      
    }
  })
  
  
  
  reactive_validate_for_scriptizing <- reactive({
    
  })
  
  
  reactive_bin_num_neuron <- reactive({
    
    validate(
      need(input$DS_chosen_bin,"Please select data source first to get total number of neurons!")
    )
    binned_data = reactive_binned_data()
    length(unique(factor(binned_data$siteID)))
  })
  
  reactive_maximum_num_of_levels_in_all_var <- reactive({
    binned_data = reactive_binned_data()
    temp_all = apply(select(binned_data, starts_with("labels"))[,],2, function(x) length(levels(as.factor(x))))
    max(temp_all)
    
  })
  reactive_binned_data <- reactive({
    req(input$DS_chosen_bin)
    # if(!is.null(input$DS_chosen_bin)){
    # the above was commented out because every function calls me has either done the above check if it's initilized...
    # before the "select_input$DS_chosen_bin" funciton was called or it's not initialized until that thing is called...
    # The reason input$DS_chosen_bin is initialized as null is that thing is not on the gui when the app starts and..
    # not called until it shows on the gui
    get(load(paste0('data/binned/', input$DS_chosen_bin)))
    # }
    
  })
  
  reactive_all_var <- reactive({
    # if(!is.null(input$DS_chosen_bin)){
    binned_data = reactive_binned_data()
    
    sub("labels.", "", names(select(binned_data, starts_with("labels"))))
    # }
    
  })
  
  reactive_all_levels_of_basic_var_to_decode <- reactive({
    # if(!is.null(input$DS_chosen_bin)){
    
    binned_data = reactive_binned_data()
    print(head(binned_data))
    print(input$DS_var_to_decode)
    levels(factor(binned_data[[paste0("labels.",input$DS_basic_var_to_decode)]]))
    
    # }
  })
  
  reactive_all_levels_of_gen_var_to_use <- reactive({
    # if(!is.null(input$DS_chosen_bin)){
    
    binned_data = reactive_binned_data()
    print(head(binned_data))
    print(input$DS_var_to_decode)
    levels(factor(binned_data[[paste0("labels.",input$DS_gen_var_to_use)]]))
    
    # }
  })
  
  reactive_all_fp_avail <- reactive({
    req(input$CL)
    all_fp[df_cl_fp[,input$CL]>0]
  })
  
  
  
  output$where = renderDataTable(input$bin_uploaded_raster)
  
  output$bin_offer_create_raster = renderUI({
print("fuck")
    req(input$bin_chosen_raster)
    print(paste0("rv", rv))
    if(rv$raster_bMat){
      checkboxInput("bin_bCreate_raster_in_rda",lLabel$bin_bCreate_raster_in_rda)
    }
  })
  

  output$bin_prep_create_raster = renderUI({
    req(rv$raster_cur_dir_name)
    temp_matlab_raster_dir_name <- rv$raster_cur_dir_name
    # if the directory name ends with _mat, remove _mat
    temp_non_desired_pattern = '.*_mat$'
    if (grepl(temp_non_desired_pattern, temp_matlab_raster_dir_name) == TRUE){
      temp_r_raster_dir_name <- substr(temp_matlab_raster_dir_name, 1, nchar(temp_matlab_raster_dir_name) - 4)
    } 
    
    # append Rda
    temp_r_raster_dir_name <- paste0(temp_r_raster_dir_name, "_rda/")
    
    list(
      textInput("bin_new_raster", lLabel$bin_new_raster, temp_r_raster_dir_name),
         
         actionButton("bin_create_raster", lLabel$bin_create_raster))

    
  })
  

  
  
  output$bin_show_chosen_raster = renderText({
    # temp_text = "Chose raster"
    # rv$raster_cur_dir_name <- parseDirPath(c(wd=eval(getwd())),input$bin_chosen_raster)
    rv$raster_cur_dir_name
  })
  
  output$bin_show_raster_cur_file_name = renderText({
    paste0("current data shown:", "\n", rv$raster_cur_file_name)
    
  })
  
  output$bin_raster_plot = renderPlot({
    head(rv$raster_cur_data)
    req(rv$raster_cur_data)
    temp_raster <-rv$raster_cur_data 
    
    color2D.matplot(1 - temp_raster, border = NA, xlab = "Time (ms)",
                    ylab = "Trial")
  })
  
  output$bin_PSTH = renderPlot({
    req(rv$raster_cur_data)
    
    temp_raster <- rv$raster_cur_data
    plot(colSums(temp_raster, na.rm = FALSE, dims = 1)/nrow(temp_raster),
         xlab = "Time(ms)", ylab = "average firing rate")
  })
  
  output$DS_list_of_binned_files = renderUI({
    selectInput("DS_chosen_bin",
                lLabel$DS_chosen_bin,
                list.files('data/binned/', "*.Rda"),
                selected = "ZD_binned_data_150ms_bins_50ms_sampled.Rda"
                
    )
  })
  
  
  output$DS_basic_list_of_var_to_decode = renderUI({
    selectInput("DS_basic_var_to_decode",
                lLabel$DS_basic_var_to_decode,
                reactive_all_var()
                # c("")
                
    )
    
  })
  
  output$DS_gen_list_of_var_to_decode = renderUI({
    selectInput("DS_gen_var_to_decode",
                lLabel$DS_gen_var_to_decode,
                reactive_all_var()
                # c("")
                
    )
    
  })
  
  output$DS_basic_list_of_levels_to_use = renderUI({
    # load(paste0('data/binned/', input$DS_chosen_bin))
    
    selectInput("DS_basic_level_to_use",
                lLabel$DS_basic_level_to_use,
                reactive_all_levels_of_basic_var_to_decode(),
                multiple = TRUE)
    
  })
  # 
  output$DS_gen_list_of_var_to_use = renderUI({
    selectInput("DS_gen_var_to_use",
                lLabel$DS_gen_var_to_use,
                reactive_all_var())
  })
  
  output$DS_gen_select_num_of_groups = renderUI({
    temp_max <- reactive_maximum_num_of_levels_in_all_var()
    numericInput("DS_gen_num_training_level_groups",
                 lLabel$DS_gen_num_training_level_groups,
                 1,
                 min = 1,
                 max  = temp_max)
    # print(temp_max)
  })
  
  output$DS_gen_list_of_training_level_groups = renderUI({
    req(input$DS_gen_num_training_level_groups)
    temp_num <- input$DS_gen_num_training_level_groups
    # print(temp_num)
    # if(!is.null(temp_num)){
    temp_output <- lapply(1:temp_num, function(i){
      list(selectInput(paste0("DS_training_level_group_", i),
                       paste("Training level group", i),
                       reactive_all_levels_of_gen_var_to_use(),
                       multiple = TRUE
      ),
      selectInput(paste0("DS_testing_level_group_", i),
                  paste("Testing level group", i),
                  reactive_all_levels_of_gen_var_to_use(),
                  multiple = TRUE
      ))
      
      
    })
    # print(temp_output)
    temp_output <- unlist(temp_output, recursive = FALSE)
    # output <- do.call(c, unlist(temp_output, recursive=FALSE))
    # print(output)
    temp_output
    # }
    
    
  })
  
  
  
  
  
  
  
  output$FP_check_fp = renderUI({
    checkboxGroupInput("FP",
                       lLabel$FP,
                       reactive_all_fp_avail()
    )
  }
  )
  
  output$FP_select_k_features = renderUI({
    if(sum(grepl('select or exclude top k features', input$FP))){
      numericInput("FP_selected_k",
                   lLabel$FP_selected_k,
                   1,
                   min = 1,
                   max = reactive_bin_num_neuron())
      
    }
    
    
    
    
  })
  
  output$FP_exclude_k_features = renderUI({
    
    req(input$FP_selected_k)
    numericInput("FP_excluded_k",
                 lLabel$FP_excluded_k,
                 1,
                 min = 1,
                 max = reactive_bin_num_neuron() - input$FP_selected_k)
  })
  
  
  output$DC_ace = renderUI({
    aceEditor("script",
              rv$script,
              mode = "r")
    # mode = "markdown")
    
    # check all inputs and poentially send error message !
    
  })
  # output$DC_list_of_scripts = renderUI({
  #   # list( 
  #     selectInput("DC_script",
  #                        "Chosse an existing script to show",
  #                        list.files('tests', "*.R")
  #   )#,
  #   # actionButton("DC_show", "Show script")
  #   # )
  # 
  #   
  # })
  # output$DC_script_to_show = renderUI({
  #   htmlOutput("input$DC_script")
  # })
}



