function(input, output, session) {
  output$bin_list_of_raster_files = renderUI(
    selectInput("bin_chosen_raster",
                "Data available",
                list.dirs('../data/raster', full.names = FALSE),
                selected = "Zhang_Desimone_7objects_R_raster_data"
                
    ))
  
  
  output$DS_list_of_binned_files = renderUI(
    selectInput("DS_chosen_bin",
                "data available",
                list.files('../data/binned', "*.Rda"), 
                selected = "ZD_binned_data_150ms_bins_50ms_sampled.Rda"
                
  ))
  
  # most recently chosen/uploaded bin/raster goes to bin_using_raster/DS_using_bin
  
  output$list_of_DS_var = renderUI({
    load(paste0('../data/binned/', input$DS_chosen_bin))
    selectInput("DS_var",
                "Variable to decoded",
                sub("labels.", "", names(select(binned_data, starts_with("labels")))))

  }

  )
}