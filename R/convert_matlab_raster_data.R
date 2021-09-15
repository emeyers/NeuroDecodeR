#' Convert raster data in MATLAB to R
#'
#' If one already has raster data created in MATLAB (.mat files), this function
#' can be used to convert it to an R format (.rda files) that can be used with
#' the NDR.
#'
#' @param matlab_raster_dir_name A character string specifying the path to a
#'   directory that contains raster data in MATLAB .mat files.
#'
#' @param r_raster_dir_name A character string specifying the path to a
#'   directory where the converted raster data in R .rda files will be saved. If
#'   this is not specified then the saved directory will have the same name as
#'   the matlab directory with _rda appended to the end of the directory name.
#'   
#' @param sampling_interval_width A number specifying how successive time bins
#'   will be labeled The default value of 1 means that points will be labeled as
#'   successive integers; i.e., time.1_2, time.2_3, etc. If this value was set
#'   to a larger number, then time points will be specified at the given
#'   sampling width. From example, if sampling_width is set to 10, then the time
#'   labels would be time.1_10, time.10_20, etc. This is useful if the data is
#'   sampled at a particular rate (e.g., if the data is sampled at 500Hz, one
#'   might want to use sampling_interval_width = 2, so that the times listed on
#'   the raster column names are in milliseconds).
#' 
#' @param start_ind A number specifying the start index for the data to be
#'   converted if one wants to convert the data from a shorter time window than
#'   the original MATLAB raster data. The default (NULL value) is to use all the
#'   data, i.e., start at the beginning with start_ind = 1.
#'
#' @param end_ind A number specifying the end index for the data to be converted
#'   if one wants to convert the data from a shorter time window than the
#'   original MATLAB raster data. The default (NULL value) is to use all the
#'   data, i.e., end value is the last time point.
#'   
#' @param zero_time_bin A number specifying the time bin that should be marked
#'    as time 0. The default (NULL value) is to use the first bin as time 1.
#'   
#' @param files_contain A string specifying that only a subset of the MATLAB
#'   raster data should be converted based on .mat files that contain this
#'   string.
#'
#' @param add_sequential_trial_numbers A Boolean specifying one should add a
#'   variable to the data called 'trial_number' that has sequential trial. These
#'   trials numbers are needed for data that was recorded simultaneously so that
#'   trials can be aligned across different sites.
#'
#' @examples
#' matlab_raster_dir_name <- file.path(
#'   system.file("extdata", package = "NeuroDecodeR"),
#'   "Zhang_Desimone_7object_raster_data_small_mat"
#' )
#'
#' # create temporary directory to hold converted data
#' r_raster_dir_name <- tempdir()
#' r_raster_dir_name <- convert_matlab_raster_data(matlab_raster_dir_name,
#'   r_raster_dir_name,
#'   files_contain = "bp1001spk"
#' )
#'
#'
#' @export
convert_matlab_raster_data <- function(matlab_raster_dir_name,
                                       r_raster_dir_name = NULL,
                                       sampling_interval_width = 1,
                                       start_ind = NULL,
                                       end_ind = NULL,
                                       zero_time_bin = NULL,
                                       files_contain = "",
                                       add_sequential_trial_numbers = FALSE) {


  # if matlab directory name ends with a slash, remove this slash
  matlab_raster_dir_name <- paste0(
    trimws(file.path(dirname(matlab_raster_dir_name), " ")),
    basename(matlab_raster_dir_name)
  )


  matlab_file_names <- list.files(matlab_raster_dir_name, pattern = files_contain)



  for (iSite in seq_along(matlab_file_names)) {

    # print message to show progress the number of sites that have been converted    
    if (iSite == 1) {
      message(sprintf("converting site %-5s", iSite))
    } else {
      message(paste0(rep("\b", 22), collapse = ""), sprintf("converting site %-5s", iSite))
    }

    # first, load in raster as a list
    curr_matlab_file_name <- matlab_file_names[iSite]

    # replace .mat with .rda for matlab_raster_directory_name
    curr_r_file_name <- paste0(substr(curr_matlab_file_name, 1, nchar(curr_matlab_file_name) - 3), "rda")

    raster <- R.matlab::readMat(file.path(matlab_raster_dir_name, curr_matlab_file_name))


    # second, create the raster_site_info list
    raster_site_info <- raster$raster.site.info[, , 1]


    # find if any of the site_info is missing values and add them as NAs
    if (sum(sapply(lapply(raster_site_info, dim), function(x) x[[1]]) == 0)) {
      raster_site_info[sapply(lapply(raster_site_info, dim), function(x) x[[1]]) == 0][[1]] <- matrix(NA)
    }

    # convert the raster site info to a data frame and add it to the raster data
    raster_site_info_df <- as.data.frame(raster_site_info)


    # if there are more then one row to site_info_df, flatten it to a single row
    if (dim(raster_site_info_df)[1] > 1) {

      all_rows_same_vals <- sapply(raster_site_info_df, n_distinct)
      raster_site_info_df_diff_row_vals <- dplyr::select(raster_site_info_df, which(all_rows_same_vals > 1))

      # reduce to a single row all columns that have all of the same values
      raster_site_info_df <- dplyr::select(raster_site_info_df, which(all_rows_same_vals <= 1))
      raster_site_info_df <- raster_site_info_df[1, ]

      # deal with rows that don't have the same value in each row
      for (iDupSiteInfo in 1:dim(raster_site_info_df_diff_row_vals)[2]) {
        
        curr_dup_col_df <- dplyr::select(raster_site_info_df_diff_row_vals, iDupSiteInfo)
        curr_dup_col_df <- distinct(curr_dup_col_df) # remove duplicated rows (keep nested df)
        curr_dup_col_df <- as.data.frame(t(as.matrix(curr_dup_col_df))) # a bit of a hack

        raster_site_info_df <- cbind(raster_site_info_df, curr_dup_col_df)
      }
    }


    # create the appropriate site_info.X prefix variables names
    raster_site_info_names <- convert_dot_back_to_underscore(names(raster_site_info_df))
    names(raster_site_info_df) <- raster_site_info_names
    names(raster_site_info_df) <- paste0("site_info.", names(raster_site_info_df))


    # third, create the raster_data df
    raster_data <- data.frame(raster$raster.data)


    # if start or end inds are not specified used the full data set
    # also create save names if start and end inds are specified
    if (is.null(start_ind)) {
      start_ind <- 1
      start_ind_save_dir_name <- ""
    } else {
      start_ind_save_dir_name <- paste0("_start_", start_ind)
    }


    if (is.null(end_ind)) {
      end_ind <- dim(raster_data)[2]
      end_ind_save_dir_name <- ""
    } else {
      end_ind_save_dir_name <- paste0("_end_", end_ind)
    }


    raster_data <- raster_data[, start_ind:end_ind]


    # Add column names to the raster data in the form of: time.1_2, time.2_3 etc.
    data_start_times <- seq(from = 1, 
                            by = sampling_interval_width,
                            length.out = dim(raster_data)[2])
                              

    # if there is an alignment time, subtract the start_ind offset from the
    # alignment and subtract alignment from the raster times
    if ( (sum(names(raster_site_info) == "alignment.event.time")) || (is.numeric(zero_time_bin)) ) {
      
      if (is.numeric(zero_time_bin)) {
        
        data_start_times <- (data_start_times - sampling_interval_width * (rep.int(zero_time_bin - (start_ind - 1), length(data_start_times)))  )
        
      } else {
        
        data_start_times <- (data_start_times - sampling_interval_width * (rep.int(raster_site_info$alignment.event.time - (start_ind - 1), length(data_start_times))))
      }

      
      # remove the alignment time from the site info since it is incorporated into the time bin names
      raster_site_info_df$site_info.alignment_event_time <- NULL
      

      # update the names if start_ind or end_ind were given as arguments
      if (!(start_ind_save_dir_name == "")) {
        start_ind_save_dir_name <- paste0("_start_", sampling_interval_width * (start_ind - raster_site_info$alignment_event_time))
      }

      if (!(end_ind_save_dir_name == "")) {
        end_ind_save_dir_name <- paste0("_end_", sampling_interval_width * (end_ind - raster_site_info$alignment_event_time))
      }

    }

    
    data_end_times <- data_start_times + sampling_interval_width
    
    names(raster_data) <- paste0("time.", data_start_times, "_", data_end_times)

    

    # forth, add the labels to raster_data
    # Get the labels for what happened on each trial and add them to the raster_data data frame
    raster_labels <- raster$raster.labels


    # loop over label names and parse them
    all_labels <- convert_dot_back_to_underscore(row.names(raster_labels))

    for (iLabel in seq_along(all_labels)) {

      # get the name for the current raster_labels
      curr_var_name <- all_labels[iLabel]

      # add the prefix labels. to the curr label name...
      curr_var_name <- paste0("labels.", curr_var_name)

      # levels are contained in an extra list - remove this extra list to get the vector of names
      curr_levels <- raster_labels[iLabel, , ][[1]]
      curr_levels <- sapply(curr_levels, function(x) x[[1]])

      # convert to a factor
      curr_levels <- as.factor(curr_levels)

      # put into a data frame with the appropriate column name
      curr_var_column <- eval(parse(text = paste0("curr_var_column <- data.frame(", curr_var_name, " = curr_levels)")))

      # add to the raster_data
      raster_data <- cbind(curr_var_column, raster_data)

    }

    # add the site_info to the raster_data with the same values in each row (trial)
    raster_data <- cbind(raster_site_info_df[rep(1, nrow(raster_data)), ], raster_data)
    rownames(raster_data) <- NULL # remove any row names if they exist

    
    # if specified, add a variable trial_number (useful for simultaneously recorded data)
    if (add_sequential_trial_numbers) {
      raster_data$trial_number <- 1:nrow(raster_data)
      raster_data <- dplyr::select(raster_data, .data$trial_number, everything())
    }

    # finally, save the raster data in the file
    if (is.null(r_raster_dir_name)) {

      # if the directory name ends with "_mat", remove "_mat"
      non_desired_pattern <- ".*_mat$"
      if (grepl(non_desired_pattern, matlab_raster_dir_name) == TRUE) {
        r_raster_dir_name <- substr(matlab_raster_dir_name, 1, nchar(matlab_raster_dir_name) - 4)
      }


      # append start and end index if applicable and append "_rda/"
      r_raster_dir_name <- file.path(paste0(
        r_raster_dir_name, start_ind_save_dir_name,
        end_ind_save_dir_name, "_rda"), "")

    }

    
    # change the class to be raster_data, data.frame
    attr(raster_data, "class") <- c("raster_data", "data.frame")
    
    
    if (dir.exists(r_raster_dir_name) == FALSE) {
      dir.create(r_raster_dir_name)
    }

    save(raster_data, file = paste0(r_raster_dir_name, curr_r_file_name), compress = TRUE)
    
  }

  r_raster_dir_name
  
}




# a helper function to convert . to _
convert_dot_back_to_underscore <- function(oldnames) {
  newnames <- gsub(oldnames, pattern = "\\.", replacement = "_")
  return(newnames)
}






#' A plot function for data in raster format
#'
#' This function will plot data that is in raster format. If the data is
#' a spike train consisting of only 0's and 1's then it will create a plot
#' of spikes as black tick marks on a white background. If the raster data
#' contains continuous data, then the plot will be color coded. 
#'
#' @param x Either data that is in raster format, or a string containing the 
#' name of a file that has data in raster format. 
#'
#' @param ... This is needed to conform to the plot generic interface.
#' 
#' @param facet_label If this is set to a string that is the name of one of the
#' labels in the raster data, then the raster plots will be faceted by 
#' this label. 
#'
#'
#' @export
plot.raster_data <- function(x, ..., facet_label = NULL) {
  
  
  # if a string of a file name is given, load the raster data
  if (is.character(x)) {
    raster_data_object_name <- load(x)
    eval(parse(text = paste0("raster_data <- ", raster_data_object_name)))
  } else {
    raster_data <- x
  }
  
  
  # check that indeed the data is in valid raster format
  test_valid_raster_format(raster_data)
  
  activity_data_only_df <- raster_data %>%
    dplyr::select(-starts_with("site_info"))
  
  # if there is not column called trial_number add it to the data
  if (dim(dplyr::select(activity_data_only_df, starts_with("trial_number")))[2] == 0) {
    activity_data_only_df$trial_number <- 1:dim(raster_data)[1]
  }
  

  if (!(is.null(facet_label))) {
    
    activity_data_only_df <- activity_data_only_df %>%
      dplyr::select(starts_with(paste0("labels.", facet_label)), 
                    starts_with("time"), starts_with("trial_number")) %>%
      dplyr::rename(label = paste0("labels.", facet_label))
    
    
    # if faceting backed on a label, let's create new trial numbers 
    #  to be 1 to number trials for each label
    
    # first arrange the data so that it is in order of the original trial number
    activity_data_only_df <- activity_data_only_df %>%
      dplyr::arrange(.data$trial_number)
    
    # overwrite the trial number with trial numbers for each label
    activity_data_only_df <- activity_data_only_df %>%
      group_by(.data$label) %>%
      mutate(trial_number = 1:n())
    
    
  } else {
    activity_data_only_df <- activity_data_only_df %>%
      dplyr::select(-starts_with("label"))
  }
  
  

  # convert to long format for plotting and time as a numeric value
  activity_data_only_df <- activity_data_only_df %>%
    tidyr::pivot_longer(starts_with("time"), names_to = "time", values_to = "activity") %>%   
    dplyr::mutate(time = floor(get_center_bin_time(.data$time)))  

  
  # if the data is a spike train of 0's and 1's
  if ((length(unique(activity_data_only_df$activity)) == 2) &&
    (sum(unique(activity_data_only_df$activity) %in% c(1, 0)) == 2)) {
    
    if (is.character(x)){
      plot_title <- paste("Spiking pattern from neuron: ", x)
    } else {
      plot_title <- "Spiking activity"
    }
    
    
    g <- ggplot(activity_data_only_df, aes(x = .data$time, y = .data$trial_number)) +
      geom_raster(aes(fill=factor(.data$activity))) +
      scale_fill_manual(values=c("0"="white", "1"="black")) +
      guides(fill = "none") + 
      labs(x="Time", y="Trial") +
      theme_classic() + 
      ggtitle(plot_title)
    
    
  }  else {
    
    
    # if the data is real valued
    
    if (is.character(x)){
      plot_title <- paste("Activity from site: ", x)
    } else {
      plot_title <- ""
    }
    
    g <- ggplot(activity_data_only_df, aes(x = .data$time, y = .data$trial_number)) +
      geom_raster(aes(fill=.data$activity)) +
      #scale_fill_continuous(trans = 'log') + 
      labs(x="Time", y="Trial") +
      theme_bw() + 
      ggtitle(plot_title) + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    
  } 
  
  
  
  
  if (!(is.null(facet_label))) {
    g <- g + facet_wrap(~.data$label) + 
      theme_bw() + 
      theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  }
  
    
  g


}




