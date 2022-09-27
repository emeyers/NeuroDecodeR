#' Read a csv, rda, rds or mat file in raster format
#'
#' Reads a csv, rda, rds or mat file that has the appropriate raster_data
#' column names (i.e., columns that start with site.info, labels. and time.),
#' and returns data in raster_data format (i.e., a data frame with the raster.data
#' class attribute).
#'
#' @param raster_file_name A string specifying the name (and path) to a csv,
#'   rda, rds or mat raster data file that has the appropriate raster data
#'   column names (i.e., columns that start with site.info, labels. and time.)
#'   
#' @return Returns a data frame of data in `raster format` (i.e., with class
#'   `raster_data`). Data that is in `raster format` as the following variables:
#' 1. `labels.XXX`  These variables contain labels of which experimental conditions were shown on a given trial.
#' 
#' 2. `time.XXX_YYY`  These variables contain the data for a given time, XXX is
#' the start time of the data in a particular bin and YYY is the end time.
#'  
#' 3. `site_info.XXX` These variables contain additional meta data about the site.
#'  
#' 4. `trial_number` This variable specifies a unique number for each row 
#'  indicating which trial a given row of data came from. 
#'  
#' For more details on `raster format` data see the vignette:
#' \code{vignette("data_formats", package = "NeuroDecodeR")}
#' 
#' 
#' @examples
#' # reading in a csv file in raster format
#' csv_raster_file_name <- file.path(
#'   system.file("extdata", package = "NeuroDecodeR"),
#'   "Zhang_Desimone_7object_raster_data_small_csv",
#'   "bp1001spk_01A_raster_data.csv"
#' )
#' 
#' # read the csv file into a raster_data data frame
#' raster_data <- read_raster_data(csv_raster_file_name)
#'
#'
#' @export
read_raster_data <- function(raster_file_name) {
  
  if (!(is.character(raster_file_name))) {
    stop("raster_file_name must be a character string specifying a file name.")
  }
  
  file_type <-  tools::file_ext(raster_file_name)
  
  if (!(file_type %in% c("csv", "rda", "rds", "mat"))) {
    stop("The raster data must be either a csv, rda, rds or mat file and have these
         corresponding file extensions")
  }
    
  if (file_type == "csv") {
    
    # read in the csv 
    raster_data <- read.csv(raster_file_name, check.names = FALSE)
    
    # set the appropriate class attribute
    attr(raster_data, "class") <- c("raster_data", "data.frame") 
    
    # should all label. be converted to factors?
    
    
  } else if (file_type == "rda") {
   

    raster_data_object_name <- load(raster_file_name)
    
    # there should be only 1 object in the raster_file_name
    if (length(raster_data_object_name) != 1) {
      stop(paste(
        "Data not in valid raster format:",
        "raster data files must contain a single object that has the raster data."))
    }
    
    eval(parse(text = paste0("raster_data <- ", raster_data_object_name)))
     
  } else if (file_type == "rds") {
    
    raster_data <- readRDS(raster_file_name)
    
  } else if (file_type == "mat") {
    
    # add message about it using read_matlab_raster_data for additional arguments...
    message("Reading MATLAB raster data using the read_raster_data() function.  
            For additional features reading MATLAB raster data please use the 
            NeuroDecodeR:::read_matlab_raster_data() function.")
    
    raster_data <- read_matlab_raster_data(raster_file_name)
    
  } else {
    
    # redundant but putting the error message here again...
    #stop("The raster data must be either a csv, rda, rds or mat file and have these
    #     corresponding file extensions")
    
  }
  
  
  # test that it is now in valid format
  test_valid_raster_format(raster_data)
  
  raster_data
  
} 







#' Reads MATLAB raster data
#'
#' Reads MATLAB data in raster format into an R raster_data format data frame. This
#' is similar to the general read_raster_data() function but it contains additional
#' arguments specific to MALTAB raster data.
#'
#' @param matlab_raster_file_name A character string specifying the file name, 
#' including the full path to a raster data in MATLAB .mat files.
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
#' @param zero_time_bin A number specifying the time bin that should be marked
#'    as time 0. The default (NULL value) is to use the first bin as time 1.
#'
#' @param add_sequential_trial_numbers A Boolean specifying one should add a
#'   variable to the data called 'trial_number' that has sequential trial. These
#'   trials numbers are needed for data that was recorded simultaneously so that
#'   trials can be aligned across different sites.
#'
#' @inherit read_raster_data return 
#' 
#' @examples
#' 
#' 
#' matlab_raster_file_name <- file.path(
#'   system.file("extdata", package = "NeuroDecodeR"),
#'   "Zhang_Desimone_7object_raster_data_small_mat",
#'   "bp1001spk_01A_raster_data.mat")
#'
#' raster_data <- read_matlab_raster_data(matlab_raster_file_name)
#'
#'
#' @keywords internal
#' 
#' @export
read_matlab_raster_data <- function(matlab_raster_file_name,
                                    sampling_interval_width = 1,
                                    zero_time_bin = NULL,
                                    add_sequential_trial_numbers = FALSE) {
  
  
  
  raster <- R.matlab::readMat(matlab_raster_file_name)
  
  
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
  
  
  # Add column names to the raster data in the form of: time.1_2, time.2_3 etc.
  data_start_times <- seq(from = 1, 
                          by = sampling_interval_width,
                          length.out = dim(raster_data)[2])
  
  
  # if there is an alignment time, subtract the start_ind offset from the
  # alignment and subtract alignment from the raster times
  if ( (sum(names(raster_site_info) == "alignment.event.time")) || (is.numeric(zero_time_bin)) ) {
    
    start_ind <- 1  # no longer an option to set this
    
    if (is.numeric(zero_time_bin)) {
      
      data_start_times <- (data_start_times - sampling_interval_width * (rep.int(zero_time_bin - (start_ind - 1), length(data_start_times)))  )
      
    } else {
      
      data_start_times <- (data_start_times - sampling_interval_width * (rep.int(raster_site_info$alignment.event.time - (start_ind - 1), length(data_start_times))))
    }
    
    
    # remove the alignment time from the site info since it is incorporated into the time bin names
    raster_site_info_df$site_info.alignment_event_time <- NULL
    
    
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
  
  
  
  # If there are site_info. columns that contain nested vectors such that:
  #  1. all the nested vectors are identical
  #  2. the number of values in the vectors are the same as the number of 
  #     rows in the raster_data, so that it is likely that each value
  #     in the nested vectors correspond to one row in the raster_data. 
  #  Then unnest the site_info variable to make it so there is one value 
  #  for each row of the raster_data.
  
  all_same <- function(x) length(unique(x)) == 1
  
  nested_inds <- which(sapply(select(raster_data, starts_with("site_info")), 
                              class) == "list")
  for (curr_ind in nested_inds) {
    
    nested_var_data <- raster_data[, curr_ind]
    
    if (all_same(nested_var_data)) {
      
      nested_var_data_vector <- as.vector(nested_var_data[[1]])
      
      if (length(nested_var_data_vector) == nrow(raster_data)) {
        raster_data[, curr_ind] <- nested_var_data_vector
      } 
      
    }
  }
  
  
  
  
  
  # change the class to be raster_data, data.frame
  attr(raster_data, "class") <- c("raster_data", "data.frame")
  
  
  raster_data
  
}





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
#'   directory where the converted raster data in R files will be saved. If
#'   this is not specified then the saved directory will have the same name as
#'   the matlab directory with _rda appended to the end of the directory name.
#'   
#' @param save_file_type A character string specifying the format that the
#'   raster data should be saved as. This must be set to a string that is either
#'   "rda", "rds", or "csv", and files will be saved to the corresponding
#'   format.
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
#' @return Returns a string with the name of the directory that the .rda raster
#'   files have been saved to.
#'
#' @examples
#' 
#' \donttest{
#' 
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
#'}
#'
#' @export
convert_matlab_raster_data <- function(matlab_raster_dir_name,
                                       r_raster_dir_name = NULL,
                                       save_file_type = "rda",
                                       sampling_interval_width = 1,
                                       zero_time_bin = NULL,
                                       files_contain = "",
                                       add_sequential_trial_numbers = FALSE) {

  
  
  if (!(save_file_type %in% c("rda", "csv", "rds"))) {
    stop('save_file_type argument must be set to either "rda", "csv", or "rds"')
  }
  

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
    curr_r_file_name <- paste0(substr(curr_matlab_file_name, 1, nchar(curr_matlab_file_name) - 3), save_file_type)

    
    # call the read_matlab_raster_data here...
    raster_data <- read_matlab_raster_data(file.path(matlab_raster_dir_name, curr_matlab_file_name),
                                        sampling_interval_width,
                                        zero_time_bin,
                                        add_sequential_trial_numbers) 
    
    #  save the raster data in the file
    if (is.null(r_raster_dir_name)) {

      # if the directory name ends with "_mat", remove "_mat"
      non_desired_pattern <- ".*_mat$"
      if (grepl(non_desired_pattern, matlab_raster_dir_name) == TRUE) {
        r_raster_dir_name <- substr(matlab_raster_dir_name, 1, nchar(matlab_raster_dir_name) - 4)
      }


      # append start and end index if applicable and append "_rda/", "_rds", or "_csv"
      r_raster_dir_name <- file.path(paste0(
        r_raster_dir_name,  save_file_type), "")

    }


    if (dir.exists(r_raster_dir_name) == FALSE) {
      dir.create(r_raster_dir_name, recursive = TRUE)
    }

    if (save_file_type == "rda") {
      save(raster_data, file = file.path(r_raster_dir_name, curr_r_file_name), compress = TRUE)
    } else if (save_file_type == "csv") {
      write.csv(raster_data, file = file.path(r_raster_dir_name, curr_r_file_name), row.names = FALSE)
    } else if (save_file_type == "rds") {
      saveRDS(raster_data, file = file.path(r_raster_dir_name, curr_r_file_name), compress = TRUE)
    } else {
      stop('save_file_type argument must be set to either "rda", "csv", or "rds"')
    }
      
    
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
#' @return Returns a ggplot object that plots the raster data.
#'
#'
#' @export 
plot.raster_data <- function(x, ..., facet_label = NULL) {
  
  
  # if a string of a file name is given, read the raster data
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
  
  
  # make it so that first trial is on the top and later trials are below
  g <- g + scale_y_reverse()
    
  g


}




