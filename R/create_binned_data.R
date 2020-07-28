#' Convert data from raster format to binned format
#'
#' This function takes the name of a directory that contains files in raster
#' format and averages the data within a specified bin width at specified
#' sampling interval increments to create data in binned format used for
#' decoding.
#'
#'
#' @param raster_dir_name A string that contains the path to a directory that
#'   has files in raster format. These files will be combined into binned format
#'   data.
#'
#' @param save_prefix_name A string with a prefix that will be used name of file
#'   that contains the saved binned format data.
#'
#' @param bin_width A number that has the bin width that data should be averaged over.
#'
#' @param sampling_interval A number that has the specifies the sampling
#'   interval between successive binned data points.
#'
#' @param start_time A number that specifies at which time should the binning
#'   start. By default it starts at the first time in the raster data.
#'
#' @param end_time A number that specifies at which time should the binning
#'   end. By default is to end at the last time in the raster data.
#'
#' @param files_contain A string that specifies that only raster files that
#'   contain this string should be included in the binned format data.
#'
#' @examples
#' # create binned data with 150 ms bin sizes sampled at 10 ms intervals
#' raster_dir_name <- file.path(
#'   "..", "data-raw", "raster",
#'   "Zhang_Desimone_7objects_raster_data_rda", ""
#' )
#' \dontrun{
#' binned_file_name <- create_binned_data(raster_dir_name, "ZD", 150, 50)
#' }
#'
#' @export
create_binned_data <- function(raster_dir_name,
                               save_prefix_name,
                               bin_width,
                               sampling_interval,
                               start_time = NULL,
                               end_time = NULL,
                               files_contain = "") {


  # if the directory name does not end with a slash, add a slash 
  raster_dir_name <- trimws(file.path(dirname(raster_dir_name), basename(raster_dir_name), " "))

  file_names <- list.files(raster_dir_name, pattern = files_contain)


  binned_data <- NULL


  # loop through all raster data files and bin them
  for (iSite in seq_along(file_names)) {

    # print message to show progress the number of sites that have been binned    
    if (iSite == 1) {
      message(sprintf("binning site %-5s", iSite))
    } else {
      message(paste0(rep("\b", 19), collapse = ""), sprintf("binning site %-5s", iSite))
    }

    binned_data_object_name <- load(paste0(raster_dir_name, file_names[iSite]))

    if (binned_data_object_name != "raster_data") {
      
      stop('Data stored in raster files must contain an object called "raster_data"')

      # added this line to get rid of R CMD check note: no visible binding for global variable 'raster_data'
      raster_data <- NULL
    }


    one_binned_site <- bin_saved_data_one_site(raster_data, bin_width, sampling_interval, start_time, end_time)

    # append siteID to raster data, which is then appended to binned data
    one_binned_site$siteID <- rep(iSite, dim(one_binned_site)[1])
    binned_data <- rbind(binned_data, one_binned_site)
    
  }



  # make the siteID be in the first column of binned dataa
  binned_data <- binned_data %>% select(.data$siteID, everything())

  # save the results to a .Rda file
  saved_binned_data_file_name <- paste0(
    save_prefix_name, "_", bin_width, "bins_", sampling_interval, "sampled")
  start_time_name <- ""
  end_time_name <- ""

  if (!is.null(start_time)) {
    start_time_name <- paste0("_start", start_time)
  }

  if (!is.null(end_time)) {
    end_time_name <- paste0("_end", end_time)
  }

  saved_binned_data_file_name <- paste0(saved_binned_data_file_name, start_time_name, end_time_name, ".Rda")
  save("binned_data", file = saved_binned_data_file_name, compress = TRUE)

  saved_binned_data_file_name
} # end function






# A helper function for create_binned_data() to bin the data from one site
bin_saved_data_one_site <- function(raster_data,
                                    bin_width,
                                    sampling_interval,
                                    start_time = NULL,
                                    end_time = NULL) {

  labels_df <- dplyr::select(raster_data, -starts_with("time"))
  spike_df <- dplyr::select(raster_data, starts_with("time"))

  # start_df_ind is the index of the time column in spike_df
  # start_df_ind and start_time are the same if the time starts at 1

  if (is.null(start_time)) {
    start_time <- as.numeric(gsub("time.", "", colnames(spike_df)[1]))
  }
  start_df_ind <- match(paste0("time.", start_time), colnames(spike_df))


  if (is.null(end_time)) {
    temp_length <- dim(spike_df)[2]
    end_time <- as.numeric(gsub("time.", "", colnames(spike_df)[temp_length]))
  }
  end_df_ind <- match(paste0("time.", end_time), colnames(spike_df))


  all_start_df_inds <- seq(start_df_ind, end_df_ind - (bin_width - 1), by = sampling_interval)
  all_end_df_inds <- all_start_df_inds + (bin_width - 1)
  dfCurr_site_binned_data <- as.data.frame(matrix(nrow = dim(raster_data)[1], ncol = length(all_start_df_inds)))


  for (iBin in seq_along(all_start_df_inds)) {

    if (all_start_df_inds[iBin] == all_end_df_inds[iBin]) {
      
      # if binning at the same resolution as the original file, return original data
      # add start_df_ind to offset the prestimlus time
      dfCurr_site_binned_data[, iBin] <- spike_df[, all_start_df_inds[iBin]]
      
    } else {

      # otherwise, actually bin the data
      dfCurr_site_binned_data[, iBin] <- rowMeans(spike_df[, all_start_df_inds[iBin]:all_end_df_inds[iBin]])

    }
  }

  names(dfCurr_site_binned_data) <- paste0("time.", all_start_df_inds + (start_time - start_df_ind), "_", all_end_df_inds + (end_time - end_df_ind))
  dfCurr_site_binned_data <- cbind(labels_df, dfCurr_site_binned_data)

  dfCurr_site_binned_data
}
