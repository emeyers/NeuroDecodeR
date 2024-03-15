# NeuroDecodeR (development version)

# NeuroDecodeR 0.2.0


## New features

* Added functionality to the the `rm_main_results` so it now calculates 
approximate standard deviations and standard errors for all decoding results. 
These decoding standard deviations/standard errors are likely a slight 
underestimate that one would get from new trials from the same set of neurons 
since they do not take into account that there is some dependencies between the 
test data when cross-validation is used. Also, added options to the 
`rm_main_results` `plot()` and `plot_main_results()`functions so that they can 
plot the standard deviations/standard errors as shaded regions. 


## Minor improvements and fixes

* Changed the name of the private function `get_center_bin_time()` to be 
`get_time_bin_center()` (and deprecated the `get_center_bin_time()` function).
Also added functions `get_time_bin_start()` and `get_time_bin_end()` to get the
start and end times of time bin strings (these are not used by the package but
could be useful for users of the package).

* Changed `read_matlab_raster_data()` function so that if there is a field called
trial_number in the MATLAB site_info, this this will automatically be set to be
the trial_number in the R raster_data unless it is overwritten by setting
sequential trial numbers using the `add_sequential_trial_numbers` argument.

* Fixed the `plot_main_results()` function so that if 1 ms bins are used, times
are plotting correctly rather than there being multiple points at a given time
(which creating a vertical line artifact). This was done by rounding the times
to be plotted using the `floor()` function rather than `round()` function.

* Fixed the `rm_confusion_matrix` so that if a classifier does not return
decision values, the and `create_decision_vals_confusion_matrix` is set to
`TRUE`, a warning message will be given and
`create_decision_vals_confusion_matrix` will be set to `FALSE`.

* Added a warning message to the `cv_standard` constructor. If the result_metric
argument is NULL, it will print a warning to let the user know that the default
`rm_main_rsults` and `rm_confusion_matrix` result metrics are being used.

* Updated `log_load_results_from_result_name` so that if multiple names match it
returns a named list where the names are given by the manifest `result_name`
column.

* Updated `plot_main_results` so that if the `results_to_plot` is character
vector and multiple manifest `result_names` match in the manifest file, it will
plot all the results that match rather than give an error.

* Small fix to the `plot()` function for the `rm_main_results` object so that
it works with version 3.4.0 of `ggplot2`. 

* Fixed the package so that it works with tidyselect 1.2.0


# NeuroDecodeR 0.1.0

## Initial release!

* This is the initial release of the NeuroDecodeR package. When new versions of
the package are created, changes will be noted in this file.


