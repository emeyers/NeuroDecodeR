
# NeuroDecodeR (development version)






BUG FIXES AND MINOR IMPROVEMENTS

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



# NeuroDecodeR 0.1.0

## Initial release!

* This is the initial release of the NeuroDecodeR package. When new versions of
the package are created, changes will be noted in this file.


