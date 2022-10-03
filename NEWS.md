
# NeuroDecodeR (development version)






BUG FIXES AND MINOR IMPROVEMENTS

* Fixed the `rm_confusion_matrix` so that if a classifier does not return
decision values, the and `create_decision_vals_confusion_matrix` is set to
`TRUE`, a warning message will be given and
`create_decision_vals_confusion_matrix` will be set to `FALSE`.

* Added a warning message to the `cv_standard` constructor. If the result_metric
argument is NULL, it will print a warning to let the user know that the default
`rm_main_rsults` and `rm_confusion_matrix` result metrics are being used.



# NeuroDecodeR 0.1.0

## Initial release!

* This is the initial release of the NeuroDecodeR package. When new versions of
the package are created, changes will be noted in this file.


