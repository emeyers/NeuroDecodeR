% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cl_max_correlation.R
\name{cl_max_correlation}
\alias{cl_max_correlation}
\title{A maximum correlation coefficient classifier (CL)}
\usage{
cl_max_correlation(
  ndr_container_or_object = NULL,
  return_decision_values = TRUE
)
}
\arguments{
\item{ndr_container_or_object}{The purpose of this argument is to make the
constructor of the cl_maximum_correlation classifier work with the pipe
(|>) operator. This argument should almost never be directly set by the
user to anything other than NULL. If this is set to the default value of
NULL, then the constructor will return a cl_max_correlation object. If this
is set to an NDT container, then a cl_max_correlation object will be added
to the container and the container will be returned. If this argument is
set to another NDT object, then both that NDR object as well as a new
cl_maximum_correlation object will be added to a new container and the
container will be returned.}

\item{return_decision_values}{A Boolean specifying whether the prediction
function should return columns that have the decision values. Setting this
to FALSE will save memory so can be useful when analyzing very large high
temporal resolution data sets. However if this is set to FALSE< metrics
won't be able to compute decoding accuracy measures that are based on the
decision values; e.g., the rm_main_results object won't be able to
calculate normalized rank decision values.}
}
\value{
This constructor creates an NDR classifier object with the class
\code{cl_max_correlation}. Like all NDR classifier objects, this classifier will
be used by a cross-validator to learn the relationship between neural
activity and experimental conditions on a training set of data, and then it
will be used to make predictions on a test set of data.
}
\description{
An implementation of a maximum correlation coefficient classifier.
}
\details{
This CL object learns a mean population vector (template) for each
class from the training set (by averaging together the all training points
within each class). The classifier is tested by calculated Pearson’s
correlation coefficient between a test point and the templates learned from
the training set, and the class with the highest correlation value is
returned as the predicted label. The decision values returned by the
classifier are the correlation coefficients between all test points and all
templates.

Like all classifiers (CL) objects, this classifier has a private
get_predictions() method which learns a model based on training data and then
makes predictions on the test data.
}
\examples{
# running a basic decoding analysis using the cl_max_correlation
data_file <- system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"),
                         package = "NeuroDecodeR")
ds <- ds_basic(data_file, "stimulus_ID", 18)
fps <- list(fp_zscore())

cl <- cl_max_correlation()
cv <- cv_standard(datasource = ds, 
                  classifier = cl, 
                  feature_preprocessors = fps,
                  num_resample_runs = 2)  # better to use more resample runs (default is 50)
\donttest{
DECODING_RESULTS <- run_decoding(cv)
}

}
\seealso{
Other classifier: 
\code{\link{cl_poisson_naive_bayes}()},
\code{\link{cl_svm}()}
}
\concept{classifier}
