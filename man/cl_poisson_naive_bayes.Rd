% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cl_poisson_naive_bayes.R
\name{cl_poisson_naive_bayes}
\alias{cl_poisson_naive_bayes}
\title{A Poisson Naive Bayes classifier (CL)}
\usage{
cl_poisson_naive_bayes(
  ndr_container_or_object = NULL,
  return_decision_values = TRUE
)
}
\arguments{
\item{ndr_container_or_object}{The purpose of this argument is to make the
constructor of the cl_poisson_naive_bayes classifier work with magrittr
pipe (|>) operator. This argument should almost never be directly set by
the user to anything other than NULL. If this is set to the default value
of NULL, then the constructor will return a cl_poisson_naive_bayes object.
If this is set to an ndr container, then a cl_poisson_naive_bayes object
will be added to the container and the container will be returned. If this
argument is set to another ndr object, then both that ndr object as well as
a new cl_poisson_naive_bayes object will be added to a new container and
the container will be returned.}

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
\code{cl_poisson_naive_bayes}. Like all NDR classifier objects, this classifier
will be used by a cross-validator to learn the relationship between neural
activity and experimental conditions on a training set of data, and then it
will be used to make predictions on a test set of data.
}
\description{
An implementation of a Poisson Naive Bayes classifier.
}
\details{
This classifier object implements a Poisson Naive Bayes classifier. The
classifier works by learning the expected number of occurrences (denoted
lambda) for each feature and each class by taking the average of the training
data over all trials (separately for each feature and each class). To
evaluate whether a given test point belongs to class i, the log of the
likelihood function is calculated using the lambda values as parameters of
Poisson distributions (i.e., there is a separate Poisson distribution for
each feature, that is based on the lambda value for that feature). The
overall likelihood value is calculated by multiplying the probabilities for
each neuron together (i.e,. Naive Bayes classifiers assume that each feature
is independent), or equivalently, adding the log of the probabilities for
each feature together. The class with the highest likelihood value is chosen
as the predicted label, and the decision values are the log likelihood
values.

\strong{Note:} this classifier uses spike counts, so the binned data must be
converted to use this classifier, for example, if you are using the basic_DS
data source, then use_count_data = TRUE should be set in the constructor.
Also, preprocessors that convert the data into values that are not integers
should not be used, for example, the fp_zscore should not be used with this
classifier.

Like all classifiers, this classifier learning a model based on training data
and then makes predictions on new test data.
}
\examples{
# running a basic decoding analysis using the cl_max_correlation

data_file <- system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"),
                         package = "NeuroDecodeR")
ds <- ds_basic(data_file, "stimulus_ID", 18, use_count_data = TRUE)
fps <- list()

cl <- cl_poisson_naive_bayes()
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
\code{\link{cl_max_correlation}()},
\code{\link{cl_svm}()}
}
\concept{classifier}
