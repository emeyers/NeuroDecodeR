
## Resubmission 5b

I tried to resubmit to CRAN addressing the comments made by Beni Altmann. I just received an email that my resubmission did not pass the incoming checks because there was a note, but the only note was addressed in my initial submission, namely, I am using the doSnow package so that I can use the txtProgressBar. Additionally,
there was a message that the name Kreiman is a spelling mistake, but this is the correct spelling of an author of a paper that was requested that I cite. Thus, I believe my resubmission passes all the checks and is ready for further review. 



## Resubmission 5

Thank you for the feedback Beni Altmann! I have made the required changes mentioned which are: 

 * Added \value statements describing the outputs of all the methods in the package using the roxygen
   @return tag. 

 * Removed the reference to the unexported get_data() function in the documentation for the ds_basic 
    (i.e., removed the reference that involved :::).
    
 * Updated all references to \dontrun{} to now be \donttest{} in the documentation as requested. 
 
 * Went through all the examples/vignettes/tests and checked where all files were being written.
   Changed two vignettes so that they write to temporary directory. All files now should only be 
   written to temporal directories.
 
 * Changed all the examples, vignettes, and tests so that they only uses 2 parallel cores. 
    FYI, this is not be ideal since I feel like some examples might mislead the 
    user to choose settings that are not the best way to use the package. However, I 
    still made the changes to conform to CRANs standards, and I made comments in the 
    examples that one should choose different settings when running an actual analysis, 
    so hopefully that will be ok.

   
Thanks for your help reviewing the package!

Ethan





## Resubmission 4

Responding to feedback from Uwe Ligges:

* Fixed the DESCRIPTION file as requested to have a better title, but package name NeuroDecodeR in quotes, and added a reference to the neural decoding methodology including the doi. 

Thanks for your continued help reviewing the package!




## Resubmission 3

I noticed I was still getting errors that a couple of links were still not working. This is a resubmission prior to getting feedback on submission 2 in order to fix these broken links.


 * Removed the link to Travis CI since I am not longer using it and it was giving a 404. Changed the link to the PNAS article since it was giving a 503, to a link to a pubmed reference to the article.

Hopefully all the links are fixed now!

Regards,

Ethan




## Resubmission 2

Responding to feedback again from Uwe Ligges

 * There appeared to be 5 links in the documentation that while not broken, were being automatically forwarded to new links by the respective websites. These links have now been set to web addresses that they were being forwarded to. Hopefully this fixed there remaining issue.

Thanks again for reviewing my submission!

Regards,

Ethan



## Resubmission 1

This is a first resubmission of a new package, responding to feedback from Uwe Ligges on intial submission below. In this version I have: 

 * Fixed three broken URLs in the vignettes/pkgdown documentation. 

Thank you for bringing these broken links to my attention and for reviewing my submission!

Regards,

Ethan



## Release summary

This is the first submission of the package to CRAN


## Test environments

 * local macOS Monterey 12.5.1, R 4.2.1
 * local Windows 10.0.19043, R 4.2.1
 * local Ubuntu 18.04, R 4.2.1


## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE

Uses the superseded packages: ‘doSNOW’, ‘snow’

  *Use of the 'doSNOW' package as opposed to the 'doParallel' package is required due to the support of the printed txtProgressBar in the 'doSNOW' package.*


