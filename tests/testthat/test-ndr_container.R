

basedir_file_name <- system.file(file.path("extdata", "ZD_150bins_50sampled.Rda"), 
                                 package="NeuroDecodeR")



test_that("can create and add objects to an ndr container", {
  
  # can construct an NDR container
  the_ndr_container <- NeuroDecodeR:::ndr_container()
  expect_equal(class(the_ndr_container), "ndr_container")

  
  # can add a ds_basic datasource to the ndr container
  ds <- ds_basic(basedir_file_name, 'stimulus_ID', 6, num_label_repeats_per_cv_split = 3)
  updated_ndr_container <- NeuroDecodeR:::add_ndr_object(the_ndr_container, ds) 

  expect_equal(class(updated_ndr_container), "ndr_container")
  expect_equal(class(updated_ndr_container$ds), "ds_basic")
  
  # should get a warning if I try to add another DS to the ndr container
  expect_warning(NeuroDecodeR:::add_ndr_object(updated_ndr_container, ds))
  
  # test that I can add multiple feature preprocessors to the ndr container
  fp <- fp_zscore()
  updated_ndr_container <- NeuroDecodeR:::add_ndr_object(updated_ndr_container, fp) 
  updated_ndr_container <- NeuroDecodeR:::add_ndr_object(updated_ndr_container, fp) 
  
  expect_equal(class(updated_ndr_container), "ndr_container")
  expect_equal(class(updated_ndr_container$fp), "list")
  expect_equal(class(updated_ndr_container$fp[[1]]), "fp_zscore")
  
})
  


