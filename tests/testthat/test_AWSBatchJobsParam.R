test_that("AWSBatchJobsParam null constructor works", {
    aws <- AWSBatchJobsParam()
    expect_true(validObject(aws))

    work.dir <- aws$reg.pars$work.dir
    expect_identical(work.dir, getwd())
})

