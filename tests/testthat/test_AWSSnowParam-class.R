context("AWSSnowParam")

## Test to check if aws credtials exist 
test_that("AWSSnowParam constructor fails without awsCredentials", {
    expect_true(file.exists("~/.aws/credentials"))
})


