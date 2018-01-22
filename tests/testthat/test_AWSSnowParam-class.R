context("AWSSnowParam")

## Test to check if aws credtials exist 
test_that("AWSSnowParam constructor fails without awsCredentials", {
    expect_true(file.exists("~/.aws/credentials"))
})

test_that("AWSSnowParam works with minimum arguments", {
    aws <- AWSSnowParam(
        workers=2,
        awsInstanceType="t2.micro",
        awsSubnet = "subnet-d66a05ec",
        awsSecurityGroup = "sg-748dcd07",
        awsAmiId="ami-2951fa53",
        awsSshKeyPair = "mykey",
        bplib="/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4/BiocParallel",
        awsCredentialsPath= .AWS_CREDENTIALS_PATH
    )
    ## Test accessors and see if identical
    expect_identical(bpworkers(aws), 2L)
    expect_identical(awsSubnet(aws), "subnet-d66a05ec")
    expect_identical(awsSecurityGroup(aws), "sg-748dcd07")
    expect_identical(awsAmiId(aws), "ami-2951fa53")
})

## Test case to check if setting `bpworkers<-` works


## Test case to check if there is always only 1 cluster available through snow


## Test case to check the AWSSnowParam null constructor
