context("AWSBatchJobsParam")

starcluster_template <- "[global]
DEFAULT_TEMPLATE = smallcluster

[aws info]
AWS_ACCESS_KEY_ID = # your ACCESS KEY
AWS_SECRET_ACCESS_KEY = # your SECRET ACCESS KEY
AWS_USER_ID = # your USER ID

[key mykey]
KEY_LOCATION = ~/.ssh/mykey.rsa

[cluster smallcluster]
KEYNAME = mykey
CLUSTER_SIZE = 2
CLUSTER_USER = ubuntu
CLUSTER_SHELL = bash
DNS_PREFIX = True
NODE_IMAGE_ID = ami-0454187e
NODE_INSTANCE_TYPE = t2.micro
permissions = http
SUBNET_IDS = subnet-d66a05ec

[permission http]
IP_PROTOCOL = tcp
FROM_PORT = 80
TO_PORT = 80
CIDR_IP = 172.30.0.0/16"

starclusterConfigPath <- tempfile()
writeLines(starcluster_template, starclusterConfigPath)

test_that("AWSBatchJobsParam constructor fails without starclusterConfig", {
    expect_error(
        AWSBatchJobsParam(
            starclusterConfigPath = starclusterConfigPath,
            awsInstanceType = "t2.micro"
        ),
        "'AWSBatchJobsParam\\(\\)' requires either.*"
    )
})

test_that("AWSBatchJobsParam null constructor works", {
    aws <- AWSBatchJobsParam()
    expect_true(validObject(aws))

    work.dir <- aws$reg.pars$work.dir
    expect_identical(work.dir, getwd())
})

test_that("AWSBatchJobsParam initializes BatchJobsParams", {
    dir <- tempfile()
    aws <- AWSBatchJobsParam(work.dir=dir)
    work.dir <- aws$reg.pars$work.dir
    expect_identical(work.dir, dir)
})

test_that("AWSBatchJobsParam overrides BatchJobsParams", {
    aws <- AWSBatchJobsParam(workers = 6)
    expect_identical(bpnworkers(aws), 6L)
})

test_that("AWSBatchJobsParam accepts starclusterConfigPath", {
    aws <- AWSBatchJobsParam(starclusterConfigPath = starclusterConfigPath)
    expect_identical(bpnworkers(aws), 2L)
    expect_identical(awsInstanceType(aws), "t2.micro")
})

## This test case works only if .starcluster/config is missing from the machine
## Expect it to fail if the tests are run with
test_that("AWSBatchJobsParam .onLoad() works", {
    .registerOnStartup <- AWSParallel:::.registerOnStartup

    expect_warning(
        .registerOnStartup(),
        "'AWSBatchJobsParam\\(\\)' registered without starcluster conf"
    )
})

