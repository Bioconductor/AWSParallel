############################################
### STEP 1
### HOST MACHINE
###########################################
library(AWSParallel)

## NOTE: Make sure ~/.aws/credentials are available

## Include Number of workers as 2
workers = 2
credentialsPath = "~/.aws/credentials"
instanceType = "t2.micro"
subnet <- "subnet-d66a05ec"
## Bioc-devel with starcluster
image <- "ami-5121052b"
## If you don't have a key you use, just create a new one
## for AWSParallel, and use that throughout.
keyPair <- "mykey"

## Construct AWSBatchJobsParam class
aws <- AWSBatchJobsParam(workers = workers,
                  awsCredentialsPath = credentialsPath,
                  awsInstanceType = instanceType,
                  awsSubnet = subnet,
                  awsAmiId = image,
                  awsSshKeyPair = keyPair,
                  awsProfile="default")


## Show object aws
aws

## Setup AWS cluster (takes a few mins)
bpsetup(aws)

## Suspend AWS cluster 
bpsuspend(aws)

## Terminate or teardown AWS cluster
bpteardown(aws)

#########################################
## STEP 2
## LOG INTO MASTER NODE
#########################################

starcluster sshmaster -u ubuntu awsparallel

#########################################
## STEP 3
## USE CLUSTER on MASTER NODE
#########################################

## Start R

## Load AWSParallel
library(AWSParallel)

##param <- AWSBatchJobsParam()
##register(param)

FUN <- function(i) system("hostname", intern=TRUE)
xx <- bplapply(1:100, FUN)
table(unlist(xx))

# bpstart(aws)

## Return cluster which was started
awsListCluster()
#
# ## Check is instance is up
# awsClusterStatus(aws)
#
# ## start an AWSParam job
# bplapply(1:4, function(i) system("hostname", intern=TRUE), BPPARAM=aws)
#
# ## Stop aws instance
# bpstop(aws)

