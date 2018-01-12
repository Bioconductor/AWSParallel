# ###########################################
# ## Trial 1: with Security Group and Subnet
# ###########################################
library(AWSParallel)

## Set required arguments and define credentials
## This DEMO requires AWS credentials

## Include Number of workers as 2
workers = 2

credentialsPath = "~/.aws/credentials"

instanceType = "t2.micro"

subnet <- "subnet-d66a05ec"

## Bioc-devel with starcluster
image <- "ami-0454187e"

## If you don't have a key you use, just create a new one
## for AWSParallel, and use that throughout.

#awsCreateKeyPair()

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



#
# ## Set the AWS SSH key pair for your machine
# awsSshKeyPair = getOption("aws_ssh_key_pair")
#

# bpstart(aws)
#
#
# ## Return cluster which was started
# awsCluster()
#
# ## Check is instance is up
# awsClusterStatus(aws)
#
# ## start an AWSParam job
# bplapply(1:4, function(i) system("hostname", intern=TRUE), BPPARAM=aws)
#
# ## Stop aws instance
# bpstop(aws)

