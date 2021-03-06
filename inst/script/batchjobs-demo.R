############################################
### STEP 1
### HOST MACHINE
###########################################
library(AWSParallel)
devtools::load_all()
## NOTE: Make sure ~/.aws/credentials are available

## Include Number of workers as 2
workers = 2
credentialsPath = "~/.aws/credentials"
instanceType = "t2.micro"
subnet <- "subnet-d66a05ec"
## Bioc-devel with starcluster
image <- "ami-18c0f562"
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

## SUSPEND WORKS BUT STARCLUSTER HAS AN ISSUE WITH RESTART, 
## Suspend AWS cluster 
bpsuspend(aws)

## Manually have to start AWS Nodes using CloudyR and finding out the ClusterID
## bpresume(aws)

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

git pull
git checkout AWSBatchParam-class
## Start R

## Load AWSParallel
library(AWSParallel)
devtools::load_all()

aws  <- registered()[[1]]
FUN <- function(i) system("hostname", intern=TRUE)
xx <- bplapply(1:100, FUN, BPPARAM=aws)

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

