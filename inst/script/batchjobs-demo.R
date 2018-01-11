# ###########################################
# ## Trial 1: with Security Group and Subnet
# ###########################################
library(AWSParallel)

## Set required arguments


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

aws

bpsetup(aws)

bpsuspend(aws)

bpteardown(aws)



## Test .config_starcluster
## .config_starcluster(workers=2,
##                     awsCredentialsPath = "~/.aws/credentials",
##                     awsInstanceType = awsInstanceType,
##                     awsSubnet = awsSubnet,
##                     awsAmiId = awsAmiId,
##                     awsSshKeyPair = awsSshKeyPair,
##                     awsProfile = awsProfile,
##                     user = "ubuntu",
##                     cidr_ip = "172.30.0.0/16"
##                     )


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

