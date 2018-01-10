# ###########################################
# ## Trial 1: with Security Group and Subnet
# ###########################################
# library(AWSParallel)
#
# ## Bioc-devel with starcluster
# image <-  "ami-0454187e"
#
# ## Include Number of workers as 2
# workers = 2
# ## workers = 4
#
# ## Set the AWS SSH key pair for your machine
# awsSshKeyPair = getOption("aws_ssh_key_pair")
#
# ## Define subnet
# subnet <- "subnet-d66a05ec"
#
# ## Create AWS instance
# aws <- AWSBatchJobsParam(
#     workers=workers,
#     awsInstanceType="t2.micro",
#     awsSubnet = subnet,
#     awsAmiId= image,
#     awsSshKeyPair = awsSshKeyPair,
#     awsCredentialsPath="/home/ubuntu/credentials"
# )
#
# aws
#
# ## Setup master and worker nodes with starcluster
# bpsetup(aws)
#
# ## Start instance --> or log into master via SSH
# bpstart(aws)
#
#
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
#
#

## Load library
library(AWSParallel)

## Set required arguments
workers = 2
awsCredentialsPath = "~/.aws/credentials"
awsInstanceType = "t2.micro"
awsSubnet = "subnet-d66a05ec"
awsAmiId = "ami-0454187e"
awsSshKeyPair = "mykey"


## Construct AWSBatchJobsParam class
aws <- AWSBatchJobsParam(workers,
                  awsCredentialsPath,
                  awsInstanceType,
                  awsSubnet,
                  awsAmiId,
                  awsSshKeyPair)

bpsetup(aws)

bpsuspend(aws)

bpteardown(aws)



.config_starcluster(workers=2,
                    awsCredentialsPath = "~/.aws/credentials",
                    awsInstanceType = awsInstanceType,
                    awsSubnet = awsSubnet,
                    awsAmiId = awsAmiId,
                    awsSshKeyPair = awsSshKeyPair,
                    awsProfile = awsProfile,
                    user = "ubuntu",
                    cidr_ip = "172.30.0.0/16"
                    )
