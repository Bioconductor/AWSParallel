###########################################
## Trial 1: with Security Group and Subnet
###########################################
library(AWSParallel)

## Bioc-devel 
image <-  "ami-9fe2fee4"

## Include Number of workers as 2
workers = 2
## Set the AWS SSH key pair for your machine
awsSshKeyPair = getOption("aws_ssh_key_pair")

sg <- "sg-748dcd07"
subnet <- "subnet-d66a05ec"
## Create AWS instance
aws <- AWSSnowParam(
    workers=workers,
    awsInstanceType="t2.micro",
    awsSubnet = subnet,
    awsSecurityGroup = sg,
    awsAmiId= image,
    awsSshKeyPair = awsSshKeyPair,
    bplib="/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4/BiocParallel",
    awsCredentialsPath="/home/ubuntu/credentials"
    )

aws
## Check if instance is up,
awsInstanceStatus(aws)

## Start instance
bpstart(aws)

## Return cluster which was started
awsCluster()

## Check is instance is up
awsInstanceStatus(aws)

## start an AWSParam job
bplapply(1:4, function(i) system("hostname", intern=TRUE), BPPARAM=aws)

## Stop aws instance
bpstop(aws)

##############################################
## Trial 2: Without security group and subnet,
##          this allows AWSParallel to create
##          as needed.
#############################################

library(AWSParallel)
workers = 2
image <-  "ami-9fe2fee4"
awsSshKeyPair = getOption("aws_ssh_key_pair")

aws <- AWSSnowParam(
    workers=workers,
    awsInstanceType="t2.micro",
    awsAmiId= image,
    awsSshKeyPair = awsSshKeyPair,
    bplib="/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4/BiocParallel"
)


aws
## Check if instance is up,
awsInstanceStatus(aws)

## Start instance
bpstart(aws)

## Return cluster which was started
awsCluster()

## Check is instance is up
awsInstanceStatus(aws)

## start an AWSParam job
bplapply(1:4, function(i) system("hostname", intern=TRUE), BPPARAM=aws)

## Stop aws instance
bpstop(aws)
