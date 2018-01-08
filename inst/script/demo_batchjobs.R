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
AWSBatchJobsParam(workers,
                  awsCredentialsPath,
                  awsInstanceType,
                  awsSubnet,
                  awsAmiId,
                  awsSshKeyPair)
