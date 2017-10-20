#devtools::load_all()
devtools::install()

##
library(AWSParallel)
## RELEASE
image <-  "ami-9fe2fee4"
#sg <- "sg-748dcd07"
## (val)Security group SSH open to the world
sg <- "sg-52b65822"

## Security group open to the world TCP
## sg <- "sg-b6884c20"


subnet <- "subnet-d66a05ec"


##access_key = strsplit(readLines("~/.aws/credentials"), " = ")[[2]][[2]]
##secret_key = strsplit(readLines("~/.aws/credentials"), " = ")[[3]][[2]]


workers = 1

aws <- AWSParam(
    workers,
    awsInstanceType="t2.micro",
    awsSubnet = subnet,
    awsSecurityGroup = sg,
    awsAmiId= image,
    awsSshKeyPair = "~/.ssh/bioc-default.pem",
)

aws

## Check if instance is up,
bpisup(aws)

## Start instance
bpstart(aws)


awsCluster()

ips <- BiocParallel:::.awsClusterIps(aws)
ips
## Check is instance is up
bpisup(aws)

## Stop instance
#bpstop(aws)


cl <- snow::makeSOCKcluster(
    ips,
    rshcmd = "ssh -i ~/.ssh/bioc-default.pem -v",
    user="ubuntu",
    rhome="/usr/local/lib/R",
    snowlib = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4",
    rscript = "/usr/local/bin/Rscript",
    outfile = "/home/ubuntu/snow.log",
    master = aws.ec2::my_ip()
)


param = SnowParam(
    ips,
    rshcmd = "ssh -i ~/.ssh/bioc-default.pem -v",
    user="ubuntu",
    rhome="/usr/local/lib/R",
    snowlib = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4",
    rscript = "/usr/local/bin/Rscript",
    outfile = "/home/ubuntu/snow.log",
    master = aws.ec2::my_ip()
)



## stop the instance
# e = environment(BiocParallel:::.awsCluster$set)
# e[["cl"]]= NULL

## Make new security group called "testing aws param"

## start a snow param
bplapply(1:4, function(i) system("hostname", intern=TRUE), BPPARAM=param)
bpstop(param)


## snow

snow::parLapply(cl, 1:5, function(i) system("hostname"))
