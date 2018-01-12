#' Reference class .AWSBatchJobsParam allows use AWS EC2 as Clusters
#'
#' The .AWSBatchJobsParam class extends the BatchJobsParam class to allow
#' usage of AWS EC2-instances for parallel computation. The methods follow a
#' style similar to that of BiocParallelParams, with bpstart, bpstop, bpisup,
#' bplapply being the important one. The behaviour of these functions is
#' described in the man pages.
#'
#' @field awsCredentialsPath Path to AWS credentials, default value is
#'     `~/.aws/credentials`
#' @field awsInstanceType Type of AWS EC2-instance, eg. t2.micro
#' @field awsSubnet AWS EC2-instance subnet, within a certain VPC
#' @field awsAmiId AMI(amazon machine image) ID for the
#'     Bioconductor-starcluster image. Correct ID is needed.
#' @field awsSshKeyPair SSH key pair, to associate with your AWS
#'     EC2-instance
#' @importFrom methods new validObject callNextMethod
#' @importClassesFrom BiocParallel BatchJobsParam BiocParallelParam
.AWSBatchJobsParam <- setRefClass(
    "AWSBatchJobsParam",
    contains = "BatchJobsParam",
    fields = list(
        awsCredentialsPath = "character",
        awsInstanceType = "character",
        awsSubnet = "character",
        awsAmiId = "character",
        awsSshKeyPair = "character",
        awsProfile = "character"
    ),
    methods = list(
        show = function() {
            callSuper()
            cat("  awsCredentialsPath: ", awsCredentialsPath(.self),
                "\n  awsInstanceType: ", awsInstanceType(.self),
                "\n  awsSubnet: ", awsSubnet(.self),
                "\n  awsAmiId: ", awsAmiId(.self),
                "\n  awsSshKeyPair: ", awsSshKeyPair(.self),
                "\n  awsProfile: ", awsProfile(.self),
                "\n",
                sep="")
        }
    ),
    inheritPackage=TRUE
)


#' AWSBatchJobsParam function to start an AWS EC2-instance cluster
#'
#' This function starts a cluster of AWS EC2-instances to allow
#' parallel computation of R objects using BatchJobs on SGE, and works
#' with BiocParallel, to allow computation with R/Bioconductor objects.
#'
#' @param workers Numeric, number of workers to launch in the cluster
#' @param awsCredentialsPath character, Path to AWS credentials,
#'     default value is `~/.aws/credentials`
#' @param awsInstanceType character, Type of AWS EC2-instance,
#'     eg. t2.micro
#' @param awsSubnet character, AWS EC2-instance subnet, within a
#'     certain VPC
#' @param awsAmiId character, AMI(amazon machine image) ID for the
#'     Bioconductor-release version
#' @param awsSshKeyPair character, SSH key pair, to associate with
#'     your AWS EC2-instance
#' @param awsProfile character, indicates what profile to use while
#'     using AWS credentials
#' @param verbose logical, gives a verbose output of SSH
#'     connection attempt, default is FALSE.
#' @return AWSSnowParam object
#' @examples
#' \dontrun{
#'         ## Minimal example
#'         aws <- AWSBatchJobsParam(
#'                    workers = 2
#'                    awsCredentialsPath = "~/.aws/credentials"
#'                    awsInstanceType = "t2.micro"
#'                    awsSubnet = "subnet-d66a05ec"
#'                    awsAmiId = "ami-0454187e"
#'                    awsSshKeyPair = "mykey"
#'                )
#' }
#' @importFrom aws.ec2 my_ip
#' @importFrom aws.signature use_credentials
#' @importFrom ini read.ini
#' @exportClass AWSBatchJobsParam
#' @export
##
## aws, starclusterConfigPaths exist: AWSBatchJobsParam() constructs a
## valid object
##
## starclusterCredentialsPaths does not exist: create from arguments
AWSBatchJobsParam <-
    function(workers = 2,
             starclusterConfigPath = "~/.starcluster/config",
             awsInstanceType = NA_character_,
             awsSubnet = NA_character_,
             awsAmiId = NA_character_,
             awsSshKeyPair = NA_character_,
             awsCredentialsPath = "~/.aws/credentials",
             awsProfile = "default"
             )
{
    # Check AWS profile
    stopifnot(
        file.exists(awsCredentialsPath),
        length(awsProfile) == 1L, is.character(awsProfile)
    )
    if (.Platform$OS.type == "windows")
        stop("'AWSBatchJobsParam' not supported on Windows")

    ## Validate AWS Credentials Path
    if (file.exists(starclusterConfigPath)) {
        ## read config

        config <- read.ini(starclusterConfigPath)
        ## extract awsInstanceType, awsSubnet, awsAmiId, awsSshKeyPair
        awsInstanceType <- config[["cluster smallcluster"]][["NODE_INSTANCE_TYPE"]]
        awsSubnet <- config[["cluster smallcluster"]][["SUBNET_IDS"]]
        awsAmiId <- config[["cluster smallcluster"]][["NODE_IMAGE_ID"]]
        awsSshKeyPair <- config[["cluster smallcluster"]][["KEYNAME"]]
        workers <- config[["cluster smallcluster"]][["CLUSTER_SIZE"]]
        cidr_ip <- config[["permission http"]][["CIDR_IP"]]
        ## FIXME:
        ## allow function arguments to override config? maybe later
    } else {
        if (is.na(awsAmiId))
            awsAmiId <- getStarclusterAmiId()
        if (is.na(awsSubnet))
            ## If on a master node
            awsSubnet <- .awsDetectSubnetOnMaster()
    }

    stopifnot(
        length(awsInstanceType) == 1L, !is.na(awsInstanceType),
        length(awsSubnet) == 1L, !is.na(awsSubnet),
        length(awsAmiId) == 1L, !is.na(awsAmiId),
        length(awsInstanceType) == 1L, !is.na(awsInstanceType),
        length(awsSshKeyPair) == 1L, !is.na(awsSshKeyPair)
    )

    ## If missing, default to release version of AMI
    ## FIXME: this AMI ID needs to be for starcluster AMI

    ## Initiate .AWSBatchJobsParam class
    x <- .AWSBatchJobsParam(
        workers = workers,
        ## AWSBatchJobsParam fields
        awsCredentialsPath = awsCredentialsPath,
        awsInstanceType = awsInstanceType,
        awsSubnet = awsSubnet,
        awsAmiId = awsAmiId,
        awsSshKeyPair = awsSshKeyPair,
        awsProfile = awsProfile
    )
    validObject(x)
    x
}


#' Get AWS Instance type.
#'
#' The possible instance types are listed in the
#' document: https://aws.amazon.com/ec2/instance-types/.  The
#' Bioconductor AMI's have been built using an m4.xlarge instance
#' type.  Large computations are best supported on this type of
#' instance.
#'
#' @param AWSBatchJobsParam object
#'
#' @return character
#' @export
awsInstanceType <-
    function(x)
{
    x$awsInstanceType
}


#' Get path to AWS credentials
#'
#' @param AWSBatchJobsParam object
#'
#' @export
awsCredentialsPath <-
    function(x)
{
    x$awsCredentialsPath
}


#' Get number of workers in the cluster
#'
#' @param AWSBatchJobsParam object
#'
#' @export
awsWorkers <-
    function(x)
{
    x$workers
}


#' Get AWS AMI-ID of the launched instance. These need to be
#' Bioconductor configured AMI's.
#'
#' @param AWSBatchJobsParam
#'
#' @export
awsAmiId <-
    function(x)
{
    x$awsAmiId
}


#' Get AWS Subnet within which the AWS EC2 instance was launched
#'
#' @param AWSBatchJobsParam
#'
#' @export
awsSubnet <-
     function(x)
{
    x$awsSubnet
}


#' Get the SSH public key path associted to the AWS EC2 instance.
#'
#' @param AWSBatchJobsParam
#'
#' @export
awsSshKeyPair <-
    function(x)
{
    x$awsSshKeyPair
}

#' Get the awsProfile being used
#'
#' @param AWSBatchJobsParam
#'
#' @export
awsProfile <-
    function(x)
{
    x$awsProfile
}

#' Setup a new AWS EC2 cluster
#'
#' The step bpsetup is required before using any of the conventional
#' BiocParallel functions like bpstart, bpstop. It is used to setup
#' or start a new or existing cluster on the user's AWS account. Once
#' a cluster is up an running, it should be safely suspended or terminated
#' using functionality like 'bpsuspend' and 'bpteardown'. NOTE: This function
#' takes a while to process, depending on the number of workers needed 
#' it may take upto 4-5 minutes.
#' 
#'
#' 
#' @param x AWSBatchJobsParam object
#' @param clustername character value given to the cluster.
#' @export
bpsetup <-
    function(x, clustername="awsparallel")
{

    .config_starcluster(workers = awsWorkers(x),
                        awsCredentialsPath = awsCredentialsPath(x),
                        awsInstanceType = awsInstanceType(x),
                        awsSubnet = awsSubnet(x),
                        awsAmiId = awsAmiId(x),
                        awsSshKeyPair = awsSshKeyPair(x),
                        awsProfile = awsProfile(x),
                        cidr_ip = "172.30.0.0/16"
                        )
    args <- c("start", clustername)
    res <- system2("starcluster", args=args)
    ## If res!=0 then fail.
    if (res != 0) {
        stop("Cluster failed to launch, please check the settings")
    }
    ## Once cluster is started transfer config file to master node
    transferToCluster(clustername, "~/.starcluster/config",
                      "~/.starcluster/config") 
    ## Register AWSBatchJobsParam in the BiocParallelRegistry
    
}

#' Suspend an AWS EC2 cluster started using bpsetup
#'
#' bpsuspend is required to 'stop' an AWS Cluster, if the user
#' has an intention of re-using it at a later time. It does NOT
#' terminate the cluster. The clustername should match the argument
#' used in bpstart.
#'
#' @param x AWSBatchJobsParam object
#' @param clustername character value given to the cluster.
#' @export
bpsuspend <-
    function(x, clustername="awsparallel")
{
    args <- c("stop", "--confirm", clustername)
    res <- system2("starcluster", args=args)
    ## Throw error if unsuccessful
    if (res != 0) {
        stop("Error suspending cluster. Please check your AWS",
             "account for these instances.")
    }
}


#' Teardown permanently (TERMINATE) the AWS cluster.
#'
#' bpteardown is used to completely remove the AWS cluster from
#' the users AWS account. The user cannot retreive any data or
#' reuse the cluster once bpteardown is started.
#'
#' We recommend using bpteardown, once the data analysis is done.
#' This will regulate AWS account costs, unless the user intends to
#' to reuse the cluster. If there is a need to reuse the cluster see,
#' '?bpsuspend'.
#'
#' @param x AWSBatchJobsParam object
#' @param clustername character value given to the cluster.
#' @export
bpteardown <-
    function(x, clustername="awsparallel")
{
    args <- c("terminate", "-f", "--confirm", clustername)
    res <- system2("starcluster", args=args)
    if (res !=0 ) {
        stop("Error terminating cluster. Please check your AWS",
             "account for these instances or run bpteardown again.")
    }
}

## TODO: Provide information for the config file to be SSH-ed into the master.
## 1. Save config file.
## 2. re-construct the AWSBatchJobsParam on the master from the config file.
## 3. Look at register.R from BiocParallel to see how to register latest
##    AWSBatchJobs param.
#'
setMethod("bpstart", "AWSBatchJobsParam",
    function(x)
{
    if(.awsDetectMaster()) {
        stop(
            "SSH to Master node of batch jobs machine using",
            "awsConnectToMaster()",
            call. = FALSE
        )
    }
})


setMethod("bpstop", "AWSBatchJobsParam",
    function(x)
{
    cat("bpstop is being called")
})
