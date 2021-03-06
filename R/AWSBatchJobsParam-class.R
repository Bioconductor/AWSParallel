
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
#' @importFrom BiocParallel BatchJobsParam
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
#' @param verbose logical, gives a verbose output of SSH connection
#'     attempt, default is FALSE.
#' @param ... Additional arguments, used to initialize BatchJobsParam.
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
##
## aws, starclusterConfigPaths exist: AWSBatchJobsParam() constructs a
## valid object
##
## starclusterCredentialsPaths does not exist: create from arguments

.starcluster_option <-
    function(config, cluster_id, option_value, default_value)
{
    if (!is.null(default_value))
        default_value
    else
        config[[cluster_id]][[option_value]]
}

#' @export
AWSBatchJobsParam <-
    function(workers = NULL,
             starclusterConfigPath = .STARCLUSTER_CONFIG_PATH,
             startclusterClusterId = "smallcluster",
             ## for bpsetup() only
             awsInstanceType = NA_character_,
             awsSubnet = NA_character_,
             awsAmiId = NA_character_,
             awsSshKeyPair = NA_character_,
             awsCredentialsPath = "~/.aws/credentials",
             awsProfile = "default",
             ## for BatchJobsParam()
             ...
             )
{
    ## Zero Check: Cannot support Windows.
    if (.Platform$OS.type == "windows") {
        stop("'AWSBatchJobsParam' not supported on Windows")
    }

    ## FIXME: On master node, when library(AWSParallel) is called, it does not need valid aws credntials to launch jobs.
    ## Zero Check: Cannot work without AWS credentials
    if (!file.exists(starclusterConfigPath)) {
        credentialCheck <- file.exists(awsCredentialsPath) &&
            any(grepl(awsProfile, readLines(awsCredentialsPath)))
        if (!credentialCheck) {
            stop("'AWSBatchJobsParam()' requires either 'startclusterConfig*' _or_ 'aws*' arguments",
                 call.=FALSE)
        }
    }

    ## First check: If starcluster config exists use that
    if (file.exists(starclusterConfigPath))
    {
        ## read config
        config <- read.ini(starclusterConfigPath)
        clusterId <- paste("cluster", startclusterClusterId)
        ## extract awsInstanceType, awsSubnet, awsAmiId, awsSshKeyPair
        awsInstanceType <- config[[clusterId]][["NODE_INSTANCE_TYPE"]]
        awsSubnet <- config[[clusterId]][["SUBNET_IDS"]]
        awsAmiId <- config[[clusterId]][["NODE_IMAGE_ID"]]
        awsSshKeyPair <- config[[clusterId]][["KEYNAME"]]
        workers <- .starcluster_option(
            config, clusterId, "CLUSTER_SIZE", workers
        )
        cidr_ip <- config[["permission http"]][["CIDR_IP"]]
    }

    ## Second Check:
    ## If any of the AWS requirements are missing, if they are
    ## initialize with empty values
    setup <- !missing(awsInstanceType) || !missing(awsSubnet) ||
        !missing(awsAmiId) || !missing(awsSshKeyPair)

    ## If workers are null give an integer value of 0
    if (!setup){
        if (is.null(workers)) {
            workers <- 0L
        }
        awsInstanceType = awsSubnet = awsAmiId = awsSshKeyPair = ""
    }

    ## ELSE, if arguments are given, use arguments
    ## Initiate .AWSBatchJobsParam class
    x <- .AWSBatchJobsParam(
        BatchJobsParam(...),
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
    return(x)
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
}

#' Suspend an AWS EC2 cluster started using bpsetup
#'
#' bpsuspend is required to 'stop' an AWS Cluster, if the user
#' has an intention of re-using it at a later time. It does NOT
#' terminate the cluster. The clustername should match the argument
#' used in bpstart.
#'
#' NOTE: bpsuspend stops the cluster, but it loses connection to resume it.
#' Please refrain from using bpsuspend.
#'
#' @param x AWSBatchJobsParam object
#' @param clustername character value given to the cluster.
#' @export
bpsuspend <-
    function(x, clustername="awsparallel")
{
    warning("If you use 'bpsuspend()', you cannot restart the cluster using 'bpresume()', please check ?bpsuspend")
    args <- c("stop", "--confirm", clustername)
    res <- system2("starcluster", args=args)
    ## Throw error if unsuccessful
    if (res != 0) {
        stop("Error suspending cluster. Please check your AWS",
             "account for these instances.")
    }
}

## FIXME: If awsparallel cluster already exists, when bpsetup is called,
## trigger bpresume
## FIXME: This is not working because of a starcluster bug, 
## "ERROR: No master node found!"
bpresume <-
    function(x, clustername="awsparallel")
    {
        stop("bpresume, does NOT work at this time")
        args <- c("restart", clustername)
        res <- system2("starcluster", args=args)
        ## Throw error if unsuccessful
        if (res != 0) {
            stop("Error resuming cluster. Please check your AWS",
                 "account to see if they have been terminated instead.")
        }
    }

## Function to resume cluster
## bpresume2 <- 
##     function(x, clustername="awsparallel")
##     {
##         ### use credentials
##         aws.ec2::use_credentials(awsCredentialsPath(aws), profile=awsProfile(awS))
##         ### Discover instances
##         instances <- aws.ec2::describe_instances()
##         ### Find instances with "name" containing clustername
##         awsList <- ## List of nodes with paste0(clustername, "-master")
##         ### start instances
##         run_instances(awsList)
##     }

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
