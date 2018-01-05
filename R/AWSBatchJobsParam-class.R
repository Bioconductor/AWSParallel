#' Reference class .AWSBatchJobsParam that allows usage of AWS EC2
#' -instances through starcluster
#'
#' The .AWSBatchJobs class extends the BatchJobsParam class to allow
#' usage of AWS EC2-instances for parallel computation.
#' The methods follow a style similar to that of BiocParallelParams,
#' with bpstart, bpstop, bpisup, bplapply being the important one.
#' The behaviour of these functions is described in the man pages.
#'
#' @field awsCredentialsPath Path to AWS credentials, default value is
#'     `~/.aws/credentials`
#' @field awsInstanceType Type of AWS EC2-instance, eg. t2.micro
#' @field awsSubnet AWS EC2-instance subnet, within a certain VPC
#' @field awsSecurityGroup Secutiry group which assigns inbound and
#'     outbound traffic at the instance level
#' @field awsInstance A list, created holding all the information of
#'     the AWS instance
#' @field awsAmiId AMI(amazon machine image) ID for the
#'     Bioconductor-release version
#' @field awsSshKeyPair SSH key pair, to associate with your AWS
#'     EC2-instance
#' @importFrom methods new validObject callNextMethod
#' @importClassesFrom BiocParallel SnowParam BiocParallelParam
.AWSBatchJobsParam <- setRefClass( "AWSBatchJobsParam",
    contains = "BatchJobsParam",
    fields = list(
        awsCredentialsPath = "character",
        awsInstanceType = "character",
        awsSubnet = "character",
        awsAmiId = "character",        awsSshKeyPair = "character"
    ),
    methods = list(
        show = function() {
            callSuper()
            cat("  awsCredentialsPath: ", awsCredentialsPath(.self),
                "\n  awsInstanceType: ", awsInstanceType(.self),
                "\n awsSubnet: ", awsSubnet(.self),
                "\n awsAmiId: ", awsAmiId(.self),
                "\n awsSshKeyPair: ", awsSshKeyPair(.self),
                "\n",
                sep="")
        }
    ),
    inheritPackage=TRUE
)


AWSBatchJobsParam <- function(workers = 2,
                              awsCredentialsPath = NA_character_,
                              awsInstanceType = NA_character_,
                              awsSubnet = NA,
                              awsSecurityGroup = NA,
                              awsAmiId = NA_character_,
                              awsSshKeyPair = NA_character_,
                              awsProfile = "default",
                              user="ubuntu",
                              verbose = FALSE
                            )
{
    # Check AWS profile
    stopifnot(length(awsProfile) == 1L, is.character(awsProfile))

    ## Validate AWS Credentials Path
    if (is.na(awsCredentialsPath)) {
        if (.Platform$OS.type == "unix") {
            awsCredentialsPath = "~/.aws/credentials"
            ## Use credentials
            use_credentials(profile=awsProfile, file=awsCredentialsPath)
        } else {
            ## if (.Platform$OS.type == "windows") {
            message("Please launch EC2 master instance following the vignette")
        }
    }
    stopifnot(
        file.exists(awsCredentialsPath),
        !missing(awsInstanceType),
        !missing(awsSshKeyPair),
        length(user) == 1L, is.character(user)
    )

    ## If missing, default to release version of AMI
    ## FIXME: this AMI ID needs to be for starcluster AMI
    if (missing(awsAmiId)) {
        awsAmiId <- getStarclusterAwsAmiId()
    }

    ## If both security group and subnet are missing, assign
    if (missing(awsSubnet)) {
        ## If on a master node
        reqs <- getAwsRequirements()
        ## Allocate subnet and securityGroup as need
        awsSubnet <- reqs$subnet
    }

    ## Initiate .AWSSnowParam class
    x <- .AWSBatchJobsParam(
        workers = workers,
        ## AWSBatchJobsParam fields
        awsCredentialsPath = awsCredentialsPath,
        awsInstanceType = awsInstanceType,
        awsSubnet = awsSubnet,
        awsAmiId = awsAmiId,
        awsSshKeyPair = awsSshKeyPair
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
#' @param AWSSnowParam object
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
#' @param AWSSnowParam object
#'
#' @export
awsCredentialsPath <-
    function(x)
    {
        x$awsCredentialsPath
    }

#' Get number of workers in the cluster
#'
#' @param AWSSnowParam object
#'
#' @export
awsWorkers <-
    function(x)
    {
        x$workers
    }

#' Get AWS AMI-ID of the launched instance
#'
#' @param AWSSnowParam
#'
#' @export
awsAmiId <-
    function(x)
{
    x$awsAmiId
}

#' Get AWS Subnet within which the AWS EC2 instance was launched
#'
#' @param AWSSnowParam
#'
#' @export
awsSubnet <-
     function(x)
 {
     x$awsSubnet
 }

#' Get the SSH public key path associted to the AWS EC2 instance.
#'
#' @param AWSSnowParam
#'
#' @export
awsSshKeyPair <-
    function(x)
{
    x$awsSshKeyPair
}

bpsetup <-
    function(clustername)
{
    tryCatch({
        cmd <- paste("starcluster", "start", clustername)
        system2(cmd, stdout=TRUE)
    }, warning <- function(w) {
        "Warning in bpsetup, starting aws master and workers"
    }, error <- function(e) {
        "Error starting cluster"
    }, message <- function(m) {
        "AWS Cluster is being setup, please be patient"
    })
}

## FIXME: If cluster cannot be stopped
bpsuspend <-
    function(clustername)
{
    tryCatch({
        cmd <- paste("starcluster", "stop", "--confirm", clustername)
        system2(cmd, stdout=TRUE)
    }, warning <- function(w) {
        "Warning in bpsuspend, stopping aws workers"
    }, error <- function(e) {
        "Error suspending cluster"
    }, message <- function(m) {
        "AWS Cluster is being stopped, please be patient"
    })
}

bpteardown <-
    function(clustername)
{
    tryCatch({
        cmd <- paste("starcluster", "terminate", "--confirm", clustername)
        system2(cmd, stdout=TRUE)
    }, warning <- function(w) {
        "Warning in bpteardown, terminating aws workers"
    }, error <- function(e) {
        "Error terminated cluster"
    }, message <- function(m) {
        "AWS Cluster is being terminated, please be patient"
    })
}


setMethod("bpstart", "AWSBatchJobsParam",
    function(x)
{
    if(.awsDetectOnMaster()) {
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

})
