#' Reference class .AWSSnowParam that allows usage of AWS EC2-instances
#'
#' The .AWSSnowParam class extends the SnowParam class
#' to allow usage of AWS EC2-instances for parallel computation.
#' The methods follow a style similar to that of BiocParallelParams,
#' with bpstart, bpstop, bpisup, bplapply being the important one.
#'
#' @field awsCredentialsPath Path to AWS credentials, default value is `~/.aws/credentials`
#' @field awsInstanceType Type of AWS EC2-instance, eg. t2.micro
#' @field awsSubnet AWS EC2-instance subnet, within a certain VPC
#' @field awsSecurityGroup Secutiry group which assigns inbound and outbound traffic at the instance level
#' @field awsInstance A list, created holding all the information of the AWS instance
#' @field awsAmiId AMI(amazon machine image) ID for the Bioconductor-release version
#' @field awsSshKeyPair SSH key pair, to associate with your AWS EC2-instance
#' @importClassesFrom BiocParallel SnowParam BiocParallelParam
.AWSSnowParam <- setRefClass(
    "AWSSnowParam",
    contains = "SnowParam",
    fields = list(
        awsCredentialsPath = "character",
        awsInstanceType = "character",
        awsSubnet = "character",
        awsSecurityGroup = "character",
        awsInstance = "list",
        awsAmiId = "character",
        awsSshKeyPair = "character"
    ),
    methods = list(
        show = function() {
            callSuper()
            ## Display only half of AWS access and secret keys
            cat("  awsCredentialsPath: ",
                awsCredentialsPath(.self),
                "\n",
                "  awsInstanceType: ", awsInstanceType(.self),
                "\n",
                "  awsSubnet: ", awsSubnet(.self),
                "\n",
                "  awsSecurityGroup(s): ", paste(awsSecurityGroup(.self), sep=" ", collapse=" "),
                "\n",
                "  awsAmiId: ", awsAmiId(.self),
                "\n",
                "  awsSshKeyPair: ", awsSshKeyPair(.self),
                "\n",
                sep = "")
        }
    ),
    inheritPackage = TRUE
)


#' AWSSnowParam function to start an AWS EC2-instance cluster
#'
#' This function starts a cluster of AWS EC2-instances to allow
#' parallel computation of R objects, and works with BiocParallel,
#' to allow computation with Bioconductor objects
#'
#' @param workers Numeric, number of workers to launch in the cluster
#' @param awsCredentialsPath character, Path to AWS credentials, default value is `~/.aws/credentials`
#' @param awsInstanceType character, Type of AWS EC2-instance, eg. t2.micro
#' @param awsSubnet character, AWS EC2-instance subnet, within a certain VPC
#' @param awsSecurityGroup character, Security group which assigns inbound and outbound traffic at the instance level. The security group needs to be
#' *Inbound rules*
#' Protocol type	Port number	Source IP
#'           TCP	22 (SSH)	0.0.0.0/0
#'           TCP	11000-11999	CIDR-Block same as VPC
#' *Outbound rules*
#' Protocol type	Port number	Destination IP
#'           All	All	        0.0.0.0/0
#' @param awsAmiId character, AMI(amazon machine image) ID for the Bioconductor-release version
#' @param awsSshKeyPair character, SSH key pair, to associate with your AWS EC2-instance
#' @return AWSSnowParam object
#' @examples
#' \dontrun{
#'         ## Minimal example
#'         aws <- AWSSnowParam(workers = 1,
#'                awsInstanceType="t2.micro",
#'                awsAmiId= image,
#'                awsSshKeyPair = "~/.ssh/<my_aws_key_pair>.pub")
#' }
#' @importFrom aws.ec2 my_ip
#' @importFrom aws.signature use_credentials
#' @exportClass AWSSnowParam
#' @export
AWSSnowParam <- function(workers = 2,
             awsCredentialsPath = NA_character_,
             awsInstanceType = NA_character_,
             awsSubnet = NA,
             awsSecurityGroup = NA,
             awsAmiId = NA_character_,
             awsSshKeyPair = NA_character_,
             user="ubuntu",
             rhome="/usr/local/lib/R",
             ## TODO: change this default
             bplib="/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4/BiocParallel",
             rscript = "/usr/local/bin/Rscript",
             outfile = "/home/ubuntu/snow.log"
             )
{
    ## Validate AWS Credentials Path
    if (is.na(awsCredentialsPath)) {
        if (.Platform$OS.type == "unix") {
            awsCredentialsPath = "~/.aws/credentials"
            ## Use credentials
            use_credentials(awsCredentialsPath)
        } else {
            ## FIXME: Windows %USERPROFILE%.awscredentials
            message("TODO: Windows machine needs path for credentials")
        }
    }
    stopifnot(
        file.exists(awsCredentialsPath),
        !missing(awsInstanceType),
        !missing(awsSshKeyPair),
        length(user) == 1L, is.character(user),
        length(rhome) == 1L, is.character(rhome),
        length(bplib) == 1L, is.character(bplib),
        length(rscript) == 1L, is.character(rscript),
        length(outfile) == 1L, is.character(outfile)
    )

    ## If missing, default to release version of AMI
    if (missing(awsAmiId)) {
        awsAmiId <- getAwsAmiId()
    }

    ## If both security group and subnet are missing, assign
    if (missing(awsSubnet) || missing(awsSecurityGroup)) {
        reqs <- getAwsRequirements()
        ## Allocate subnet and securityGroup as need
        awsSubnet <- reqs$subnet
        awsSecurityGroup <- reqs$sgroup$groupId
    }

    .clusterargs <- list(
        spec = workers, type = "SOCK",
        ## TODO: Remove verbose argument -v
        rshcmd = paste("ssh -i", awsSshKeyPair, "-v", sep=" "),
        user=user,
        rhome=rhome,
        snowlib=bplib,
        rscript=rscript,
        outfile=outfile
    )

    ## Initiate .AWSSnowParam class
    x <- .AWSSnowParam(
        ## base class (SnowParam) fields
        workers = workers,
        ## TODO: There is no `-i` in OS-X
        hostname = system2("hostname", stdout=TRUE),
        .clusterargs = .clusterargs,
        ## AWSSnowParam fields
        awsCredentialsPath = awsCredentialsPath,
        awsInstanceType = awsInstanceType,
        awsSubnet = awsSubnet,
        awsSecurityGroup = awsSecurityGroup,
        awsAmiId = awsAmiId,
        awsSshKeyPair = awsSshKeyPair
    )
    validObject(x)
    x
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

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

#' Get AWS instance attributes in a list
#'
#' @param AWSSnowParam object
#'
#' @export
awsInstance <-
    function(x)
{
    x$awsInstance
}


#' Get AWS Instance type.
#'
#' The possible instance types are listed in the document:https://aws.amazon.com/ec2/instance-types/.
#' The Bioconductor AMI's have been built using an m4.xlarge instance type.
#' Large computations are best supported on this type of instance.
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

#' Get AWS Security group for the EC2 instance, which defines inbound and
#' outbound traffic.
#'
#' @param AWSSnowParam
#'
#' @export
awsSecurityGroup <-
    function(x)
{
    x$awsSecurityGroup
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods - control
###

## Create a local enviroment to store the cluster created. This allows for
## only a single AWSSnowParam object to be present at a time.
.awsCluster <- local({
    cl <- NULL
    list(
        isup = function() {
            !is.null(cl)
        },
        get = function() {
            cl
        },
        set = function(cluster) {
            stopifnot(is(cluster, "AWSSnowParam"))
            cl <<- cluster
        },
        reset = function() {
            cl <<- NULL
        }
    )
})

#' Get the AWSSnowParam object currently launched. Only one AWSSnowParam object can be
#' started within one session.
#'
#' @return AWSSnowParam object
#' @export
awsCluster <- function()
{
    if (!.awsCluster$isup()) {
        stop("no existing cluster")
    }
    .awsCluster$get()
}


#' @importFrom aws.ec2 describe_instances
.awsClusterIps <- function(x)
{
    instances <- describe_instances(awsInstance(x))
    vapply(instances[[1]][["instancesSet"]], `[[`, character(1), "privateIpAddress")
}


#' @importFrom aws.ec2 run_instances
#' @importFrom aws.signature use_credentials
#' @importFrom BiocParallel bpstart
#' @exportMethod bpstart
setMethod("bpstart", "AWSSnowParam",
    function(x)
{
    if (.awsCluster$isup())
        stop(
            "use 'bpstop(awsCluster())' to shut down existing AWS cluster",
            call. = FALSE
        )
    use_credentials(file=awsCredentialsPath(x))
    ## Set awsBiocVersion, devel vs release

    result <- run_instances(
        image=awsAmiId(x),
        type=awsInstanceType(x),
        min=awsWorkers(x),
        subnet=awsSubnet(x),
        sgroup=awsSecurityGroup(x)
    )
    ## Print instance state to screen after starting instance
    x$awsInstance <- result
    .awsCluster$set(x)
    ## Wait for instance to be up.
    message("starting...", appendLF = FALSE)
    repeat{
        if (.awsisup(x)) {
            break
        }
        message(".", appendLF = FALSE)
        Sys.sleep(1)
    }
    message(awsInstanceStatus(x))
    ## start cluster
    bpworkers(x) <- .awsClusterIps(x)
    ## Sleep for 10 seconds to make sure there is no race condition
    ## TODO: make this better
    Sys.sleep(10)
    ## Call bpstart in SnowParam
    callNextMethod(x)
})


# Check status of aws ec2 instance
#' @importFrom aws.ec2 instance_status
#' @export
awsInstanceStatus <- function(x)
{
    instance <- awsInstance(x)
    if (length(instance) == 0L) {
        "stopped"
    } else {
        status <- instance_status(instance)
        if (length(status) == 0L) {
            "starting"
        } else {
            status$item$instanceState$name[[1]]
        }
    }
}

#' Check if AWS cluster is up
.awsisup <- function(x)
{
    awsInstanceStatus(x) == "running"
}


#' @importFrom aws.ec2 terminate_instances
#' @importFrom BiocParallel bpstop
#' @exportMethod bpstop
setMethod("bpstop", "AWSSnowParam",
    function(x)
{
    if (.awsisup(x)) {
        result <- terminate_instances(x$awsInstance)
        message("stopping...", appendLF = FALSE)
        repeat {
            if (!.awsisup(x))
                break
            message(".", appendLF = FALSE)
        }
        message("terminated")
        ## TODO: Fix this
        .awsCluster$reset()
    }
    ## Return terminated instance state to screen
    x$awsInstance <- list()
    invisible(x)
})
