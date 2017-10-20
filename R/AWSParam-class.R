#' Reference class .AWSParam that allows usage of AWS EC2-instances
#'
#' The .AWSParam class extends the BiocParallelParam class
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
#' @importFrom BiocParallel BiocParallelParam
.AWSParam <- setRefClass("AWSParam",
   contains = "BiocParallelParam",
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
        initialize = function(...,
             awsCredentialsPath = NA_character_,
             awsInstanceType = NA_character_,
             awsSubnet = NA,
             awsSecurityGroup = NA,
             awsAmiId = NA_character_,
             awsSshKeyPair = NA_character_
             )
        {
            callSuper(...)
            initFields(
                awsCredentialsPath = awsCredentialsPath,
                awsInstanceType = awsInstanceType,
                awsSubnet = awsSubnet,
                awsSecurityGroup = awsSecurityGroup,
                awsAmiId = awsAmiId,
                awsSshKeyPair = awsSshKeyPair
            )
        },
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
                "  awsSecurityGroup: ", awsSecurityGroup(.self),
                "\n",
                "  awsAmiId: ", awsAmiId(.self),
                "\n",
                "  awsSshKeyPair: ", awsSshKeyPair(.self),
                "\n",
                sep = "")
        }
    )
)


#' Get name of bioconductor release version AMI
#'
#' @return Bioconductor release version
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom yaml yaml.load
#' @importFrom httr stop_for_status
#' @export
getAwsAmiId <- function()
{
    res <- GET("https://www.bioconductor.org/config.yaml")
    stop_for_status(res)
    content <- content(res, type="text", encoding="UTF-8")
    txt <- yaml.load(content)
    release_version <- sub(".", "_", txt$release_version, fixed=TRUE)
    txt$ami_ids[[paste0("bioc",release_version)]]
}


#' AWSParam function to start an AWS EC2-instance cluster
#'
#' This function starts a cluster of AWS EC2-instances to allow
#' parallel computation of R objects, and works with BiocParallel,
#' to allow computation with Bioconductor objects
#'
#' @param workers Numeric, number of workers to launch in the cluster
#' @param awsCredentialsPath character, Path to AWS credentials, default value is `~/.aws/credentials`
#' @param awsInstanceType character, Type of AWS EC2-instance, eg. t2.micro
#' @param awsSubnet character, AWS EC2-instance subnet, within a certain VPC
#' @param awsSecurityGroup character, Secutiry group which assigns inbound and outbound traffic at the instance level
#' @param awsAmiId character, AMI(amazon machine image) ID for the Bioconductor-release version
#' @param awsSshKeyPair character, SSH key pair, to associate with your AWS EC2-instance
#' @return
#' @example
#' \dontrun{
#' aws <- AWSParam(workers = 1,
#'                awsInstanceType="t2.micro",
#'                awsSubnet = subnet,
#'                awsSecurityGroup = sg,
#'                awsAmiId= image,
#'                awsSshKeyPair = "~/.ssh/bioc-default.pem")
#' }
#'
#' @exportClass AWSParam
AWSParam <- function(workers = 1,
             awsCredentialsPath = NA_character_,
             awsInstanceType = NA_character_,
             awsSubnet = NA,
             awsSecurityGroup = NA,
             awsAmiId = NA_character_,
             awsSshKeyPair = NA_character_
             )
{
    ## Validate AWS Credentials Path
    if (is.na(awsCredentialsPath)) {
        if (.Platform$OS.type == "unix") {
            awsCredentialsPath = "~/.aws/credentials"
        } else {
            ## FIXME: Windows %USERPROFILE%.awscredentials
            message("TODO: Windows machine needs path for credentials")
        }
    }
    stopifnot(
        file.exists(awsCredentialsPath),
        !missing(awsInstanceType),
        !missing(awsSubnet) ,
        !missing(awsSecurityGroup),
        !missing(awsSshKeyPair)
    )
    ## If missing, default to release version of AMI
    if (missing(awsAmiId)) {
        awsAmiId <- getAwsAmiId()
    }
    ## Initiate .AWSParam class
    .AWSParam(
        workers = workers,
        awsCredentialsPath = awsCredentialsPath,
        awsInstanceType = awsInstanceType,
        awsSubnet = awsSubnet,
        awsSecurityGroup = awsSecurityGroup,
        awsAmiId = awsAmiId,
        awsSshKeyPair = awsSshKeyPair
    )
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

#' Get path to AWS credentials
#'
#' @param AWSParam object
#'
#' @export
awsCredentialsPath <-
    function(x)
{
    x$awsCredentialsPath
}

#' Get number of workers in the cluster
#'
#' @param AWSParam object
#'
#' @export
awsWorkers <-
    function(x)
{
    x$workers
}

#' Get AWS instance attributes in a list
#'
#' @param AWSParam object
#'
#' @export
awsInstance <-
    function(x)
{
    x$awsInstance
}


#' Get AWS Instance type.
#'
#' The possible instance types are listed in the document:https://aws.amazon.com/ec2/instance-types/. The Bioconductor AMI's have been built using an m4.xlarge instance type. Large computations are best supported on this type of instance.
#'
#' @param AWSParam object
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
#' @param AWSParam
#'
#' @export
awsAmiId <-
    function(x)
{
    x$awsAmiId
}

#' Get AWS Subnet within which the AWS EC2 instance was launched
#'
#' @param AWSParam
#'
#' @export
awsSubnet <-
     function(x)
 {
     x$awsSubnet
 }


#' Get the SSH public key path associted to the AWS EC2 instance.
#'
#' @param AWSParam
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
#' @param AWSParam
#'
#' @export
awsSecurityGroup <-
    function(x)
{
    x$awsSecurityGroup
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods - control
###

#' Create a local enviroment to store the cluster created. This allows for
#' only a single AWSParam object to be present at a time.
#'
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
            stopifnot(is(cluster, "AWSParam"))
            cl <<- cluster
        },
        reset = function() {
            cl <<- NULL
        }
    )
})

#' Get the AWSParam object currently launched. Only one AWSParam object can be
#' started within one session.
#'
#' @return AWSParam object
#' @export
awsCluster <-
    function()
{
    if (!.awsCluster$isup()) {
        stop("no existing cluster")
    }
    .awsCluster$get()
}


#' @importFrom aws.ec2 run_instances
#' @importFrom aws.signature use_credentials
#' @exportMethod bpstart
setMethod("bpstart", "AWSParam",
    function(x)
{
    if (.awsCluster$isup())
        stop(
            "use 'bpstop(awsCluster())' to shut down existing AWS cluster",
            call. = FALSE
        )
    use_credentials()
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
        if (bpisup(x)) {
            break
        }
        message(".", appendLF = FALSE)
        Sys.sleep(1)
    }
    message(.awsInstanceStatus(x))
    invisible(x)
})


# Check status of aws ec2 instance
#' @importFrom aws.ec2 instance_status
.awsInstanceStatus <-
    function(x)
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


#' @importFrom aws.ec2 terminate_instances
#' @exportMethod bpstop
setMethod("bpstop", "AWSParam",
    function(x)
{
    if (bpisup(x)) {
        result <- terminate_instances(x$awsInstance)
        message("stopping...", appendLF = FALSE)
        repeat {
            if (!bpisup(x))
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

#' @exportMethod bpisup
setMethod("bpisup", "AWSParam",
    function(x)
{
    .awsInstanceStatus(x) == "running"
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods - evaluation
###

#' @exportMethod bplapply
setMethod("bplapply", c("ANY","AWSParam"),
    function(X, FUN, ..., BPREDO = list(), BPPARAM = bpparam())
{
    FUN <- match.fun(FUN)
})


#' @importFrom aws.ec2 decribe_instances
.awsClusterIps <- function(x)
{
    instances <- describe_instances(awsInstance(x))
    vapply(instances[[1]][["instancesSet"]], `[[`, character(1), "ipAddress")
}


#' Make socket connection by calling SnowParam
#'
#' @param AWSParam Object of class AWSParams
#'
#' @return SnowParam object
#'
#' @importFrom BiocParallel SnowParam
#' @importFrom aws.ec2 my_ip
#' @export
awsSnowParamCall <- function(x)
{
    ips <- .awsClusterIps(x)
    param = SnowParam(
        ips,
        rshcmd = paste("ssh -i", awsSshKeyPair(x), "-v", sep=" "),
        user="ubuntu",
        rhome="/usr/local/lib/R",
        snowlib = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4",
        rscript = "/usr/local/bin/Rscript",
        outfile = "/home/ubuntu/snow.log",
        master = my_ip()
    )
    param
}
