.AWSParam <- setRefClass("AWSParam",
#    contains = "BiocParallelParam",
    fields = list(
        awsCredentialsPath = "character",
        awsInstanceType = "character", ## instance type i.e "t2.micro"
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
            # callSuper(...)
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
            # callSuper()
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

## Get name of bioconductor release version AMI
.getAwsAmiId <-
    function()
{
    res <- httr::GET("https://www.bioconductor.org/config.yaml")
    httr::stop_for_status(res)
    content <- httr::content(res, type="text", encoding="UTF-8")
    txt <- yaml::yaml.load(content)
    release_version <- sub(".", "_", txt$release_version, fixed=TRUE)
    txt$ami_ids[[paste0("bioc",release_version)]]
}


AWSParam <-
    function(workers = 1,
             awsCredentialsPath = NA_character_,
             awsInstanceType = NA_character_,
             awsSubnet = NA,
             awsSecurityGroup = NA,
             awsAmiId = NA_character_,
             awsSshKeyPair = NA_character_
             )
{
    if (is.na(awsCredentialsPath)) {
        if (.Platform$OS.type == "unix") {
            awsCredentialsPath = "~/.aws/credentials"
        } else {
            ## FIXME: Windows %USERPROFILE%.awscredentials
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
        awsAmiId <- .getAwsAmiId()
    }

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

#' @export
awsCredentialsPath <-
    function(x)
{
    x$awsCredentialsPath
}

#' @export
awsWorkers <-
    function(x)
{
    x$workers
}

#' @export
awsInstance <-
    function(x)
{
    x$awsInstance
}

## TODO: Documentation about which instance type works
#' @export
awsInstanceType <-
    function(x)
    {
        x$awsInstanceType
    }

#' @export
awsAmiId <-
    function(x)
{
    x$awsAmiId
}

#' @export
awsSubnet <-
     function(x)
 {
     x$awsSubnet
 }

#' @export
awsSshKeyPair <-
    function(x)
{
    x$awsSshKeyPair
    }

#' @export
awsSecurityGroup <-
    function(x)
{
    x$awsSecurityGroup
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods - control
###

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

#' @export
awsCluster <- function() {
    if (!.awsCluster$isup()) {
        stop("no existing cluster")
    }
    .awsCluster$get()
}


#' @importFrom aws.ec2 run_instances
#' @importFrom aws.signature use_credentials
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


setMethod("bpisup", "AWSParam",
    function(x)
    {
        .awsInstanceStatus(x) == "running"
    })


## TODO: What does bpworkers need to do?
## setMethod("bpworkers", "AWSParam",
##           function(x)
##     {
##          x$workers
##     })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods - evaluation
###


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


## Make socket connection by calling SnowParam
#' @importFrom BiocParallel SnowParam
#' @importFrom aws.ec2 my_ip
.awsSnowParamCall <- function(x)
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
