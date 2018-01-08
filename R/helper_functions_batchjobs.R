#' Configure starcluster with the
#' @importFrom ini read.ini
.config_starcluster <-
    function(workers,
             awsCredentialsPath,
             awsInstanceType,
             awsSubnet <- NA_character_,
             awsAmiId,
             awsSshKeyPair,
             awsProfile,
             user
             )
{
    starcluster_config <- "~/.starcluster/config"
    config <- read.ini(starcluster_config)

    ## Process AWS credentials
    aws_credentials <- read.ini(awsCredentialsPath)
    config[["aws info"]][["AWS_ACCESS_KEY_ID"]] <-
        aws_credentials[[awsProfile]][["aws_access_key_id"]]
    config[["aws info"]][["AWS_SECRET_ACCESS_KEY"]] <-
        aws_credentials[[awsProfile]][["aws_secret_access_key"]]
    ## Process AWS instance configuration   
    config[["cluster awsparallel"]][["SUBNET_ID"]] <- subnet
    config[["cluster awsparallel"]][["CLUSTER_SIZE"]] <- workers
    config[["cluster awsparallel"]][["CLUSTER_USER"]] <- user
    config[["cluster awsparallel"]][["KEYNAME"]] <- awsSshKeyPair
    config[["cluster awsparallel"]][["NODE_INSTANCE_TYPE"]] <- awsInstanceType
    config[["cluster awsparallel"]][["NODE_IMAGE_ID"]] <- awsAmiId
    ## FIXME: Write ini
    write.ini(config, starcluster_config)
}

## FIXME: AMI should be listed on bioconductor.org/config.yaml
#' This function returns the AMI ID listed on bioconductor.
#' 
getStarclusterAmiId <-
    function()
{
    "ami-0454187e"
}

