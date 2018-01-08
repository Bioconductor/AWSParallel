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


#' Detect the subnet on the AWS master instance of the cluster
#' 
#' @importFrom aws.ec2 describe_instances
.awsDetectSubnetOnMaster <-
    function()
{
    ## Get list of all instances on AWS account
    instances <- describe_instances()
    ## Get hostname of local machine code is being run on
    hostname <- system2("hostname", stdout=TRUE)
    hostname <- gsub("-",".", sub("ip-","", hostname))
    subnet <- NA_character_
    for (i in seq_along(instances)) {
        instancesSet = instances[[i]][["instancesSet"]]
        for (j in seq_along(instancesSet)) {
            privateIpAddress <- instancesSet[[j]][["privateIpAddress"]]
            if (privateIpAddress == hostname)
                subnet <- instancesSet[[j]][["subnetId"]]
        }
    }
    subnet
}


getStarclusterAmiId <-
    function()
{

}

