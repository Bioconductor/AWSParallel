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
