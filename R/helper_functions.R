#' Get name of bioconductor release version AMI
#'
#' @return Bioconductor release version
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom yaml yaml.load
#' @importFrom httr stop_for_status
#' @export
getAwsAmiId <-
    function()
    {
        res <- GET("https://www.bioconductor.org/config.yaml")
        stop_for_status(res)
        content <- content(res, type="text", encoding="UTF-8")
        txt <- yaml.load(content)
        release_version <- sub(".", "_", txt$release_version, fixed=TRUE)
        txt$ami_ids[[paste0("bioc",release_version)]]
    }


#' Describe the VPC that needs to be used
#'
#' @param cidr character, CIDR block for the VPC
#' @return vpc information
#' @importFrom aws.ec2 create_vpc
#' @importFrom aws.ec2 describe_vpcs
.awsDetectVpc <-
    function(cidr = "10.0.0.0/16")
    {
        ## TODO: Fix this
        ## Needs IF Statment
        vpcs <- describe_vpcs()

        if (length(vpcs) >= 1) {
            vpc <- vpcs[[1]]
        } else {
            stop("Please create a VPC on your AWS account")
        }
        vpc
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



#' Describe the Subnet that needs to be used
#'
#' @param vpc character subnet is created within the given VPC-ID
#' @return subnet information
#' @importFrom aws.ec2 create_subnet
#' @importFrom aws.ec2 describe_subnets
.awsDetectOrCreateSubnet <-
    function(vpc)
    {
        awsSubnet <- .awsDetectSubnetOnMaster()
        if (is.na(awsSubnet)) {
            ## If no subnet is available in that VPC,
            ## create one
            awsSubnet <- create_subnet(vpc, cidr=vpc$cidrBlock)
            awsSubnet <- awsSubnet$subnet$subnetId
        }
        awsSubnet
    }

#' Detect the security group which needs to be used
#'
#' @param vpc character Security Group is created within given VPC-ID
#' @return security group information
#' @importFrom aws.ec2 create_sgroup
#' @importFrom aws.ec2 authorize_ingress
#' @importFrom aws.ec2 describe_sgroups
.awsDetectSecurityGroup <-
    function(vpc)
    {
        ## TODO: add error checking to see if sg exists
        sgroups <- describe_sgroups()
        group_names <- vapply(sgroups, `[[`, character(1), "groupName")
        idx <- grep("AWSParallel_sgroup", group_names)
        if (length(idx) !=0 ) {
            sg <- sgroups[[idx]]
        } else {
            ## create sgroup
            sg <- create_sgroup(
                name="AWSParallel_sgroup",
                description="Security group for AWSParallel",
                vpc = vpc
            )
            ## Add TCP port range between 11000 to 11999
            authorize_ingress(
                sg, port=c(11000,11999), protocol="tcp", cidr=vpc$cidrBlock
            )
            ## Add SSH 22 port
            authorize_ingress(
                sg, port=22, protocol="tcp", cidr="0.0.0.0/0"
            )
        }
        sg
    }

#' Get AWS security requirements
#'
#' Security requirements to launch the EC2 instances into a VPC,
#' subnet, and security group
#' @return list, containing VPC, subnet, security group information
#' @export
getAwsRequirements <-
    function()
    {
        ## If user passes in CIDR block, does it get passed in?
        vpc <- .awsDetectVpc()
        subnet <- .awsDetectOrCreateSubnet(vpc)
        sg <- .awsDetectSecurityGroup(vpc)
        ## Return a named list of vpc, subnet and security group
        list(vpc=vpc, subnet=subnet, sgroup=sg)
    }


#' Function to detect if code is being run on EC2 master node
.awsDetectMaster <-
    function()
    {
        ## Get list of all instances on AWS account
        instances <- describe_instances()
        ## Get hostname of local machine code is being run on
        hostname <- system2("hostname", stdout=TRUE)
        hostname <- gsub("-",".", sub("ip-","", hostname))
        bool <- FALSE
        for (i in seq_along(instances)) {
            instancesSet = instances[[i]][["instancesSet"]]
            for (j in seq_along(instancesSet)) {
                privateIpAddress <- instancesSet[[j]][["privateIpAddress"]]
                if (privateIpAddress == hostname) {
                    bool <- TRUE
                }
            }
        }
        bool
    }


#' @importFrom aws.ec2 run_instances
.awsLaunchMaster <-
    function(x)
    {
        reqs <- getAwsRequirements()
        master_instance <- run_instances(image=awsAmiId(x),
                                         type=awsInstanceType(x),
                                         min=awsWorkers(x),
                                         subnet=reqs$subnet,
                                         sgroup=reqs$sgroup$groupId
        )
        master_instance
    }


#' Configure starcluster with the
#' @importFrom ini read.ini
#' @importFrom ini write.ini
.config_starcluster <-
    function(workers,
             awsCredentialsPath="~/.aws/credentials",
             awsInstanceType,
             awsSubnet=NA_character_,
             awsAmiId,
             awsSshKeyPair,
             awsProfile="default",
             user="ubuntu",
             cidr_ip=NA_character_
             )
{
    starcluster_config <- system.file("extdata",
                                      "config.ini",
                                      package = "AWSParallel")
    ## Read starcluster config
    config <- read.ini(starcluster_config)
    
    ## Fill starcluster config, Process AWS credentials
    aws_credentials <- read.ini(awsCredentialsPath)
    config[["aws info"]][["AWS_ACCESS_KEY_ID"]] <-
        aws_credentials[[awsProfile]][["aws_access_key_id"]]
    config[["aws info"]][["AWS_SECRET_ACCESS_KEY"]] <-
        aws_credentials[[awsProfile]][["aws_secret_access_key"]]
    
    ## Process AWS instance configuration
    config[["cluster smallcluster"]][["SUBNET_IDS"]] <- awsSubnet
    config[["cluster smallcluster"]][["CLUSTER_SIZE"]] <- workers
    config[["cluster smallcluster"]][["CLUSTER_USER"]] <- user
    config[["cluster smallcluster"]][["KEYNAME"]] <- awsSshKeyPair
    config[["cluster smallcluster"]][["NODE_INSTANCE_TYPE"]] <- awsInstanceType
    config[["cluster smallcluster"]][["NODE_IMAGE_ID"]] <- awsAmiId
    
    ## Write CIDR block
    config[["permission http"]][["CIDR_IP"]] <- cidr_ip
    
    ## Write config file in the correct path.
    write.ini(config, "~/.starcluster/config")
}

## FIXME: AMI should be listed on bioconductor.org/config.yaml
#' This function returns the AMI ID listed on bioconductor
#' https://www.bioconductor.org/config.yaml
getStarclusterAmiId <-
    function()
{
    "ami-0454187e"
}


.transferStarclusterConfig <-
    function()
{
    cat("Transfer config file")
}


#' Print command on exit from R session on host machine
.Last <-
    function()
{
    message("Please use this command to ssh to your master node",
            " on your AWS Cluster",
            "",
            " 'starcluster sshmaster -u ubuntu awsparallel' ")
}

