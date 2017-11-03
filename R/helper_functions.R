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


#' Describe the VPC that needs to be used
#'
#' @param cidr character, CIDR block for the VPC
#' @return vpc information
#' @importFrom aws.ec2 create_vpc
#' @importFrom aws.ec2 describe_vpcs
.awsDetectVpc <- function(cidr = "10.0.0.0/16")
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
    ## Using default VPC settings
    ##    new_vpc <- create_vpc(cidr)
    ##    new_vpc
}



#' importFrom aws.ec2 describe_instances
.awsDetectMasterOnEC2 <- function()
{
    instances <- describe_instances()
    hostname <- system2("hostname", stdout=TRUE)
    hostname <- gsub("-",".", sub("ip-","", hostname))
    for (i in 1:length(instances)) {
        privateIpAddress = sapply(instances[[i]][["instancesSet"]], `[[`, "privateIpAddress")
        if (hostname == privateIpAddress[1]) {
            subnet <- sapply(instances[[i]][["instancesSet"]], `[[`, "subnetId")    
        }
    }
    if (is.na(subnet)) {
        stop("You are not a recognized AWS EC2 instance")
    } else {
        subnet
    }
}



#' Describe the Subnet that needs to be used
#'
#' @param vpc character subnet is created within the given VPC-ID
#' @return subnet information
#' @importFrom aws.ec2 create_subnet
#' @importFrom aws.ec2 describe_subnets
.awsDetectSubnet <- function(vpc)
{
    subnets <- describe_subnets()
    subnet_vpc_id <- sapply(subnets, `[[`, "vpcId")
    ## Find subnet with same VPC ID
    idx <- grep(vpc$vpcId, subnet_vpc_id)
    ## Get subnet in VPC
    if (length(idx) >= 1) {
        one_idx = idx[1]
        awsSubnet <- subnets[[one_idx]]
        ## TODO: This is a hack,
        checkSubnetOnMaster <- .awsDetectMasterOnEC2()
        if (checkSubnetOnMaster != awsSubnet) {
            awsSubnet <- checkSubnetOnMaster
        }
    } else {
        ## If no subnet is available in that VPC,
        ## create one
        awsSubnet <- create_subnet(vpc, cidr=vpc$cidrBlock)
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
.awsDetectSecurityGroup <- function(vpc)
{
    ## TODO: add error checking to see if sg exists
    sgroups <- describe_sgroups()
    group_names <- sapply(sgroups, `[[`, "groupName")
    idx <- grep("AWSParallel_sgroup", group_names)
    if (length(idx) !=0 ) {
        sg <- sgroups[[idx]]
    } else {
        ## create sgroup
        sg <- create_sgroup(name="AWSParallel_sgroup",
                            description="Security group for AWSParallel",
                            vpc = vpc)
        ## Add TCP port range between 11000 to 11999
        authorize_ingress(sg, port=c(11000,11999),
                          protocol="tcp", cidr=vpc$cidrBlock)
        ## Add SSH 22 port
        authorize_ingress(sg, port=22,
                          protocol="tcp", cidr="0.0.0.0/0")
    }
    sg
}

#' Get AWS security requirements
#'
#' Security requirements to launch the EC2 instances into
#' a VPC, subnet, and security group
#' @return list, containing VPC, subnet, security group information
#' @export
getAwsRequirements <- function()
{
    ## If user passes in CIDR block, does it get passed in?
    vpc <- .awsDetectVpc()
    subnet <- .awsDetectSubnet(vpc)
    sg <- .awsDetectSecurityGroup(vpc)
    ## Return a named list of vpc, subnet and security group
    list(vpc=vpc, subnet=subnet, sgroup=sg)
}
