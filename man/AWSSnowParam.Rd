% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AWSSnowParam-class.R
\name{AWSSnowParam}
\alias{AWSSnowParam}
\title{AWSSnowParam function to start an AWS EC2-instance cluster}
\usage{
AWSSnowParam(workers = 2, awsCredentialsPath = NA_character_,
  awsInstanceType = NA_character_, awsSubnet = NA, awsSecurityGroup = NA,
  awsAmiId = NA_character_, awsSshKeyPair = NA_character_,
  awsProfile = "default", user = "ubuntu", rhome = "/usr/local/lib/R",
  bplib = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4/BiocParallel",
  rscript = "/usr/local/bin/Rscript", outfile = "/home/ubuntu/snow.log",
  verbose = FALSE)
}
\arguments{
\item{workers}{Numeric, number of workers to launch in the cluster}

\item{awsCredentialsPath}{character, Path to AWS credentials,
default value is `~/.aws/credentials`}

\item{awsInstanceType}{character, Type of AWS EC2-instance,
eg. t2.micro}

\item{awsSubnet}{character, AWS EC2-instance subnet, within a
certain VPC}

\item{awsSecurityGroup}{character, Security group which assigns
    inbound and outbound traffic at the instance level. The
    security group needs to be
*Inbound rules*
Protocol type   Port number   Source IP
          TCP      22 (SSH)   0.0.0.0/0
          TCP   11000-11999   CIDR-Block same as VPC
*Outbound rules*
Protocol type   Port number   Destination IP
          All           All   0.0.0.0/0}

\item{awsAmiId}{character, AMI(amazon machine image) ID for the
Bioconductor-release version}

\item{awsSshKeyPair}{character, SSH key pair, to associate with
your AWS EC2-instance}

\item{awsProfile}{character, indicates what profile to use while
using AWS credentials}

\item{verbose}{logical, gives a verbose output of SSH
connection attempt, default is FALSE.}
}
\value{
AWSSnowParam object
}
\description{
This function starts a cluster of AWS EC2-instances to allow
parallel computation of R objects, and works with BiocParallel, to
allow computation with R/Bioconductor objects
}
\examples{
\dontrun{
        ## Minimal example
        aws <- AWSSnowParam(workers = 1,
               awsInstanceType="t2.micro",
               awsAmiId= image,
               awsSshKeyPair = "~/.ssh/<my_aws_key_pair>.pub")
}
}
