% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AWSBatchJobsParam-class.R
\name{AWSBatchJobsParam}
\alias{AWSBatchJobsParam}
\title{AWSBatchJobsParam function to start an AWS EC2-instance cluster}
\usage{
AWSBatchJobsParam(workers = NULL,
  starclusterConfigPath = .STARCLUSTER_CONFIG_PATH,
  startclusterClusterId = "smallcluster", awsInstanceType = NA_character_,
  awsSubnet = NA_character_, awsAmiId = NA_character_,
  awsSshKeyPair = NA_character_, awsCredentialsPath = "~/.aws/credentials",
  awsProfile = "default", ...)
}
\arguments{
\item{workers}{Numeric, number of workers to launch in the cluster}

\item{awsInstanceType}{character, Type of AWS EC2-instance,
eg. t2.micro}

\item{awsSubnet}{character, AWS EC2-instance subnet, within a
certain VPC}

\item{awsAmiId}{character, AMI(amazon machine image) ID for the
Bioconductor-release version}

\item{awsSshKeyPair}{character, SSH key pair, to associate with
your AWS EC2-instance}

\item{awsCredentialsPath}{character, Path to AWS credentials,
default value is `~/.aws/credentials`}

\item{awsProfile}{character, indicates what profile to use while
using AWS credentials}

\item{...}{Additional arguments, used to initialize BatchJobsParam.}

\item{verbose}{logical, gives a verbose output of SSH connection
attempt, default is FALSE.}
}
\value{
AWSBatchJobsparam object
}
\description{
This function starts a cluster of AWS EC2-instances to allow
parallel computation of R objects using BatchJobs on SGE, and works
with BiocParallel, to allow computation with R/Bioconductor objects.
}
\examples{
\dontrun{
        ## Minimal example
        aws <- AWSBatchJobsParam(
                   workers = 2
                   awsCredentialsPath = "~/.aws/credentials"
                   awsInstanceType = "t2.micro"
                   awsSubnet = "subnet-d66a05ec"
                   awsAmiId = "ami-0454187e"
                   awsSshKeyPair = "mykey"
               )
}
}
