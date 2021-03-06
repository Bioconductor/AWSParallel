% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AWSBatchJobsParam-class.R
\name{bpsetup}
\alias{bpsetup}
\title{Setup a new AWS EC2 cluster}
\usage{
bpsetup(x, clustername = "awsparallel")
}
\arguments{
\item{x}{AWSBatchJobsParam object}

\item{clustername}{character value given to the cluster.}
}
\description{
The step bpsetup is required before using any of the conventional
BiocParallel functions like bpstart, bpstop. It is used to setup
or start a new or existing cluster on the user's AWS account. Once
a cluster is up an running, it should be safely suspended or terminated
using functionality like 'bpsuspend' and 'bpteardown'. NOTE: This function
takes a while to process, depending on the number of workers needed
it may take upto 4-5 minutes.
}
