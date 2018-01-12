.onLoad <-
    function(libname, pkgname)
{
    test <- !(file.exists("~/.starcluster/config") &&
              file.exists("~/.aws/credentials"))
    if (test)
        warning("You need to provide arguments for invoking AWSBatchJobsParam")
    else{
        register(AWSBatchJobsParam())
    }
}
