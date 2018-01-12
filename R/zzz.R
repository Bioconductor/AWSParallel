.onLoad <-
    function(libname, pkgname)
{
    test <- !(file.exists("~/.starcluster/config") &&
              file.exists("~/.aws/credentials"))
    if (test)
        warning("You need to provide arguments for invoking AWSBatchJobsParam")
    else{
        res <- tryCatch({
            register(AWSBatchJobsParam())
        }, error = function(e) {
            warning("Missing arguments for AWSBatchJobsParam")
        })
    }
}
