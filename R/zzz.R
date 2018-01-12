.onLoad <-
    function(libname, pkgname)
{
    test <- !file.exists("~/.starcluster/config")
    if (test)
        warning("Cannot register AWSBatchJobsParam, without starcluster config and",
                " aws credentials. Pleaes read vignette for details.",
                call.=FALSE)
    res <- tryCatch({
        register(AWSBatchJobsParam())
    }, error = function(e) {
            warning("Missing arguments to AWSBatchJobsParam",
                    call.=FALSE)
    })
}
