.STARCLUSTER_CONFIG_PATH <- "~/.starcluster/config"
.AWS_CREDENTIALS_PATH <- "~/.aws/credentials"

.registerOnStartup <-
    function()
{
    test <- !file.exists(.STARCLUSTER_CONFIG_PATH)
    ## If starcluster_config does NOT exists
    if (test)
        warning(
            "'AWSBatchJobsParam()' registered without starcluster configuration; see ?AWSBatchJobsParam",
            call.=FALSE
        )
    ## If starcluster_config does exist
    res <- tryCatch({
        aws <- AWSBatchJobsParam()
        register(aws)
    }, error = function(e) {
        warning(
            "'.onLoad()' failed to register 'AWSBatchJobsParam():",
            "\n  ", conditionMessage(e),
            call.=FALSE
        )
    })
}

.onLoad <-
    function(libname, pkgname)
{
    .registerOnStartup()
}
