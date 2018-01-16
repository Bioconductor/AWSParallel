.STARCLUSTER_CONFIG_PATH <- "~/.starcluster/config"

.registerOnStartup <-
    function()
{
    test <- !file.exists(.STARCLUSTER_CONFIG_PATH)
    if (test)
        warning(
            "'AWSBatchJobsParam()' registered without starcluster configuration; see ?AWSBatchJobsParam",
            call.=FALSE
        )
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
