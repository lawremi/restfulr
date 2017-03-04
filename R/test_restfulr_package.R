.test <- function() {
    if (!requireNamespace("rsolr")) {
        warning("install rsolr to perform tests")
        return()
    }
    solr <- rsolr::TestSolr()
    on.exit(solr$kill())
    testPackage <- get("testPackage", getNamespace("BiocGenerics"))
    testPackage("restfulr")
}
