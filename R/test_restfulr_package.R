.test <- function() {
    if (!requireNamespace("rsolr")) {
        stop("rsolr package required for testing")
    }
    solr <- rsolr::TestSolr()
    on.exit(solr$kill())
    testPackage <- get("testPackage", getNamespace("BiocGenerics"))
    testPackage("restfulr")
}
