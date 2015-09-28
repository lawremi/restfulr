.test <- function() {
    solr <- rsolr::TestSolr()
    on.exit(solr$kill())
    BiocGenerics:::testPackage("restfulr")
}
