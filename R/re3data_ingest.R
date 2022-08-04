### query re3data api


## this seems like a good candidate for a methods based approach
extract_repository_info <- function(repository_metadata_XML) {
  list(
    re3data_ID = xml_text(xml_find_all(repository_metadata_XML, "//r3d:re3data.orgIdentifier")),
    repositoryName = xml_text(xml_find_all(repository_metadata_XML, "//r3d:repositoryName")),
    repositoryUrl = xml_text(xml_find_all(repository_metadata_XML, "//r3d:repositoryURL")),
    description = xml_text(xml_find_all(repository_metadata_XML, "//r3d:description"))
  )
}

# test hyperlinks
re3data_ingest <- function(repo_df){

  # check that required fields are in the df
  if(!all(c("url","repo_name") %in% names(repo_df))){
    stop("repo_df must contain the fields: url and repo_name")
  }
  
  purrr::map2_dfr(repo_df$url,repo_df$repo_name, function(x,y){
    
    ## create empty dataframe
    
    repository_info <- data.frame(matrix(ncol = 4, nrow = 0))
    
    colnames(repository_info) <- c("re3data_ID", "repositoryName", "repositoryUrl", "description")
    
    
    if(is.na(x)){
      repository_info[1,] <- NA
      repository_info$nnlm_name <- y
      repository_info$nnlm_url <- x
      return(repository_info)
    }
    
    ## create a query
    re3data_query <- list("query" = x)
    
    ## get query results
    re3data_request <- GET("https://www.re3data.org/api/beta/repositories?", query = re3data_query) 
    
    URLs <- xml_text(xml_find_all(read_xml(re3data_request), xpath = "//@href"))
    
    
    ## pull info from repo
    
    for (url in URLs) {
      repository_metadata_request <- GET(url)
      repository_metadata_XML <-read_xml(repository_metadata_request) 
      results_list <- extract_repository_info(repository_metadata_XML)
      repository_info <- rbind(repository_info, results_list)
    }
    
    if(nrow(repository_info) == 0){
      repository_info[1,] <- NA
    }
    repository_info$nnlm_name <- y
    repository_info$nnlm_url <- x
    
    return(repository_info)
    
  })

}
