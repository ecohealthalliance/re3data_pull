### query re3data api

## this seems like a good candidate for a class based approach
extract_repository_info <- function(repository_metadata_XML,attributes) {
  attr_list <- list()
  
  for(i in 1:length(attributes)){
    xml_element <- sprintf("//r3d:%s",attributes[i])
    attr_list[[i]] <- list(xml_text(xml_find_all(repository_metadata_XML, xml_element)))
  }

  names(attr_list) <- attributes
  ## consider reducing to character strings here
  df_attr <- purrr::map_dfr(attr_list,
             function(x){
              attr_value  <- ""
               #browser()
               if(!is_empty(x[[1]])){
                 attr_value <- purrr::reduce(flatten(x),paste, sep = "; ")
               }
              return(attr_value)
               })
  
  return(df_attr)
}

# test hyperlinks
re3data_ingest <- function(repo_df,attributes){

  # check that required fields are in the df
  if(!all(c("url","repo_name") %in% names(repo_df))){
    stop("repo_df must contain the fields: url and repo_name")
  }
  
  purrr::map2_dfr(repo_df$url,repo_df$repo_name, function(x,y){
    
    #browser()
    ## create empty dataframe
    
    empty_matrix <- matrix(
      ncol = length(attributes),
      nrow = 0)
    
    colnames(empty_matrix) <- attributes
    
    repository_info <- as_tibble(
      empty_matrix
     )
    
    
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
      #browser()
      repository_metadata_request <- GET(url)
      repository_metadata_XML <-read_xml(repository_metadata_request) 
      results_tibble <- extract_repository_info(repository_metadata_XML,attributes)
      
      repository_info <- rbind(repository_info, results_tibble)
    }
    
    if(nrow(repository_info) == 0){
      repository_info[1,] <- NA
    }
    repository_info$nnlm_name <- y
    repository_info$nnlm_url <- x
    
    return(repository_info)
    
  })

}
