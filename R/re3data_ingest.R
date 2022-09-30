### query re3data api

## this seems like a good candidate for a class based approach
extract_repository_info <- function(repository_metadata_XML,attributes) {
 # browser()
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

#' Query re3data for listed repos
#'
#' @param repo_df Data frame. Contains list of desired repos
#' @param req_fields Vector of strings of length 2. What fields will be used in purrr::map2
#' @param attributes Vector of strings. What attributes will be extracted from re3data.
#' @param repo_id_type String. either "repositoryUrl" or "re3data"
#' 
#' @return
#' @export
#'
#' @examples
re3data_ingest <- function(repo_df,
                           req_fields = c("repo_id", "repo_name"),
                           attributes,
                           repo_id_type = "re3data"){
  
  # check that required fields are in the df
  if(!all(req_fields %in% names(repo_df))){
    stop(glue::glue("repo_df must contain the fields: {req_fields[1]} and {req_fields[2]}"))
  }
  
  output_df <- purrr::map2_dfr(repo_df[,req_fields[1]],repo_df[,req_fields[2]], function(x,y){
    
    print(x)
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
      repository_info$repo_id <- x
      return(repository_info)
    }
    
    ## check id type
    if(repo_id_type == "repositoryUrl"){
      
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
    }
    
    if(repo_id_type == "re3data"){
      ## repo id request
      get_request <- sprintf("https://www.re3data.org/api/beta/repository/%s",x)
      ## get query results
      re3data_request <- GET(get_request) 
      repository_metadata_XML <-read_xml(re3data_request) 
      results_tibble <- extract_repository_info(repository_metadata_XML,attributes)
      
      repository_info <- rbind(repository_info, results_tibble)
      
    }
    
    if(nrow(repository_info) == 0){
      repository_info[1,] <- NA
    }
    repository_info$nnlm_name <- y
    repository_info$repo_id <- x
    
    return(repository_info)
  })
  
  return(output_df)
}
