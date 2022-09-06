### read in google sheet
#googlesheets4::gs4_auth()

get_google_sheet<- function(ss = "https://docs.google.com/spreadsheets/d/1FWuhJKQ99lyFJSHWTJ6wRp71IxLbAPDtf9d-F4sVaX4/edit#gid=294483193", 
                 sheet = "Data Collection",
                 skip = 1){

  repo_sheet <- googlesheets4::read_sheet(ss = ss,
                                          sheet = sheet,skip = skip
  )

  return(repo_sheet)
}


get_repo_urls <- function(ss = "https://docs.google.com/spreadsheets/d/1FWuhJKQ99lyFJSHWTJ6wRp71IxLbAPDtf9d-F4sVaX4/edit#gid=294483193", 
                         sheet = "Data Collection",...){
 
  repo_sheet <- get_google_sheet(ss, sheet,...)
  
  upper_range <- nrow(repo_sheet)
  
  cell_range <- glue::glue("{sheet}!B3:B{upper_range}")
  
  cells_read <- googlesheets4::range_read_cells(ss = ss,range = cell_range,cell_data = "full")
  
  repo_df <- cells_read$cell %>% 
    purrr::map_df(function(x){
      
      repo_name <- x$formattedValue
      if(is.null(repo_name)){
        repo_name <- NA
      }
      
      repo_url <- x$hyperlink
      if(is.null(repo_url)){
        repo_url <- NA
      }
      
      data.frame(repo_name = repo_name, url =repo_url)
    })  
  
  return(repo_df)

}

get_repo_ids <- function(ss = "https://docs.google.com/spreadsheets/d/1FWuhJKQ99lyFJSHWTJ6wRp71IxLbAPDtf9d-F4sVaX4/edit#gid=294483193", 
                          sheet = "Data Collection",...){
  
  #browser()
  
  repo_sheet <- get_google_sheet(ss, sheet,...)
  
  repo_id <- repo_sheet$`re3data identifier`
  repo_name <- repo_sheet$`Repository Name`
  
  df <-data.frame("repo_name" = repo_name, "repo_id" = repo_id)
  
  return(df)
}


 




