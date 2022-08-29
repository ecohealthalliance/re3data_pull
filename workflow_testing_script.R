lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)

## get google sheet?

meta_data<- get_google_sheet(sheet = "NNLM Metadata",skip = 0)

meta_data_re3data <- meta_data %>% 
  filter(!is.na(`re3data Property ID`))

repo_df <-get_repo_ids()

#debugonce(re3data_ingest)

raw_re3data <- re3data_ingest(repo_df,meta_data_re3data$`re3data Property ID`)


# total records = 72
repo_df %>% nrow()

## unmatched - 35

raw_re3data %>% 
  filter(is.na(repositoryName)) %>% 
  select(nnlm_name,repositoryName) %>% 
  nrow()


## exact matches - 17
raw_re3data %>% 
  filter(repositoryURL == nnlm_url)

## split urls at ::
split_url <- stringr::str_split(raw_re3data$nnlm_url,pattern = ":")
## drop an s from https
regex_url <- map_chr(split_url, function(x){
  x[1] <- stringr::str_remove(x[1],"s")
  ## add s* suffix
  x[1] <- sprintf("%ss*",x[1])
  
  ## recombine urls
  unescaped_url <- sprintf("%s:%s$",x[1],x[2])
  escapced_url <- stringr::str_replace_all(unescaped_url,pattern = "\\.",replacement = "\\\\.")
  
  return(escapced_url)
})

## add regex url column
raw_re3data$regex_url <- regex_url

# http vs https matches - 23
matched_data <- raw_re3data %>% 
  filter(stringr::str_detect(repositoryURL,regex_url))


# rename fields to match spreadsheet

 matched_names <- which( meta_data_re3data$`re3data Property ID` %in% names(matched_data))

 names(matched_data)[matched_names] <-   meta_data_re3data$`Element Name`[matched_names]

# upload data to google sheets
  

googlesheets4::write_sheet(data = matched_data,sheet = "Data Collection- Re3data", ss = "https://docs.google.com/spreadsheets/d/1FWuhJKQ99lyFJSHWTJ6wRp71IxLbAPDtf9d-F4sVaX4/edit#gid=294483193" ) 
 



