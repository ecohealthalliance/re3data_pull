raw_re3data <- re3data_ingest(repo_df)

# total records = 104
repo_df %>% nrow()

## unmatched - 60

raw_re3data %>% 
  filter(is.na(repositoryName)) %>% 
  select(nnlm_name,repositoryName) %>% 
  nrow()
  

## exact matches - 20
raw_re3data %>% 
  filter(repositoryUrl == nnlm_url)

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

# http vs https matches - 26
raw_re3data %>% 
  filter(stringr::str_detect(repositoryUrl,regex_url)) %>% 
  View()
  



