# api.R

library(plumber)
library(httr)

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* Return Hello World
#* @get /hello-world
function(){
  "Hello, World!"
}

#* Echo back the input
#* @param msg:str The message to echo
#* @get /echo
function(msg="") {
  list(message = paste("You said:", msg))
}

#* Get the elements of a sequence
#* param seqID:str The sequence ID.
#* @get /seq
function(seqID){
  url <- paste0("https://oeis.org/search?fmt=json&q=", seqID)
  res <- GET(url)
  
  if(status_code(res) !=200){
    return("Error")
  }

  content_data <- content(res, as="parsed", encoding="UTF-8")
  arr_string <- content_data[[1]]$data
  arr_num <- as.numeric(strsplit(arr_string, ",")[[1]])
  print(arr_num)
  s <- "1,2,3,6"
  s_num <- as.numeric((strsplit(s, ",")[[1]]))
  print(s_num[4])
  return(arr_num);
}
