
# Tomorrow, June 18th: Make linear model - DONE
# Tomorrow, June 19th: Return the coefficients of the intercept and the slope
# Tomorrow, June 19th: Make quadratic model
# Tomorrow, June 19th: Get list working for large numbers 
# Tomorrow, June 19th: Implement Caching

# api.R
source("algorithms.R")

library(plumber)
library(httr)
library(ggplot2)

getwd()

# library(gmp)

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
  A_seqID <- paste0("A", seqID)
  url <- paste0("https://oeis.org/search?fmt=json&q=", seqID)
  print(url)
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


#* Get the elements of a sequence from the B list of the OEIS website
#* param seqID:str The sequence ID
#* @get /seqB
#* @serializer text
function(seqID){
  url <- "https://oeis.org/A000045/b000045.txt"
  
  data <- read.table(url, colClasses = c("integer", "character"))

  index_values <- data[[1]]
  nums_values <- data[[2]]
  
  index <- index_values[1:100]
  nums <- nums_values[1:100]
  
  index_vector <- as.vector(index)
  num_vector <- as.vector(nums)
  
  print(index_vector)
  print(num_vector)
  return(num_vector[29])
}

plot_of_numbers <- function(){

}


# Problem is that seqID is being interpretted as a number and not a string
model_cache <- new.env()

#* Print a picture of the plot of a given sequence
#* param seqID:str The sequence ID
#* @get /getSeqPNG
#* @serializer png list(width = 800, height = 500)
function(seqID){
  if(!exists(seqID, envir=model_cache)){
      create_df_and_cache(seqID, model_cache)
  }

  p <- draw_graph(seqID, model_cache)
  print(p)

}


#* Print a picture of the plot of a given sequence with its linear fit
#* param seqID:str The sequence ID
#* @get /getSeqLinearModelPNG
#* @serializer png
function(seqID){
  if(!exists(seqID, env=model_cache)){
    create_df_and_cache(seqID, model_cache)
  }

  p <- draw_graph_with_linear_fit(seqID, model_cache)
  print(p)
}


#* Print a picture of the plot of a given sequence with a quadratic fit
#* param seqID:str The sequence ID
#* @get /getSeqQuadraticModelPNG
#* @serializer png
function(seqID){
  if(!exists(seqID, env=model_cache)){
    create_df_and_cache(seqID, model_cache)
  }

  p <- draw_graph_with_quadratic_fit(seqID, model_cache)
  print(p)
}

#* Print a picture of the plot of a given sequence with a quadratic fit
#* param seqID:str The sequence ID
#* @get /getLinearCoeffs
function(seqID){
  
}