# Tomorrow, June 18th: Get list working for large numbers 
# Tomorrow, June 18th: Make linear model
# Tomorrow, June 18th: Make quadratic model if I can

# api.R

library(plumber)
library(httr)
library(ggplot2)

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


#* Print a picture of the plot of a given sequence
#* param seqID:str The sequence ID
#* @get /getPic
#* @serializer png
function(seqID){

  # url <- "https://oeis.org/A000045/b000045.txt"

  A_seqID <- paste0("A", seqID)
  b_seqID <- paste0("b", seqID)
  url <- paste0("https://oeis.org/", A_seqID, "/", b_seqID, ".txt")
  
  data <- read.table(url, colClasses = c("integer", "numeric"))

  index_values <- data[[1]]
  nums_values <- data[[2]]
  
  index <- index_values[1:20]
  nums <- nums_values[1:20]
  
  index_vector <- as.vector(index)
  nums_vector <- as.vector(nums)

  df <- data.frame(x = index_vector, y = nums_vector)

  (print(ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    geom_point()))
  print("picture made!")
}

plot_of_numbers <- function(){

}


# A_seqID <- paste0("A", seqID)
  # b_seqID <- paste0("b", seqID)
  # url <- paste0("https://oeis.org/", A_seqID, "/", b_seqID, ".txt")
  # test_url <- "https://oeis.org/A000045/b000045.txt"

  # res <- GET(test_url)

  # if(status_code(res) != 200){
  #   stop("Error fetching B-files")
  # }

  # text_data <- content(res, as = "text", encoding = "UTF-8")
  # lines <- strsplit(text_data, "\n")[[1]]
  # # to avoid truncation because the numbers are huge use colClasses = c("character", "character") to avoid coercion. 
  # data <- read.table(textConnection(lines), header=FALSE, colClasses = c("character", "character"))
  
  # print(head(data, 400))
  # return(lines)