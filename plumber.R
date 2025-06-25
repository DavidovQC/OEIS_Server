
# Tomorrow, June 18th: Make linear model - DONE
# Tomorrow, June 19th: Return the coefficients of the intercept and the slope
# Tomorrow, June 19th: Make quadratic model
# Tomorrow, June 19th: Get list working for large numbers 
# Tomorrow, June 19th: Implement Caching
# Tomorrow, June 19th: https://chatgpt.com/c/6852b849-c900-800c-a840-e87fe5e0ccd3

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


# Problem is that seqID is being interpretted as a number and not a string
model_cache <- new.env()
model_cache[["000045"]] <- TRUE

print(model_cache)

#* Print a picture of the plot of a given sequence
#* param seqID:str The sequence ID
#* @get /getLinearPic
#* @serializer png
function(seqID){
  if(exists(seqID, env=model_cache)){
    print("model_cache contains")
    print(seqID)
  } else {

  }
}
#* Print a picture of the plot of a given sequence
#* param seqID:str The sequence ID
#* @get /getPic
#* @serializer png
function(seqID){

  if(exists(seqID, envir=model_cache)){
    print("WORKING!")
    print(model_cache)
    print(ls(model_cache))
    print(ggplot(model_cache[[seqID]], aes(x=x, y=y)) 
    + geom_line() 
    + geom_point())
    # print("dataframe retrieved from cache")
    # print(ggplot(model_cache[[seqID]], aes(x=x, y=y)))
  }
  else {
    # url <- "https://oeis.org/A000045/b000045.txt"

    A_seqID <- paste0("A", seqID)
    b_seqID <- paste0("b", seqID)
    url <- paste0("https://oeis.org/", A_seqID, "/", b_seqID, ".txt")
    
    data <- read.table(url, colClasses = c("integer", "numeric"))

    index_values <- data[[1]]
    nums_values <- data[[2]]
    
    index <- index_values[1:100]
    nums <- nums_values[1:100]
    
    index_vector <- as.vector(index)
    nums_vector <- as.vector(nums)

    df <- data.frame(x = index_vector, y = nums_vector)

    linear_model <- lm(y ~ x, data=df)
    df$linear_fit <- predict(linear_model) 
    model_cache[[seqID]] <- df
    print((seqID))
    
    print(ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      geom_point() + 
      geom_line(aes(y=linear_fit), color="red"))

    print("picture made!")
    print("dataframe added to cache!")

  }
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