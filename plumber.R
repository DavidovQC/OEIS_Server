#To do: 

# Done:
# 8. Rational fits P(x)/Q(x) (hard) - x
# 7. Recurrence fits (?) (Hard) 
# 2. logarithmic (easy)

# Day 1:
# 1. polynomial of degree n (easy)
# 3. Power law ax^b (easy) 
# 5. Piecewise (medium) 
# 6. Factorial (medium) (maybe)
# 9. Polynomial interpolation (hard) 
# 4. rsin(\Omega x) + rcos(\Omega x) (hard) (maybe)
# 10. Discrete Fourier ? 
# 11. Compute Continued fraction (?)


# Day 3
# Set title and set formulas for each graph

# Day 3:
# Conduct meta analysis of all sequences? 
# Uploading csv of sequences

# Day 4:
# Probabilistic dependence
# Improve Search to not require seqID



# Fun note: the cache wasn't working and each function was caching 
#the sequence over and over again 
# - I asked chatGPT what the problem was and it recommended I
# Implement a "lock file" (???) to "ensure single creation" - 
# the actual problem was I was doing 
#if(!file.exists(paste0("./cache", seqID))) instead of 
# if(!file.exists(paste0("./cache/", seqID)))



# api.R
source("algorithms.R")
source("caching.R")

library(plumber)
library(httr)
library(ggplot2)
library(pracma)


getwd()

# library(gmp)

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

lim <- 25

for(n in 1:lim){
    seqID <- pad_id(n)
    url <- paste0("https://oeis.org/search?fmt=json&q=A", seqID)
    
    res <- GET(url)

    if(status_code(res) != 200){
        print("Failed to fetch OEIS data")
    } else {
        content_data <- content(res, as = "parsed", encoding = "UTF-8")[[1]]
        print(content_data$name)
        result <- list(
            title = content_data$name,
            data = content_data$data,
            seqID = seqID
        )
        saveRDS(result, file=paste0("./cache/data/", seqID))
    }
    Sys.sleep(0.1)
}

# for(n in 1:lim){
#   seqID <- pad_id(n)
#   print(readRDS(paste0("./cache/data/", seqID))$title)
# }

#* Return the data of all sequences from 1 to lim
#* @get /getAllSequenceData
function(){
  all_data <- list()

  for(n in 1:lim){
    seqID <- pad_id(n)
    file_path <- paste0("./cache/data/", seqID)

    if(verify_data_cached(seqID)){
      data <- readRDS(file_path)
      all_data[[n]] <- data
    }
  }

  return(all_data)
}



#* Print a picture of the plot of a given sequence
#* param seqID:str The sequence ID
#* @get /getSeqPNG
#* @serializer png list(width = 800, height = 500)
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    create_df_and_cache(seqID)
  }


  result <- readRDS(paste0("./cache/models/", seqID))
  p <- draw_graph(seqID)

  print(p)

}


#* Print a picture of the plot of a given sequence with its linear fit
#* param seqID:str The sequence ID
#* @get /getSeqLinearModelPNG
#* @serializer png
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    print("df created in /getSeqLinearModelPNG")
    print(seqID)
    create_df_and_cache(seqID)
  }

  p <- draw_graph_with_linear_fit(seqID)
  print(p)
}


#* Print a picture of the plot of a given sequence with a quadratic fit
#* param seqID:str The sequence ID
#* @get /getSeqQuadraticModelPNG
#* @serializer png
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    create_df_and_cache(seqID)
    print("df created in /setQuadraticModelPNG")
    print(seqID)
  }

  p <- draw_graph_with_quadratic_fit(seqID)
  print(p)
}


#* Print a picture of the plot of a given sequence with an exponential fit
#* param seqID:str The sequence ID
#* @get /getSeqExpModelPNG
#* @serializer png
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    create_df_and_cache(seqID)
    print("df created in /getSeqExpModelPNG")
    print(seqID)
  }

  p <- draw_graph_with_exp_fit(seqID)
  print(p)
}


#* Print a picture of the plot of a given sequence with an exponential fit
#* param seqID:str The sequence ID
#* @get /getSeqRationalModelPNG
#* @serializer png
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    create_df_and_cache(seqID)
    print("df created in /getSeqRationalModelPNG")
    print(seqID)
    df <- readRDS(paste0("./cache/", seqID))$df
  }

  p <- draw_graph_with_rational_fit(seqID)
  print(p)
}

#* Print a picture of the plot of a given sequence with an exponential fit
#* param seqID:str The sequence ID
#* @get /getSeqLogModelPNG/<seqID>
#* @serializer png
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    create_df_and_cache(seqID)
    print("df created in /getSeqLogModelPNG")
    print(seqID)
    df <- readRDS(paste0("./cache/", seqID))$df
  }

  p <- draw_graph_with_log_fit(seqID)
  print(p)
}





#* Print a picture of the plot of a given sequence with an exponential fit
#* param seqID:str The sequence ID
#* @get /getSeqRecurrenceModelPNG
#* @serializer png
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    create_df_and_cache(seqID)
    print("df created in /getSeqRecurrenceModelPNG")
    print(seqID)
    df <- readRDS(paste0("./cache/", seqID))$df
  }
  p <- draw_graph_with_recurrence_fit(seqID)
  print(p)
}

#* Return a json object with m, b the slope and y-intercept of the linear fit
#* param seqID:str The sequence ID
#* @get /getLinearCoeffs
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    create_df_and_cache(seqID)
    print("df created in /getLinearCoeffs")
  }

  return(linear_coeffs(seqID))
}


#* Return an object with a, b, c, ax^2+bx+c is the best fit quadratic for SeqID
#* param seqID:str The sequence ID
#* @get /getQuadraticCoeffs
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    create_df_and_cache(seqID)
    print("df created in /getQuadraticCoeffs")
  }

  return(quadratic_coeffs(seqID))
}

#* Return an object with a, b, c, d, e, f such that y ~ (ax^2+bx+c)/(dx^2+ex+f)
#* param seqID:str The sequence ID
#* @get /getRationalCoeffs
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    create_df_and_cache(seqID)
    print("df created in /getQuadraticCoeffs")
  }

  return(rational_coeffs(seqID))
}


#* Return an object with (a,b) such that y ~ ae^b
#* param seqID:str The sequence ID
#* @get /getExponentialCoeffs
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    create_df_and_cache(seqID)
    print("df created in /getExponentialCoeffs")
  }

  return(exp_coeffs(seqID))
}

#* Return an object with (a,b) such that y ~ ae^b
#* param seqID:str The sequence ID
#* @get /getLogarithmicCoeffs
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    create_df_and_cache(seqID)
    print("df created in /getLogarithmicCoeffs")
  }

  return(logarithmicCoeffs(seqID))
}

#* Return an object with (a,b) x_n = ax_n-1 + bx_n-2
#* param seqID:str The sequence ID
#* @get /getRecursiveCoeffs
function(seqID){
  if(!file.exists(paste0("./cache/models/", seqID))){
    create_df_and_cache(seqID)
    print("df created in /getRecursiveCoeffs")
  }

  df <- readRDS(paste0("./cache/models/", seqID))$df
 
  k <- 2
  E <- embed(df$value, k + 1)

  X <- E[1:2, 2:3]
  y <- E[1:2, 1]

  if (abs(det(X)) < 1e-8) {
    coeffs_rec <- c(0, 0)
    names(coeffs_rec) <- c("c1", "c2")
  } else {
    coeffs_rec <- solve(X, y)
    names(coeffs_rec) <- c("c1", "c2")
  }

  recursive_coeff1 <- coeffs_rec["c1"]
  recursive_coeff2 <- coeffs_rec["c2"]
  print(recursive_coeff1)
  print(recursive_coeff2)
  print(coeffs_rec)
  
  return(coeffs_rec)
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
  s <- "1,2,3,6"
  s_num <- as.numeric((strsplit(s, ",")[[1]]))
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

  return(num_vector[29])
}
