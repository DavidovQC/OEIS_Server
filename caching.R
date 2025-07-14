source("algorithms.R")

ensure_cache_df <- function(seqID){
    if(!file.exists(paste0("./cache/", seqID))){
        print("df created:")
        print(seqID)
        create_df_and_cache(seqID)
    }
}

create_df_and_cache <- function(seqID){
    length <- 100
    url <- createBListURL(seqID)
    df <- read.table(url, colClasses = c("integer", "numeric"), col.names = c("index", "value"), nrows = length)
    result <- analyze(df)
    cache_result(seqID, result)
}

cache_result <- function(seqID, result){
    saveRDS(result, file=paste0("./cache/", seqID))
}