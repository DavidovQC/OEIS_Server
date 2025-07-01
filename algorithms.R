library(ggplot2)

create_df_and_cache <- function(seqID, model_cache){
    length <- 100
    url <- createBListURL(seqID)
    df <- read.table(url, colClasses = c("integer", "numeric"), col.names = c("index", "value"), nrows = length)
    result <- analyze(df)
    cache_result(seqID, result, model_cache)
}

cache_result <- function(seqID, result, model_cache){
    model_cache[[seqID]] <- result
}

analyze <- function(df){
    linear_model <- lm(value~index, data=df)
    df$linear_fit <- predict(linear_model)
    
    quadratic_model <- lm(value~index + I(index^2), data = df)
    df$quadratic_fit <- predict(quadratic_model)
    

    return(list(df = df, linear_model = linear_model, quadratic_model = quadratic_model))
}

draw_graph <- function(seqID, model_cache){
    df <- model_cache[[seqID]]$df
    return(
        ggplot(df, aes(x=index, y=value))
        +geom_point()
        +geom_line()
    )
}

draw_graph_with_linear_fit <- function(seqID, model_cache){
    df <- model_cache[[seqID]]$df
    return(
        ggplot(df, aes(x=index, y=value))
        +geom_point()
        +geom_line()
        +geom_line(aes(y=linear_fit), color='red')
    )
}

draw_graph_with_quadratic_fit <- function(seqID, model_cache){
    df <- model_cache[[seqID]]$df
    return(
        ggplot(df, aes(x=index, y=value)) 
        +geom_point()
        +geom_line()
        +geom_line(aes(y=quadratic_fit), color='red')
    )
}




createBListURL <- function(seqID){
    # example URL: "https://oeis.org/A000045/b000045.txt"
    A_seqID <- paste0("A", seqID)
    b_seqID <- paste0("b", seqID)
    url <- paste0("https://oeis.org/", A_seqID, "/", b_seqID, ".txt")
    return(url)
}
