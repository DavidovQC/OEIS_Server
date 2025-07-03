library(ggplot2)


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


analyze <- function(df){
    linear_model <- lm(value~index, data=df)
    df$linear_fit <- predict(linear_model)
    
    quadratic_model <- lm(value~index + I(index^2), data = df)
    df$quadratic_fit <- predict(quadratic_model)

    df_pos <- df[df$value > 0,  ]
    exp_model <- lm(log(value) ~ index, data=df_pos)

    log_a <- coef(exp_model)[1]
    b <- coef(exp_model)[2]
    a <- exp(log_a)
    df$exp_fit <- 0
    df$exp_fit[df$value > 0] <- a * exp(b * df$index[df$value > 0])
    

    return(
        list(
            df = df,  
            linear_model = linear_model, 
            quadratic_model = quadratic_model, 
            exp_model = exp_model
        )
    )
}

draw_graph <- function(seqID){
    df <- readRDS(paste0("./cache/", seqID))$df
    return(
        ggplot(df, aes(x=index, y=value))
        +geom_point()
        +geom_line()
    )
}


draw_graph_with_linear_fit <- function(seqID){
    df <- readRDS(paste0("./cache/", seqID))$df
    return(
        ggplot(df, aes(x=index, y=value))
        +geom_point()
        +geom_line()
        +geom_line(aes(y=linear_fit), color='red')
    )
}

draw_graph_with_quadratic_fit <- function(seqID){
    df <- readRDS(paste0("./cache/", seqID))$df
    return(
        ggplot(df, aes(x=index, y=value)) 
        +geom_point()
        +geom_line()
        +geom_line(aes(y=quadratic_fit), color='red')
    )
}

draw_graph_with_exp_fit <- function(seqID){
    df <- readRDS(paste0("./cache/", seqID))$df
    return(
        ggplot(df, aes(x=index, y=value))
        +geom_point()
        +geom_line()
        +geom_line(aes(y=exp_fit), color='red')
    
    )
}

linear_coeffs <- function(seqID){
    linear_model <- readRDS(paste0("./cache/", seqID))$linear_model
    coeffs <- round(linear_model$coefficients, 2)
    
    return(coeffs)
}

quadratic_coeffs <- function(seqID){
    quadratic_model <- readRDS(paste0("./cache/", seqID))$quadratic_model
    coeffs <- round(quadratic_model$coefficients, 2)
    
    return(coeffs)
}

exp_coeffs <- function(seqID){
    exp_model <- readRDS(paste0("./cache/", seqID))$exp_model
    coeffs <- round(exp_model$coefficients, 2)

    return(coeffs)
}


createBListURL <- function(seqID){
    # example URL: "https://oeis.org/A000045/b000045.txt"
    A_seqID <- paste0("A", seqID)
    b_seqID <- paste0("b", seqID)
    url <- paste0("https://oeis.org/", A_seqID, "/", b_seqID, ".txt")
    return(url)
}
