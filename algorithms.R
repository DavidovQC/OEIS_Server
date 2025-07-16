library(ggplot2)
library(pracma)

# source("caching.R")

create_df_and_cache <- function(seqID){
    length <- 100
    url <- createBListURL(seqID)
    df <- read.table(url, colClasses = c("integer", "numeric"), col.names = c("index", "value"), nrows = length)
    result <- analyze(df)
    cache_result(seqID, result)
}

cache_result <- function(seqID, result){
    saveRDS(result, file=paste0("./cache/models", seqID))
}


analyze <- function(df){
    # linear fit
    linear_model <- lm(value~index, data=df)
    df$linear_fit <- predict(linear_model)
    

    # quadratic fit
    quadratic_model <- lm(value~index + I(index^2), data = df)
    df$quadratic_fit <- predict(quadratic_model)

    #-----------------------------------rational fit-----------------------------------

    rational_poly <- rationalfit(df$index, df$value, 3, 3)
    rational_func <- function(x) {
        polyval(rational_poly$p1, x) / polyval(rational_poly$p2, x)
    }

    imag_parts <- Im(rational_func(df$index))
    max_imag <- max(abs(imag_parts))
    if (max_imag < 1e-10) {
        df$rational_fit <- Re(rational_func(df$index))
    } else {
        warning("Non-negligible imaginary components detected in fit.")
    }

    # -----------------------------------exponential fit-----------------------------------
    #choose only values which are > 0.
    # Notes: For values that oscilate from positive to negative
    # this will show as a "spiked" exponential - see A000009
    df_pos <- df[df$value > 0,  ]

    if(nrow(df_pos) > 1) {
        exp_model <- lm(log(value) ~ index, data=df_pos)

        log_a <- coef(exp_model)[1]
        b <- coef(exp_model)[2]
        a <- exp(log_a)
        df$exp_fit <- 0
        df$exp_fit[df$value > 0] <- a * exp(b * df$index[df$value > 0])
    } else {
        exp_model <- NULL
        df$exp_fit <- 0
    }


    # -----------------------------------logarithmic fit-----------------------------------
    print("log called")
    df_ind_pos <- df[df$index > 0, ]
    log_model <- lm(value ~ log(index), data=df_ind_pos)
    df$log_fit <- 0
    df$log_fit[df$index > 0] <- predict(log_model, newdata = df[df$index > 0, ])


    #-----------------------------------recurrence fit-----------------------------------
    #To do: return iff non-degenerate case
    #(e.g, A000003 where first two elts are equal)
    #(e.g, A000007, 1, 0, 0, 0, 0, 0....)
    #(in such cases set c1 = 0, c2 =0)



    # k <- 2
    # E <- embed(df$value, k + 1)

    # coeffs_rec <- solve(E[1:2, 2:3], E[1:2, 1])
    # names(coeffs_rec) <- c("c1","c2")
    
    k <- 2
    E <- embed(df$value, k + 1)

    X <- E[1:2, 2:3]
    y <- E[1:2, 1]

    if (abs(det(X)) < 1e-8) {
    coeffs_rec <- c(c1 = 0, c2 = 0)
    } else {
    coeffs_rec <- solve(X, y)
    names(coeffs_rec) <- c("c1", "c2")
    }


    
    #returns n-1th value
    recurrence_function_pre <- function(n){
        values <- df$value[1:k]
        if(n <= k){
            return(values[n])
        }

        for(i in (k+1):n){
            val = sum(values[(i-k):(i-1)] * rev(coeffs_rec))
            values <- c(values, val)
        }

        return(values[n])

    }

    #true recurrence function
    recurrence_function <- function(n){
        recurrence_function_pre(n + 1)
    }

    df$recurrence_fit <- sapply(df$index, recurrence_function)



    return(
        list(
            df = df,  
            linear_model = linear_model, 
            quadratic_model = quadratic_model, 
            exp_model = exp_model,
            rational_model = rational_poly,
            log_model = log_model,
            recurrence_function = recurrence_function
        )
    )
}

draw_graph <- function(seqID){
    df <- readRDS(paste0("./cache/models/", seqID))$df
    return(
        ggplot(df, aes(x=index, y=value))
        +geom_point()
        +geom_line()
    )
}


draw_graph_with_linear_fit <- function(seqID){
    df <- readRDS(paste0("./cache/models/", seqID))$df
    return(
        ggplot(df, aes(x=index, y=value))
        +geom_point()
        +geom_line()
        +geom_line(aes(y=linear_fit), color='red')
    )
}

draw_graph_with_quadratic_fit <- function(seqID){
    df <- readRDS(paste0("./cache/models/", seqID))$df
    return(
        ggplot(df, aes(x=index, y=value)) 
        +geom_point()
        +geom_line()
        +geom_line(aes(y=quadratic_fit), color='red')
    )
}

draw_graph_with_exp_fit <- function(seqID){
    df <- readRDS(paste0("./cache/models/", seqID))$df
    return(
        ggplot(df, aes(x=index, y=value))
        +geom_point()
        +geom_line()
        +geom_line(aes(y=exp_fit), color='red')
    
    )
}

draw_graph_with_rational_fit <- function(seqID){
    df <- readRDS(paste0("./cache/models/", seqID))$df
    return(
        ggplot(df, aes(x=index, y=value))
        +geom_point()
        +geom_line()
        +geom_line(aes(y=rational_fit), color='red')
    )
}

draw_graph_with_recurrence_fit <- function(seqID){
    df <- readRDS(paste0("./cache/models/", seqID))$df
    return(
        ggplot(df, aes(x=index, y=value))
        +geom_point()
        +geom_line()
        +geom_line(aes(y=recurrence_fit), color='red')
    )
}

draw_graph_with_log_fit <- function(seqID){
    df <- readRDS(paste0("./cache/models/", seqID))$df

    return(
        ggplot(df, aes(x=index, y=value))
        +geom_point()
        +geom_line()
        +geom_line(aes(y=log_fit), color='red')
    ) 
}


linear_coeffs <- function(seqID){
    linear_model <- readRDS(paste0("./cache/models", seqID))$linear_model
    coeffs <- round(linear_model$coefficients, 2)
    
    return(coeffs)
}

quadratic_coeffs <- function(seqID){
    quadratic_model <- readRDS(paste0("./cache/models", seqID))$quadratic_model
    coeffs <- round(quadratic_model$coefficients, 2)
    
    return(coeffs)
}

exp_coeffs <- function(seqID){
    exp_model <- readRDS(paste0("./cache/models", seqID))$exp_model
    if(is.null(exp_model)){
        return(c(0, 0))
    }
    coeffs <- round(exp_model$coefficients, 2)

    return(coeffs)
}

createAListURL <- function(seqID){
    # example URL: "https://oeis.org/A000045"
    url <- paste0("https://oeis.org/", seqID)
    return(url)
}


createBListURL <- function(seqID){
    # example URL: "https://oeis.org/A000045/b000045.txt"
    A_seqID <- paste0("A", seqID)
    b_seqID <- paste0("b", seqID)
    url <- paste0("https://oeis.org/", A_seqID, "/", b_seqID, ".txt")
    return(url)
}

get_data_and_cache <- function(seqID){

    datum_url <- "https://oeis.org/search?fmt=json&q=A000004"
    res <- GET(datum_url)

    if(status_code(res) != 200){
        print("Failed to fetch OEIS data")
    } else {
        content_data <- content(res, as = "parsed", encoding = "UTF-8")[[1]]

        result <- list(
            title = content_data$name,
            data = content_data$data,
            seqID = seqID
        )
        saveRDS(result, file=paste0("./cache/data/", seqID))
    }

}

pad_id <- function(n){
    sprintf("%06d", n)
}