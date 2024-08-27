overview_interactive1 <- function(my.inds, channel = 1, n.inds = c(1:length(my.inds)), 
    xlimi = c(min(ladder), max(ladder)), ladder, channel.ladder = dim(my.inds[[1]])[2], 
    ploidy = 2, dev = 50, method = "iter", init.thresh = 200, 
    ladd.init.thresh = 200, warn = TRUE, my.palette = NULL, env = parent.frame()) {
    
    if (dim(my.inds[[1]])[2] < channel.ladder) {
        stop("ERROR: The number of channels in the data is less than specified channel.ladder")
    }
    if (method == "ci") {
        warning("Ensure the same 'dev' value is used for ladder detection; otherwise, results may not match")
    }
    count <- 0
    tot <- length(n.inds)
    my.inds2 <- list(NA)
    for (i in 1:length(n.inds)) {
        v1 <- n.inds[i]
        my.inds2[[i]] <- my.inds[[v1]]
        names(my.inds2)[i] <- names(my.inds)[v1]
    }
    my.inds <- my.inds2
    ncfp <- c("COL1", "COL2", "COL3", "COL4", "COL5")
    if (!is.null(my.palette)) {
        cfp <- rep(my.palette, 100)
    }
    else {
        cfp <- c("cornflowerblue", "chartreuse4", "gold2", "red", 
            "orange", "purple")
    }
    list.data <- list(NA)
    if (exists("list.data.covarrubias")) {
        list.data <- env$list.data.covarrubias
    }
    else {
        list.ladders <- lapply(my.inds, function(x) {
            y <- x[, channel.ladder]
            return(y)
        })
        list.data <- lapply(list.ladders, find.ladder, ladder = ladder, 
            draw = FALSE, dev = dev, warn = warn, method = method, 
            init.thresh = ladd.init.thresh)
    }
    list.models <- lapply(list.data, function(da) {
        y <- da[[3]]
        x <- da[[1]]
        mod <- lm(y ~ I(x) + I(x^2) + I(x^3) + I(x^4) + I(x^5), 
            data = da)
        return(mod)
    })
    xx <- lapply(my.inds2, function(x, channel) {
        1:nrow(x)
    }, channel = channel)
    newxx <- numeric()
    newyy <- numeric()
    plot_data <- list()
    for (h in 1:length(xx)) {
        h1 <- n.inds[h]
        count <- count + 1
        newxx <- as.vector(predict(list.models[[h1]], newdata = data.frame(x = xx[[h]])))
        newyy <- my.inds2[[h]][, channel]
        plot_data[[h]] <- data.frame(x = newxx, y = newyy, Sample = names(my.inds2)[h])
    }
    plot_df <- do.call(rbind, plot_data)
    
    # Return data frame for plotting
    return(plot_df)
}
