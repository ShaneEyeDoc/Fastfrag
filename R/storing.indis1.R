storing.inds1 <- function(folder, file, channels = NULL, fourier = TRUE, saturated = TRUE, 
                         lets.pullup = TRUE, plotting = FALSE, rawPlot = FALSE, llength = 3000, 
                         ulength = 80000) {
    if (missing(file)) {
        stop("Please specify a file to process.")
    }
    
    coli <- c("cornflowerblue", "chartreuse4", "gold2", "red", "orange", "purple")
    file_path <- file.path(folder, file)
    
    # Check file extension and set FSA flag
    FSA <- grepl(".fsa$", file)
    if (!FSA && !grepl(".txt$", file)) {
        stop("The specified file does not have a valid extension (.fsa or .txt):", 
             call. = FALSE)
    }
    
    all.inds.mats <- list(NA)
    if (FSA) {
        print("Reading FSA file")
    } else {
        print("Reading TXT file")
    }

    if (FSA) {
        fsaFile <- read.abif(file_path)
        lens <- lapply(fsaFile$Data, length)
        aaa <- table(unlist(lens))
        if (is.null(channels)) {
            channels <- as.vector(aaa[which(as.numeric(names(aaa)) > llength & as.numeric(names(aaa)) < ulength)])
        }
        if (length(channels) > 1) {
            cat("\nYour data has multiple possible places where run markers could be stored, and we don't know which is correct.\n")
            prov <- aaa[which(as.numeric(names(aaa)) > llength & as.numeric(names(aaa)) < ulength)]
            prov2 <- matrix(prov, nrow = 1)
            rownames(prov2) <- "number.of.channels.found"
            colnames(prov2) <- paste("Run_Length", names(prov), "indexes", sep = "_")
            prov2 <- rbind(prov2, prov2)
            rownames(prov2)[2] <- "number.to.type.if.selected"
            prov2[2, 1:ncol(prov2)] <- 1:ncol(prov2)
            cat("Please tell us which is the correct channel location\n\n")
            print(prov2)
            inut <- as.numeric(readline(prompt = "Enter one of the number.to.type: "))
            channels <- channels[inut]
        }
        real.len <- as.numeric(names(aaa)[which(aaa == channels & 
                                                as.numeric(names(aaa)) > llength & as.numeric(names(aaa)) < 
                                                ulength)])
        v <- as.vector(which(unlist(lens) == real.len))
        reads <- list(NA)
        for (j in 1:channels) {
            v2 <- v[j]
            reads[[j]] <- fsaFile$Data[[v2]]
        }
        prov <- matrix(unlist(reads), ncol = channels)
        colnames(prov) <- paste("channel_", 1:ncol(prov), sep = "")
        rownames(prov) <- paste("index_", 1:nrow(prov), sep = "")
        all.inds.mats[[1]] <- prov
        names(all.inds.mats)[1] <- as.character(file)
    } else {
        dodo <- scan(file_path, what = "a", nlines = 7, skip = 6, quiet = TRUE)
        chaco <- grep("FILTER", dodo)
        nchaco <- length(chaco)
        topick <- chaco[1]:(chaco[1] + (nchaco - 1))
        ds <- read.table(file_path, header = FALSE, skip = 84)
        all.inds.mats[[1]] <- as.matrix(ds[, topick])
        names(all.inds.mats)[1] <- as.character(file)
    }

    if (fourier == TRUE) {
        print("Applying Fourier transformation for smoothing...")
        all.inds.mats <- lapply_pb(all.inds.mats, function(x) {
            apply(x, 2, transfft)
        })
    }
    if (saturated == TRUE) {
        print("Checking and correcting for saturated peaks...")
        all.inds.mats <- lapply_pb(all.inds.mats, function(x) {
            apply(x, 2, saturate)
        })
    }
    if (FSA && lets.pullup == TRUE) {
        print("Applying pull up correction to decrease noise from channel to channel")
        if (plotting == TRUE) {
            all.inds.mats <- lapply_pb(all.inds.mats, pullup, channel = channels, plotting = TRUE)
        } else {
            all.inds.mats <- lapply_pb(all.inds.mats, pullup, channel = channels)
        }
    }
    if (rawPlot == TRUE) {
        layout(matrix(1:2, 2, 1))
        coli <- cfp <- c("cornflowerblue", "chartreuse4", "gold2", "red", "orange", "purple")
        naname <- c("FAM", "HEX", "NED", "ROX", "LIZ")
        print("Plotting raw data")
        plot(all.inds.mats[[1]][, 1], col = transp(coli[1], 0.6), type = "l", ylab = "RFU", 
             main = paste(naname[1], "channel. Plot 1 of", ncol(all.inds.mats[[1]])), las = 2)
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey75")
        if (ncol(all.inds.mats[[1]]) > 1) {
            for (i in 2:ncol(all.inds.mats[[1]])) {
                lines(all.inds.mats[[1]][, i], col = transp(coli[i], 0.4), lwd = 0.2)
            }
        }
    }
    layout(matrix(1, 1, 1))
    class(all.inds.mats) <- c("fsa_stored")
    cat("\n     Output is a LIST where each element of the list is a DATAFRAME \n            with the channels in columns for the specified file.\n\n")
    return(all.inds.mats)
}
