### Title:    Create Imputation Diagnostic Plots
### Author:   Kyle M. Lang
### Created:  2015-OCT-02
### Modified: 2015-OCT-13


plotImps <- function(impList, targetVar = NULL, type = "scatter")
{
    nImps <- length(impList)
    rMat <- impList[[1]] != impList[[2]] |
        impList[[2]] != impList[[3]] |
            impList[[1]] != impList[[3]]
    
    missData <- impList[[1]]
    missData[rMat] <- NA
    
    if(is.null(targetVar)) {
        indexVec <- which(colSums(rMat) > 0)
    } else if(is.character(targetVar)) {
        indexVec <- which(colnames(missData) %in% targetVar)
    } else {
        indexVec <- targetVar
    }
    
    for(i in indexVec) {
        target <- missData[ , i]
        targetName <- colnames(missData)[i]

        if(any(class(target) == "factor")) {
            targetLevels <- levels(impList[[1]][ , i])
            imps <- lapply(impList,
                           FUN = function(x, rMat, index) {
                               as.numeric( factor(x[rMat[ , index], index]) )
                           },
                           rMat = rMat,
                           index = i)
        } else {
            imps <- lapply(impList,
                           FUN = function(x, rMat, index) {
                               x[rMat[ , index], index]
                           },
                           rMat = rMat,
                           index = i)
        }

        if(type == "scatter") {
            yRange <- range(c(as.numeric(target), imps), na.rm = TRUE)

            plot(NULL,
                 main =
                     paste0("Imputed (Red) vs. Observed (Black) Values\nfor ",
                            targetName),
                 ylab = targetName,
                 xlab = "Observation",
                 ylim = yRange,
                 xlim = c(1, length(target)),
                 yaxt = "n")

            if(any(class(target) == "factor")) {
                axisVals <- sort(
                    unique(
                        as.numeric( target[!is.na(target)] )
                    )
                )
                axisLabs <- targetLevels
            } else {
                lb <- min(unlist(imps), na.rm = TRUE)
                ub <- max(unlist(imps), na.rm = TRUE)
                axisVals <- round(
                    seq(lb, ub, (ub - lb) / 5)
                )
                axisLabs <- axisVals
            }
            axis(side = 2,
                 at = axisVals,
                 labels = axisLabs)
            
            tmp <- rep(NA, length(target))
            for(m in 1 : nImps) {
                tmp[is.na(target)] <- imps[[m]]
                points(as.numeric(tmp), col = "red")
            }
            
            points(target, pch = 19)
        } else if(type == "density") {
            if(any(class(target) == "factor")) {
                tmp <- table(as.numeric(target))
                tmp2 <- lapply(imps, table)
                tmp3 <- lapply(tmp2,
                               FUN = function(x, levs) {
                                   tmpVec <- rep(NA, length(levs))
                                   tmpVec[levs %in% names(x)] <- x
                                   tmpVec
                               },
                               levs = names(tmp)
                               )
                tmp4 <- do.call("rbind", tmp3)
                
                yMax <- max(tmp, tmp4, na.rm = TRUE)
                
                barplot(tmp4,
                        beside = TRUE,
                        col = rgb(1, 0, 0, alpha = 0.5),
                        border = rgb(1, 0, 0, alpha = 0.5),
                        ylim = c(0, yMax),
                        space = c(0, nImps * 0.1),
                        names.arg = targetLevels,
                        main =
                            paste0("Imputed (Red) vs. Observed (Black) Values\nof ",
                                   targetName),
                        ylab = "Frequency",
                        xlab = paste0("Level of ", targetName)
                        )
                
                barplot(t(tmp),
                        beside = TRUE,
                        add = TRUE,
                        col = "black",
                        border = "black",
                        width = nImps,
                        space = c(0, 0.1),
                        axes = FALSE,
                        axisnames = FALSE)
                
                barplot(tmp4,
                        beside = TRUE,
                        col = rgb(1, 0, 0, alpha = 0.5),
                        border = rgb(1, 0, 0, alpha = 0.5),
                        add = TRUE,
                        space = c(0, nImps * 0.1),
                        axes = FALSE,
                        axisnames = FALSE)
            } else {
                dens1 <- density(target, na.rm = TRUE)
                if(sum(rMat[ , i]) > 1) {
                    dens2 <- lapply(imps, density)
                    dens2RangeY <- range(lapply(dens2, function(x){x$y}))
                    dens2RangeX <- range(lapply(dens2, function(x){x$x}))   
                } else {
                    dens2 <- dens2RangeY <- dens2RangeX <- NA
                }
                dens2.2 <- density(unlist(imps))
                
                yRange <- range(dens1$y,
                                dens2RangeY,
                                dens2.2$y,
                                na.rm = TRUE)
                xRange <- range(dens1$x,
                                dens2RangeX,
                                dens2.2$x,
                                na.rm = TRUE)
                
                plot(NULL,
                     main =
                         paste0("Densities of Imputations (Red) vs.\n",
                                "Observed Data (Black) vs.\n",
                                "Mean Imputation (Blue) for ",
                                targetName),
                     ylab = "Density",
                     xlab = paste0("Value of ", targetName),
                     ylim = yRange,
                     xlim = xRange
                     )
                if(sum(rMat[ , i]) > 1) {
                    for(m in 1 : length(impList)) {
                        lines(dens2[[m]], col = "red")
                    }
                }
                lines(dens1, lwd = 3)
                lines(dens2.2, lwd = 3, col = "blue")
            }
        }
    }
}# END plotImps()


##### Usage #####

## Produce a scatterplot for every imputed variable in the data set:
## plotImps(impList = myImpList)

## Produce a scatterplot for only "var1"
## plotImps(impList = myImpList, targetVar = "var1")

## Produce a density plot for only "var1"
## plotImps(impList = myImpList, targetVar = "var1", type = "density")
