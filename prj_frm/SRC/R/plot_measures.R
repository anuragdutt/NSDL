##This R programme consists of functions to plot various market
## measures. Following functions will be called once the data has been
## extracted using extractData.R

############
## Prices ##
############

plotIndex <- function(mdata,
                      y.lim,
                       increment,
                       filename,
                       round.vec,
                       mai.vec,
                       mgp.vec,
                       mtext.line,
                       legend.vec,
                       col.vec,
                       lty.vec,
                       lwd.vec,
                       ncol.vec,
                       cex.vec
                       ){

    max.val <- round(max(c(mdata$index),
                         na.rm = TRUE), round.vec)
    
    if(max.val < y.lim) {
        max.val <- y.lim
    }
                                        # y-labels
    ylabelaxis <- seq(round(min(c(mdata$index),
                                na.rm = TRUE), round.vec),
                      max.val,
                      by = increment)
    ylabpoints <- as.character(ylabelaxis)
                                        # x-labels
    datelist <- mdata$date[seq(from = 2, to = length(mdata$date),
                               by = 2)]
    indexTime <- which(mdata$date %in% datelist)
    datelist <- datelist[-length(datelist)]
    datelist <- c(as.character(datelist), "Today")

                                        # Plot
    png(filename, width = 3840, height = 2160)
    layout(matrix(c(1,2)), heights = c(9, 2.5))
    par(mai = mai.vec)
    par(mgp = mgp.vec)
    
    yhilo <- range(c(mdata$index, max.val), na.rm = TRUE)
    plot(y = mdata$index,
         x = 1:nrow(mdata),
         t = "s", xaxt = "n", xlab = "", lty = 1, yaxt = "n",
         lwd = 9.5, col = "darkred", ylab = "Financial reforms index", cex.lab = 8.5,
         cex.axis = 8.5, ylim = yhilo,
         las = 1, xaxs = "i", verticals = TRUE)
    axis(1, at = indexTime, labels = FALSE, lwd.ticks = 5, tck = -0.015)
    axis(2, at = ylabelaxis, labels = FALSE, lwd.ticks = 5, tck = -0.015)
    abline(v = indexTime, lty = 2, col = "darkgray", lwd = 4)
    abline(h = ylabpoints, lty = 2, col = "darkgray", lwd = 4)
    mtext(text = datelist, side = 1, line = 5.5, at = indexTime, cex = 8.5)
    mtext(text = ylabpoints, side = 2, line = mtext.line, at = ylabelaxis,
          cex = 7, adj = 0, las = 1)
    box(lty = "solid")
    plot.new()
    par(mar = c(0, 4, 0, 0))
    legend(x = "center",
           legend = legend.vec,
           bty = "n",
           col = col.vec,
           lty = lty.vec,
           lwd = lwd.vec,
           ncol = ncol.vec, 
           cex = cex.vec,
           y.intersp = 2
           )
  dev.off()

}


plotIndexDaily <- function(mdata,
                           y.lim,
                           increment,
                           filename,
                           round.vec,
                           mai.vec,
                           mgp.vec,
                           mtext.line,
                           legend.vec,
                           col.vec,
                           lty.vec,
                           lwd.vec,
                           ncol.vec,
                           cex.vec,
                           y.jump
                           ){

    max.val <- round(max(c(mdata$index),
                         na.rm = TRUE), round.vec)
    
    if(max.val < y.lim) {
        max.val <- y.lim
    }
                                        # y-labels
    ylabelaxis <- seq(round(min(c(mdata$index),
                                na.rm = TRUE), round.vec),
                      max.val,
                      by = increment)
    ylabpoints <- as.character(ylabelaxis)

                                        # x-labels
    end.dt <- tail(mdata$date, 1)
    datelist <- mdata$date[!duplicated(format(as.Date(mdata$date),
                                              "%Y"), fromLast = FALSE)]
    datelist <- datelist[-1]
    datelist <- c(datelist, end.dt)
    datelist <- datelist[seq(from = 1+y.jump,
                             to = length(datelist),
                             by = y.jump+1)]
    
    indexTime <- which(mdata$date %in% datelist)
    datelist  <- format(as.Date(datelist), "%Y")
    datelist <- datelist[-length(datelist)]
    datelist <- c(as.character(datelist), "Today")
    
                                        # Plot
    png(filename, width = 3840, height = 2160)
    layout(matrix(c(1,2)), heights = c(9, 2.5))
    par(mai = mai.vec)
    par(mgp = mgp.vec)
    
    yhilo <- range(c(mdata$index, max.val), na.rm = TRUE)
    plot(y = mdata$index,
         x = 1:nrow(mdata),
         t = "s", xaxt = "n", xlab = "", lty = 1, yaxt = "n",
         lwd = 8.5, col = "darkred", ylab = "Financial reforms index", cex.lab = 8.5,
         cex.axis = 8.5, ylim = yhilo,
         las = 1, xaxs = "i", verticals = TRUE)
    axis(1, at = indexTime, labels = FALSE, lwd.ticks = 5, tck = -0.015)
    axis(2, at = ylabelaxis, labels = FALSE, lwd.ticks = 5, tck = -0.015)
    abline(v = indexTime, lty = 2, col = "darkgray", lwd = 4)
    abline(h = ylabpoints, lty = 2, col = "darkgray", lwd = 4)
    mtext(text = datelist, side = 1, line = 5.5, at = indexTime, cex = 8.5)
    mtext(text = ylabpoints, side = 2, line = mtext.line, at = ylabelaxis,
          cex = 7, adj = 0, las = 1)
    box(lty = "solid")
    plot.new()
    par(mar = c(0, 4, 0, 0))
    legend(x = "center",
           legend = legend.vec,
           bty = "n",
           col = col.vec,
           lty = lty.vec,
           lwd = lwd.vec,
           ncol = ncol.vec, 
           cex = cex.vec,
           y.intersp = 2
           )
  dev.off()

}



