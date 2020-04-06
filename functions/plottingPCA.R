
#####################################################      PCA      ######################################################## 






plottingPCA <- function(my_data, 
                        xcomp = 1,
                        ycomp = 2,
                        color_palette,
                        conditions,
                        quantiles = c(0,1),
                        show_labels = TRUE,
                        point_size = 1.5,
                        my_limits = c(-100,100),
                        my_title = "PCA"){
        
        rv <- rowVars(my_data)
        
        selection <- (rv >  quantile(rv, quantiles[1])  & rv < quantile(rv, quantiles[2]))
        
        
        pca <- prcomp(t(my_data[selection, ]), scale. = TRUE)
        
        percentVar <- round(pca$sdev^2/sum(pca$sdev^2)*100,1)[1:10]
        
        
        
        plot(pca$x[, xcomp], pca$x[, ycomp]*-1, 
             col = color_palette[conditions], 
             pch=16, cex = point_size,
             xlab = paste("PC",xcomp," (", percentVar[xcomp], "%)", sep=""),
             ylab = paste("PC",ycomp," (", percentVar[ycomp], "%)", sep=""),
             xlim= my_limits, ylim=my_limits)
        
        points(pca$x[, xcomp], pca$x[, ycomp]*-1,                
               cex = point_size, col = "#555555", pch=1, lwd=0.5)
        title(main = my_title, line = 0.5)
        
        
        if(show_labels){
                text(pca$x[, xcomp], pca$x[, ycomp]*-1, labels = rownames(pca$x), 
                     adj = -0.5, col = "gray32", cex=0.5)      
        }
        
        
        
}

#####################################################      PCA      ######################################################## 

