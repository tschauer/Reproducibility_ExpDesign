




######################################################## make Count Table ######################################################## 



makeCountTable <- function(count_files, count_file_path, stranded = FALSE){
        
        if(stranded){
                cidx <- 4
        } else {
                cidx <- 2
        }
        
        for(i in seq_along(count_files)){
                
                tmp <- read.table(count_file_path[i])
                
                if(i == 1){
                        my_counts <- tmp[,cidx] 
                } else {
                        my_counts <- cbind(my_counts, tmp[,cidx])
                }
        }
        
        rownames(my_counts) <- tmp[,1]
        colnames(my_counts) <- gsub("_[G,A,T,C].*|.ReadsPerGene.out.tab","", count_files)
        
        return(my_counts)
}


######################################################## make Count Table ######################################################## 


