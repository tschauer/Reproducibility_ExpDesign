





######################################################## setupDDS ######################################################## 


library(DESeq2)


setupDDS <- function(SampleTableName = "SampleTable",
                     CountTableName = "dmel.counts_genes",
                     SampleIdName = "SampleID",
                     ConditionsName = "Conditions",
                     BatchName = "Replicate",
                     n_samples_for_filtering = 3
){
        
        SampleTable <- get(SampleTableName)
        
        my_counts_genes <- get(CountTableName)
        
        stopifnot(
                identical(colnames(my_counts_genes), as.character(SampleTable[,SampleIdName]))
        )
        
        
        
        ######################################################## 
        
        
        
        filter <- apply(my_counts_genes, 1, function(x) length(x[x>1]) >= n_samples_for_filtering)
        
        my_counts_filtered <- my_counts_genes[filter,]
        
        
        
        ######################################################## 
        
        if(is.null(BatchName)){
                
                my_colData <- DataFrame(Sample = SampleTable[,ConditionsName])
                
                rownames(my_colData) <- SampleTable[,SampleIdName]
                
                
                ######################################################## 
                
                dds <- DESeqDataSetFromMatrix(countData = my_counts_filtered, 
                                              colData = my_colData, 
                                              design = ~Sample)
                
        } else {
                
                my_colData <- DataFrame(Sample = SampleTable[,ConditionsName],
                                        Batch = factor(SampleTable[,BatchName]))
                
                rownames(my_colData) <- SampleTable[,SampleIdName]
                
                
                ######################################################## 
                
                dds <- DESeqDataSetFromMatrix(countData = my_counts_filtered, 
                                              colData = my_colData, 
                                              design = ~Batch+Sample)
      
        }
        
        dds <- DESeq(dds)
        
        return(dds)
}



######################################################## setupDDS ######################################################## 
