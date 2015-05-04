corr <- function(arg1_directory, arg2_threshold = 0) {
        ## Load files list
        
        files_list <- list.files(arg1_directory,full.names=TRUE)
        
        ## use this DF to hold hols complete cases for each montitor
        
        data_frame <- data.frame()
        
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## Call complete function to count complete case within all 332 monitors
        
        corr_result <- numeric(0)
        
        corr_complete_cases <- complete(arg1_directory,1:332)
        
        
        ## identify all monitors that meet the threshold of complete cases
        
        corr_subset_cases <- subset(corr_complete_cases,nobs > arg2_threshold)
        
        ## list all monitors that meet threshold
        ## for (i in 1:nrow(corr_subset_cases)) 
        ##      { print (corr_subset_cases[i,]) }
        ##
        
        for (i in corr_subset_cases$id ) {
                ## get data
                data_frame <- data.frame()
                ## print (head(data_frame))
                data_frame <- na.omit(rbind(data_frame,read.csv(files_list[i])))
                corr_result <- c(corr_result, cor(data_frame$sulfate, data_frame$nitrate, 
                                                  use = "pairwise.complete.obs"))
                ## print (head(data_frame))
                ## print(i)
                ## print(corr_result)
                
                
                ##
        }
        
        
        
        return (round(corr_result,5))
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default 
        
        ## Return a numeric vector of correlations
}