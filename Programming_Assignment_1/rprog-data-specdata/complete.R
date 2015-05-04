complete <- function(arg1_directory, arg3_id = 1:1) {
        
        files_list <- list.files(arg1_directory,full.names=TRUE)
        
        data_frame <- data.frame()
        nobs <- data.frame()
      
        ##
        index <- 1
        for (i in arg3_id) {
                data_frame <- na.omit(rbind(data_frame,read.csv(files_list[i])))
                nobs [index,1] = i
                nobs [index,2] = nrow(data_frame)
                data_frame = NULL
                index<-index+1
        }
        ##
        
        
        colnames(nobs)[1] <- "id"
        colnames(nobs)[2] <- "nobs"
        nobs1 <- na.omit(nobs)
        rownames(nobs1) <- 1:nrow(nobs1)
        return(nobs1)
}