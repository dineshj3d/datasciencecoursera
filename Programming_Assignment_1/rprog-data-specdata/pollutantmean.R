pollutantmean <- function(arg1_directory, arg2_pollutant, arg3_id = 1:332) {
        
        files_list <- list.files(arg1_directory,full.names=TRUE)
        
        ## 'arg1_directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        data_frame <- data.frame()
        ##
        for (i in arg3_id) {
                data_frame <- rbind(data_frame,read.csv(files_list[i]))
        }
        ##
        
        data_frame_head <- head(data_frame)
        data_frame_tail <- tail(data_frame)
        data_frame_dim <- dim(data_frame)
        data_frame_column_names <- names(data_frame)
        
        ##print(data_frame_head)
        ##print(data_frame_tail)
        ##print(data_frame_dim)
        ##print(data_frame_column_names)
        
        ##print (arg1_directory)
        ##print (arg2_pollutant)
        ##print(arg3_id)
        ##print(files_list[arg3_id])
        
        arg_out_mean <- 9999
        
        if (arg2_pollutant == 'sulfate') {
                arg_out_mean = mean(data_frame$sulfate, na.rm=TRUE)
        }
        else
                
                if (arg2_pollutant == 'nitrate') {
                        arg_out_mean = mean(data_frame$nitrate, na.rm=TRUE)
                }
        else
                
                print(arg_out_mean)
        
        return(round(arg_out_mean,3))
        
        ## 'arg2_pollutant' is a character vector of length 1 indicating
        ## the name of the arg2_pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'arg3_id' is an integer vector indicating the monitor arg3_id numbers
        ## to be used
        
        ## Return the mean of the arg2_pollutant across all monitors list
        ## in the 'arg3_id' vector (ignoring NA values)
}