rankhospital <- function(arg1_state, arg2_outcome, arg3_num = "best") {
    print ("Rankhospital - ***ENTER***")    
    
    ## assume argument are valid
        
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        ##
        ## Step 1 - validate outcome arg
        ##          assume arg2 is valid
        arg2_outcome_valid <- TRUE
        
        outcome_data_column <- 0
        
        if (arg2_outcome == 'heart attack') {
                print (arg2_outcome)
                outcome_data_column <- 11
        }
        else
                if (arg2_outcome == 'heart failure') {
                        print (arg2_outcome)
                        outcome_data_column <- 17
                }
                else
                
                        if (arg2_outcome == 'pneumonia') {
                                print (arg2_outcome)
                                outcome_data_column <- 23
                        }
                        else
                                stop ('invalid outcome')
        
        ##
        ## Step 2 - validate state arg
        ##
        outcomeallDfr <- read.csv(file="C:/Users/Dinesh/Documents/GitHub/datasciencecoursera/Programming_Assignment_3/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv"
                                  , header=TRUE, sep=",",stringsAsFactors=FALSE)
        
        
        ## get list of all states in data set
        states_listDfr <- unique(subset(outcomeallDfr , select=c("State")))
        ## check to see if state in list
        ##          assume arg2 is valid
        arg1_state_valid <- TRUE
        
        state_in_list <-  (states_listDfr$State == arg1_state)
        
        state_in_list_found <- any(state_in_list)
        
        if (state_in_list_found == TRUE) {
            
            print (arg1_state)
            
        }
                 
        else 
                stop ('invalid state!')
        
        ## Step 3 - main processing. assume arguments are valid
        ## create data frame a given state e.g "NY" with associated outcomes
        
        if (arg2_outcome == 'heart attack') {
                print (arg2_outcome)
                print (outcome_data_column)
                outcomesubDfr <- subset(outcomeallDfr,select = c(2,7,outcome_data_column))
                ## rename data frame columns
                names(outcomesubDfr) <- c("hospital", "state", "heartattack")
                outcomesubNADfr <- outcomesubDfr[-grep("Not Available",outcomesubDfr$heartattack),]
                ## sort 
                
                outcomesubNADfr$heartattack <- as.numeric(as.character(outcomesubNADfr$heartattack))
                
                outcomeresultDfr <- subset(outcomesubNADfr,state == arg1_state)
                
                outcomeresultDfr <- outcomeresultDfr[order(outcomeresultDfr$heartattack, decreasing=FALSE, outcomeresultDfr$hospital),]
                
                print (head(outcomeresultDfr))
                print (tail(outcomeresultDfr))
                
        }
        else
                if (arg2_outcome == 'heart failure') {
                        print (arg2_outcome)
                        print (outcome_data_column)
                        outcomesubDfr <- subset(outcomeallDfr,select = c(2,7,outcome_data_column))
                        ## rename data frame columns
                        names(outcomesubDfr) <- c("hospital", "state", "heartfailure")
                        outcomesubNADfr <- outcomesubDfr[-grep("Not Available",outcomesubDfr$heartfailure),]
                        
                        ## sort 
                        
                        outcomesubNADfr$heartfailure <- as.numeric(as.character(outcomesubNADfr$heartfailure))
                        
                        outcomeresultDfr <- subset(outcomesubNADfr,state == arg1_state)
                        
                        outcomeresultDfr <- outcomeresultDfr[order(outcomeresultDfr$heartfailure, decreasing=FALSE, outcomeresultDfr$hospital),]
                        
                        print (head(outcomeresultDfr))
                        print (tail(outcomeresultDfr))
                }
                else
                        if (arg2_outcome == 'pneumonia') {
                        print (arg2_outcome)
                        print (outcome_data_column)
                        outcomesubDfr <- subset(outcomeallDfr,select = c(2,7,outcome_data_column))
                        ## rename data frame columns
                        
                        print (head(outcomesubDfr))
                        print (tail(outcomesubDfr))
                        
                        names(outcomesubDfr) <- c("hospital", "state", "pneumonia")
                        
                        outcomesubNADfr <- outcomesubDfr[-grep("Not Available",outcomesubDfr$pneumonia),]
                
                        
                        print (head(outcomesubNADfr))
                        print (tail(outcomesubNADfr))
                        
                        ## sort 
                        
                        outcomesubNADfr$pneumonia <- as.numeric(as.character(outcomesubNADfr$pneumonia))
                        
                        outcomeresultDfr <- subset(outcomesubNADfr,state == arg1_state)
                                            
                        outcomeresultDfr <- outcomeresultDfr[order(outcomeresultDfr$pneumonia, decreasing=FALSE, outcomeresultDfr$hospital),]
                        
                        print (head(outcomeresultDfr))
                        print (tail(outcomeresultDfr))
                        
                        
                        }
                        else
                                stop ('invalid outcome')
        
        
        if (arg3_num == "best") {
            
            rankhospitaloutcome <- outcomeresultDfr[1,1]
            
        }
        else if (arg3_num == "worst") {
            
            rankhospitaloutcome <- outcomeresultDfr[nrow(outcomeresultDfr),1]
            
        }
            else if ((nrow(outcomeresultDfr) + 1) > arg3_num) { 
                
                  rankhospitaloutcome <- outcomeresultDfr[arg3_num,1]
                }
                else rankhospitaloutcome <- NA
                
    print ("Rankhospital - ***EXIT***")             
    
    return(rankhospitaloutcome)
    
    
    ###
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
}