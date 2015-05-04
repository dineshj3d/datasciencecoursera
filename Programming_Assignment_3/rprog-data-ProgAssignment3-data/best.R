best <- function(arg1_state, arg2_outcome = "xx") {
       ## assume argument are valid
        
        
        
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
                
        ##
        ## Step 1 - validate outcome arg
        ##          assume arg2 is valid
        arg2_outcome_valid <- TRUE
        
        if (arg2_outcome == 'heart attack') 
                {print (arg2_outcome)}
        else
                if (arg2_outcome == 'heart failure') 
                        {print (arg2_outcome)}
                else
                
                        if (arg2_outcome == 'pneumonia') 
                                {print (arg2_outcome)}
                        else
                                 stop ('invalid outcome')
        ##
        ## Step 2 - validate State arg
        ##
        outcomeallDfr <- read.csv(file="C:/Users/Dinesh/Documents/GitHub/datasciencecoursera/Programming_Assignment_3/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv"
                                  , header=TRUE, sep=",",stringsAsFactors=FALSE)
       
        
        ## get list of all States in data set
        states_listDfr <- unique(subset(outcomeallDfr , select=c("State")))
        ## check to see if state in list
        ##          assume arg2 is valid
        arg1_state_valid <- TRUE
        
        state_in_list <-  (states_listDfr$State == arg1_state)
        
        state_in_list_found <- any(state_in_list)
        
        if (state_in_list_found == TRUE)
              print (arg1_state) 
        else 
                stop ('invalid state!')
        
        ## Step 3 - main processing. assume arguments are valid
        ## create data frame a given State e.g "NY" with associated outcomes
        outcomesubDfr <- subset(outcomeallDfr,State == arg1_state, select = c(2,7,11,17,23))
        ## rename data frame columns
        names(outcomesubDfr) <- c("Hospital", "State", "HeartAttack", "HeartFailure", "Pneumonia")
        
        if (arg1_state_valid == TRUE & arg2_outcome_valid == TRUE) {
                ##print (arg1_state_valid)
                ##print (arg2_outcome_valid)
                ##head_outcomesubDfr <- head(outcomesubDfr)
                ##print (head_outcomesubDfr)
                if (arg2_outcome == 'heart attack') {
                        print (arg2_outcome)
                        outcomesubNADfr <- outcomesubDfr[-grep("Not Available",outcomesubDfr$HeartAttack),]
                        print (outcomesubNADfr$HeartAttack)
                        print (min(outcomesubNADfr$HeartAttack))
                        print (which.min(outcomesubNADfr$HeartAttack))
                        ##print (class(outcomesubNADfr))
                        return(outcomesubNADfr[which.min(outcomesubNADfr$HeartAttack),1])
                        
                }
                else
                        if (arg2_outcome == 'heart failure') {
                                print (arg2_outcome)
                                outcomesubNADfr <- outcomesubDfr[-grep("Not Available",outcomesubDfr$HeartFailure),]
                                print (outcomesubNADfr$HeartFailure)
                                print (min(outcomesubNADfr$HeartFailure))
                                print (which.min(outcomesubNADfr$HeartFailure))
                                ##print (class(outcomesubNADfr))
                                return(outcomesubNADfr[which.min(outcomesubNADfr$HeartFailure),1])
                        }
                else
                        
                        if (arg2_outcome == 'pneumonia') {
                                print (arg2_outcome)
                                outcomesubNADfr <- outcomesubDfr[-grep("Not Available",outcomesubDfr$Pneumonia),]
                                print (outcomesubNADfr$Pneumonia)
                                print (min(outcomesubNADfr$Pneumonia))
                                print (which.min(outcomesubNADfr$Pneumonia))
                                ##print (class(outcomesubNADfr))
                                return(outcomesubNADfr[which.min(outcomesubNADfr$Pneumonia),1])
                        }
                else
                        stop ('invalid outcome')
                
        }
            
}