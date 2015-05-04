rankall <- function(arg1_outcome, arg2_num = "best") {
    
    print (arg1_outcome)
    print (arg2_num)
    
    ## Read outcome data
    ## Check that state and outcome are valid
    
    ## get list of all states in outcome data set
    
    outcomeallDfr <- read.csv(file="C:/Users/Dinesh/Documents/GitHub/datasciencecoursera/Programming_Assignment_3/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", header=TRUE, sep=",",stringsAsFactors=FALSE)
    
    states_listDfr <- data.frame()
    
    states_listDfr <- unique(subset(outcomeallDfr , select=c("State")))
    rownames(states_listDfr) <- NULL
    ## sort by State Name
    ## states_listDfr <- states_listDfr[order(states_listDfr$State),]
    
    print (head(states_listDfr,20))
    print (nrow(states_listDfr))
    
    ## create results data frame    
    rankallresultDfr <- data.frame()
    ## names columns
     
    
    index <- 1
    
    for (i in 1:nrow(states_listDfr) ) {
        
        ##print (rankallresultDfr [index,1])
        ##print (rankallresultDfr [index,2])
        ##print (index)
        
        
        ##rankallresultDfr [index,1] = rankhospital(states_listDfr[index,1], arg1_outcome, arg2_num)
        rankallresultDfr [index,1] = rankhospital(states_listDfr[index,1], arg1_outcome, arg2_num)
        rankallresultDfr [index,2] = states_listDfr$State[index]
        
        print (rankallresultDfr [index,1])
        print (rankallresultDfr [index,2])
        
        print (index)
        
        
        index = index + 1
        
    }
    
    names(rankallresultDfr) <- c("hospital", "state")
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    
    return (rankallresultDfr[order(rankallresultDfr$state),])
}