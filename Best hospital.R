best <- function(state, outcome){
        data <- read.csv("outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)
        outcomelist <- c("heart attack", "heart failure", "pneumonia")
        if (!(state %in% unique(data$State))) stop("Please reenter a vaild state")
        if (!(outcome %in% outcomelist)) stop("Please reenter a vaild outcome")
        
        teststate <- data[data$State == state, ]
        names(teststate)[c(11,17,23)] <- outcomelist
        answer <- teststate[teststate[, outcome] == min(teststate[, outcome], na.rm = TRUE), ][2]
        result <- answer[with(answer, order(Hospital.Name)), ]
        result[1]
        
}