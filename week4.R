#1. assignment
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11]) #use as numeric values
hist(outcome[, 11])

#2nd assignment
states <- unique(outcome[, 7])
print(states[2])
library(data.table)

best <- function(state, outcome) {
  #read the data
  data1 <- fread("outcome-of-care-measures.csv", sep=",")
  data <- as.data.table(data1)
  
  #checking parameters
  if (is.element(state, states) && is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
    
    #getting the correct index
    switch(outcome,
           "heart attack" = {
             idx <- 11
           },
           "heart failure" = {
             idx <- 17
           },
           "pneumonia" = {
             idx <- 23
           }
           )
    
    hospitals_in_state <- data[state == "TX"] #getting every matching rows
    ordered_hospitals <- hospitals_in_state[ order( hospitals_in_state[, idx], hospitals_in_state[, 2]), ]
    
    print(nrow(hospitals_in_state))
  }
}

best("TX", "heart attack")
