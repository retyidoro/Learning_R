#1. assignment
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11]) #use as numeric values
hist(outcome[, 11])

#2nd assignment: given state, given outcome> the best hospital selection
library(readr)
library(dplyr)

outcomes <- read_csv("outcome-of-care-measures.csv")
states <- sort(unique(outcome$State))
print(states)

best <- function(state, outcome) {
  #read the data done outside of the function
  
  #check the validity of parameters
  if(!state %in% states) stop("Not a valid state!")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("Not a valid outcome")
  
  idx <- 11
  #getting the index of needed column
  if(outcome == "heart failure") {
    idx <- 17
  }
  else {
    if (outcome == "pneumonia") {
      idx <- 23
    }
  }
  print(idx)
  #subsetting the dataframe
  df_small <- outcomes %>% select(c(2, 7, idx)) #state and illness
  colnames(df_small) <- c("Hospital", "State", "Outcome")
  
  df_state <- df_small %>% filter(State == state) #the right state
  #print(df_state)
  df_arr <- df_state %>% arrange(as.double(Outcome), Hospital)
  #print(df_arr)

  df_arr[1, ]
}

res <- best("NY", "hert attack")
print(res)


#3 ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
  #already red data
  
  #check the validity of parameters
  if(!state %in% states) stop("Not a valid state!")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("Not a valid outcome")
  
  idx <- 11
  #getting the index of needed column
  if(outcome == "heart failure") {
    idx <- 17
  }
  else {
    if (outcome == "pneumonia") {
      idx <- 23
    }
  }
  print(idx)
  
  #subsetting the dataframe
  df_small <- outcomes %>% select(c(2, 7, idx)) #state and illness
  colnames(df_small) <- c("Hospital", "State", "Outcome")
  
  df_state <- df_small %>% filter(State == state) #the right state
  
  
  #print(df_state)
  df_state$Outcome <- as.numeric(as.character(df_state$Outcome)) #replace not available with na
  
  df_arr <- df_state %>% arrange(as.double(Outcome), Hospital) #order by col
  
  df_av <- df_arr %>% filter(Outcome != "NA")
  print(df_av)
  
  #print(df_arr)
  print("az utolso")
  print(df_av[nrow(df_av), ])
  
  #check the rank
  if (num == "best") num <- 1
  if (num == "worst") num <- nrow(df_av)
  
  print(df_av[num, ])
}

res <- rankhospital("NM", "heart attack", 5000)
print(res)




#4   ranking hospitals in all states

rankall <- function(outcome, num = "best") {
  #data red
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("Not a valid outcome")
  
  idx <- 11
  #getting the index of needed column
  if(outcome == "heart failure") {
    idx <- 17
  }
  else {
    if (outcome == "pneumonia") {
      idx <- 23
    }
  }
  print(idx)
  print(outcome)
  
  #get the tree columns
  df_small <- outcomes %>% select(c(2, 7, idx))
  colnames(df_small) <- c("Hospital", "State", "Outcome")
  df_small$Outcome <- as.numeric(as.character(df_small$Outcome)) #replace not available with na
  df_small <- df_small %>% filter(Outcome != "NA")
  
  hospitals <- c()
  
  for (state in states) {
    
    df_state <- df_small %>% filter(State == state)
    df_arr <- df_state %>% arrange(as.double(Outcome), Hospital) #order by col
    print("a df hossza")
    print(nrow(df_arr))
    #check the rank
    if (num == "best") { num <- 1 }
    if (num == "worst") { num <- nrow(df_arr) }
    print("a rank szama: ")
    print(num)
    print(df_arr[num, ])
    hospitals <- c(hospitals, df_arr[num, ]$Hospital)
    
  } 
  #print(hospitals)
  df <- data.frame(states, hospitals)
}

res <- rankall("heart failure", 10)
print(res)








