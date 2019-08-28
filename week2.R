#1st problem: mean by pollutant of the chosen idx
#it seems that I had to get all values bevore mean it by files

pollutantmean <- function(directory, pollutant, id = 1:332) {
  #getting the file names from directory
  files <- list.files(directory, full.names = TRUE)
  #filtering out the id files
  files_filtered <- files[id]
  print(files_filtered)

  values <- c()
  #reading a csv file to data.frame
  for (ind in 1:length(id)) {
    df <- read.csv(files_filtered[ind])
    #filtering the pollutant column
    v <- df[[pollutant]]
    #skipping the NA values
    v <- v[!is.na(v)]
    #adding all values to a vector
    values <- c(values, v)
  }
  
  #computing mean at the end
  all_mean <- mean(values)
}

res <- pollutantmean("specdata", "sulfate", 1:10)
print(res)


#2nd how many full observation rows are present per file

complete <- function(directory, id = 1:332) {
  #getting the file names
  files <- list.files(directory, full.names = TRUE)
  #filtering out the id files
  files_filtered <- files[id]
  
  comp = rep(0, length(id))
  for (ind in 1:length(id)) { #for every file
    df <- read.csv(files_filtered[ind]) #read csv to dataframe
    comp[ind] <- nrow(na.omit(df)) #count the rows, exclude the na s
    
  }
  y <- data.frame(id, comp) #creating a dataframe from ids and values
}

res <- complete("specdata", 3)
print(res)

#3rd calculate the correlation between sulfate and nitrate per file, but with
#full observations above a thershold

corr <- function(directory, threshold = 0) {
  #getting the file names
  files <- list.files(directory, full.names = TRUE)
  
  correl <- NULL
  for (f in files) {
    df <- read.csv(f)
    df <- df[complete.cases(df),] #if there is no na
    if (nrow(df) > threshold) {
      correl <- c(correl, cor(df[,"nitrate"], df[,"sulfate"])) #correl for every file
    }
  }
  
  return(correl)

}

res <- corr("specdata")
head(res, 6)
summary(res)
length(res)
