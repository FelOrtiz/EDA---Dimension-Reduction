#First, we should download the file using csv format and turn it into a dataset.
url_file <- "https://docs.google.com/spreadsheets/d/1RkqOE28fmTQvpD2YCEF0WxK7b9B0WIKW06LZZsQA7VI/export?format=csv&id=1RkqOE28fmTQvpD2YCEF0WxK7b9B0WIKW06LZZsQA7VI&gid=1516859227"
dataset <- read.csv(url(url_file), header = TRUE, sep = ",")


#Then we must remove the unnecessary columns
clean_dataset <- subset(dataset, select = -c(A:D, F, H))


#Now we normalize the values within a range from 0 to 1
norm_datase <- normalize(clean_dataset)

#Function that normalize every value from 0 to 1 using Feature scaling.
normalize <- function(clean_dataset)
{
  new_dataset <- clean_dataset
  
  #Start from 4 to datasets columns number
  for (i in 4:ncol(new_dataset)) 
  {
    for(j in 1:nrow(new_dataset))
    {
      #Get the original value
      original_x <- clean_dataset[j,i]
      #Get the min of its column
      min <- min(clean_dataset[,i])
      #Get the max of its column
      max <- max(clean_dataset[,i])
      #We rescale it
      new_x <- rescaling(original_x, min, max)
      #Then, we put the rescaled value on original value position
      new_dataset[j,i] <- new_x
    }
  }
  #Finally, return all changes
  return(new_dataset)
}

#Function that implements the formula for the simplest way to rescaling.
rescaling <- function(value, min, max)
{
  val <- (value - min)/(max-min)
  return(val)
}


