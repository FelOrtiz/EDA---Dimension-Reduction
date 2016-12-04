#First, we should download the file using csv format and turn it into a dataset.
url_file <- "https://docs.google.com/spreadsheets/d/1RkqOE28fmTQvpD2YCEF0WxK7b9B0WIKW06LZZsQA7VI/export?format=csv&id=1RkqOE28fmTQvpD2YCEF0WxK7b9B0WIKW06LZZsQA7VI&gid=1516859227"
dataset <- read.csv(url(url_file), header = TRUE, sep = ",")


#Then we must remove the unnecessary columns
clean_dataset <- subset(dataset, select = -c(A:D, F, H))


#Function that normalize every value from 0 to 1 using Feature scaling.
normalize <- function(clean_dataset)
{
  new_dataset <- clean_dataset
  
  #Start from 4 to dataset columns number
  for (i in 4:ncol(clean_dataset)) 
  {
    #Get the min of its column
    min <- min(clean_dataset[,i])
    #Get the max of its column
    max <- max(clean_dataset[,i])
    
    for(j in 1:nrow(new_dataset))
    {
      #Get the original value
      original_x <- clean_dataset[j,i]
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
  val <- (value-min) / (max-min)
  return(val)
}


#Now we normalize the values within a range from 0 to 1
norm_dataset <- normalize(clean_dataset)


#If you don't have FSelector library uncomment the following line
#install.packages("FSelector")
library(FSelector)


#----Feature Ranking Algorithms

#--Chi-squared Filter

#Prediction over green columns
we <- chi.squared(E~., subset(norm_dataset, select = -c(G,I)))
wg <- chi.squared(G~., subset(norm_dataset, select = -c(E,I)))
wi <- chi.squared(I~., subset(norm_dataset, select = -c(E,G)))

#Get 10 best predictions
subset_we <- cutoff.k(we, 10)
subset_wg <- cutoff.k(wg, 10)
subset_wi <- cutoff.k(wi, 10)

f_we <- as.simple.formula(subset_we, "E")
print(f_we)
f_we <- as.simple.formula(subset_wg, "G")
print(f_wg)
f_we <- as.simple.formula(subset_wi, "I")
print(f_wi)


#Correlation Filter
#linear.correlation()

#Entropy-Based Filter
#information.gain()


#symmetrical.uncertainty()

#Random Forest Filter
#random.forest.importance(formula, data, importance.type = 1)

#----Feature Subset Selection Algorithms



