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

#write.csv(norm_dataset, "C:/Users/HoLeX/Desktop/norm_dataset.csv", row.names=F)
#write.table(norm_dataset, "C:/Users/HoLeX/Desktop/norm_dataset.txt", sep="\t")


#If you don't have FSelector library uncomment the following line
#install.packages("FSelector")
library(FSelector)


execute <- function(class, letter, subset, picks)
{

#----Feature Ranking Algorithms

#--Chi-squared Filter

chi <- chi.squared(class, subset)
subset_chi <- cutoff.k(chi, picks)
out_chi <- as.simple.formula(subset_chi, letter)
cat("Chi-squared Filter\n")
print(out_chi)

#--Correlation Filter

linear <- linear.correlation(class, subset)
subset_linear <- cutoff.k(linear, picks)
out_linear <- as.simple.formula(subset_linear, letter)
cat("Correlation Filter - Pearson’s correlation\n")
print(out_linear)

#--

rank <- rank.correlation(class, subset)
subset_rank <- cutoff.k(rank, picks)
out_rank <- as.simple.formula(subset_rank, letter)
cat("Correlation Filter - Spearman’s correlation\n")
print(out_rank)

#--Entropy-Based Filter

inf_gain <- information.gain(class, subset, unit = "log2")
subset_inf_gain <- cutoff.k(inf_gain, picks)
out_inf_gain <- as.simple.formula(subset_inf_gain, letter)
cat("Entropy-Based Filter - Information Gain\n")
print(out_inf_gain)

#--

gain_ratio <- gain.ratio(class, subset, unit = "log2")
subset_gain_ratio <- cutoff.k(gain_ratio, picks)
out_gain_ratio <- as.simple.formula(subset_gain_ratio, letter)
cat("Entropy-Based Filter - Gain Ratio\n")
print(out_gain_ratio)

#--

sym_unc <- symmetrical.uncertainty(class, subset, unit = "log2")
subset_sym_unc <- cutoff.k(sym_unc, picks)
out_sym_unc <- as.simple.formula(subset_sym_unc, letter)
cat("Entropy-Based Filter - Symmetrical Uncertainty\n")
print(out_sym_unc)

#--One R Algorith

one_r <- oneR(class, subset)
subset_one_r <- cutoff.k(one_r, picks)
out_one_r <- as.simple.formula(subset_one_r, letter)
cat("One R Algorith\n")
print(out_one_r)

}

num_values <- 10
subset_E <- subset(norm_dataset, select = -c(G,I))
subset_G <- subset(norm_dataset, select = -c(E,I))
subset_I <- subset(norm_dataset, select = -c(E,G))

out <- capture.output(execute(E~., "E", subset_E, num_values))
cat("Output", out, file="C:/Users/HoLeX/Desktop/E.txt", sep="\n", append=TRUE)

out <- capture.output(execute(G~., "G", subset_G, num_values))
cat("Output", out, file="C:/Users/HoLeX/Desktop/G.txt", sep="\n", append=TRUE)

out <- capture.output(execute(I~., "I", subset_I, num_values))
cat("Output", out, file="C:/Users/HoLeX/Desktop/I.txt", sep="\n", append=TRUE)




