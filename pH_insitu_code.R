# 
# Here you can see how the pH in situ was calculated, and what constants were used. 
# The names of the columns depend on which dataset you are calculating from,  and so does the TA division (have it in is in mol/kg)
# The calculated data was then added as new columns to the existing dataset
# 

library(seacarb)
library(dplyr)
library(readxl)

# Read in you dataframe and name it 'data'
#dataset <- read_csv("Chemistry_multiple.csv")
str(dataset) # Look at column names and change the code below as needed to match names


# Make sure the data does not contain NAs. You can use the following to drop the NAs:
dataset <- na.omit(dataset)  # Drops any rows with NA in any column



# Calculate pH in situ for 
pH_insitu_vector <- pHinsi(
  pH     = dataset$pHt, 
  ALK    = dataset$TA/1000000, # Make sure the alkalinity is in mol/kg
  Tinsi  = dataset$Temp, # Or temp in situ
  Tlab   = dataset$Temp, 
  Pinsi  = 0,       
  S      = 38,      # or use data$salinity if you have it
  Pt     = 0,       
  Sit    = 0,
  k1k2   = "x",
  kf     = "x",
  ks     = "d",
  pHscale= "T",
  b      = "u74",
  eos    = "eos80",
  long   = 1e+20,
  lat    = 1e+20
)

# Add a pH_insitu column to the dataset, with the pH_insitu vector just created
dataset$pH_insitu2<-pH_insitu_vector

# Write out a csv
write.csv(dataset, "chemistry_multiple_mergedreps_with_pH_insitu.csv")