library(seacarb)
library(writexl)

# Read in your data
dataset <- read.csv("chemistry_multiple_mergedreps_with_pH_insitu.csv") 
# Read Chemistry_multiple.xlsx


# Calculate the carbonate system in your dataset. 
# See ?carb for more information on which flag to use and how to put in your variables.
carb_system <- carb(8, dataset$pHt, dataset$TA/1000000., S=38, 
                    T=dataset$Temp,
               Patm=1, P=0, Pt=0, Sit=0,
               k1k2="x", kf="x", ks="d", pHscale="T", b="u74", gas="potential", 
               warn="y", eos="eos80", long=1.e20, lat=1.e20)



# Merge the carbonate system into your dataset
dataset <- cbind(dataset, carb_system)


# Write dataset to Excel
write_xlsx(dataset, "Chemistry_multiple.xlsx")



