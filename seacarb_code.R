library(seacarb)
library(writexl)

# Read in your data
chemistry_multiple <- read.csv("chemistry_multiple_mergedreps_with_pH_insitu.csv") 


# Calculate the carbonate system in your chemistry_multiple. 
# See ?carb for more information on which flag to use and how to put in your variables.
carb_system <- carb(8, chemistry_multiple$pHt, chemistry_multiple$TA/1000000., S=38, 
                    T=chemistry_multiple$Temp,
               Patm=1, P=0, Pt=0, Sit=0,
               k1k2="x", kf="x", ks="d", pHscale="T", b="u74", gas="potential", 
               warn="y", eos="eos80", long=1.e20, lat=1.e20)



# Merge the carbonate system into your chemistry_multiple
chemistry_multiple <- cbind(chemistry_multiple, carb_system)


# Write chemistry_multiple to .csv
#write.csv(chemistry_multiple, "chemistry_multiple_mergedreps.csv")




# Read in your data
single_data <- read.csv("single_data_with_pH_insitu.csv") 


# Calculate the carbonate system in your single_data. 
# See ?carb for more information on which flag to use and how to put in your variables.
carb_system <- carb(8, single_data$mean_pHts, single_data$meanALK/1000000., S=38, 
                    T=single_data$temp,
                    Patm=1, P=0, Pt=0, Sit=0,
                    k1k2="x", kf="x", ks="d", pHscale="T", b="u74", gas="potential", 
                    warn="y", eos="eos80", long=1.e20, lat=1.e20)



# Merge the carbonate system into your single_data
single_data <- cbind(single_data, carb_system)

# Write single_data to .csv
#write.csv(single_data, "single_data_with_pH_insitu.csv")
