library(dplyr)

df <- Point_B_01_2025.11.06

df$Sampling.date <- as.Date(df$Sampling.date)

# Filter using base R format()
df_ond <- df[format(df$Sampling.date, "%m") %in% c("10", "11"), ]

# Run summary on just the two columns of interest
summary(df_ond)
