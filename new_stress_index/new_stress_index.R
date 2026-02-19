
#Remember to fix wd to relative path
setwd("~/Library/CloudStorage/OneDrive-PublicAdministration/Documents/surt/BOAT/monaco_multiple_drivers/scripts_data/rproject_final/OA-ICC_MScourse2022")
      
new_stress <- read_excel("new_stress_index/new_stress.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "text"))
attach(new_stress)
library(ggplot2)
library(dplyr)


# Colorblind-friendly palette
driver_palette <- c(
  "temperature" = "#E69F00",     # orange
  "pH" = "#56B4E9",       # sky blue
  "lithium" = "#009E73",       # bluish green
  "multi-driver" = "#F0E442"        # yellow
)


ggplot(new_stress, aes(SIca, gr)) + geom_point(aes(color=Treatment)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, alpha = 0.3, aes(color=Treatment)) + scale_color_manual(values = driver_palette)

#new_stress$SIca_adjusted <- c(SItemp+SIpH*28+SILi*0.35)

ggplot(new_stress, aes(SIca_adjusted, gr))+geom_point(aes(color=Treatment)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, alpha = 0.3, aes(color=Treatment), fullrange=TRUE) + scale_color_manual(values = driver_palette)

ggplot(new_stress, aes(SIca_adjusted, gr, shape = experiment))+geom_point(aes(color=multi_out_of_reference)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, alpha = 0.3, aes(color=experiment), fullrange=TRUE)+
  scale_shape_manual(values = c(single = 4, multi  = 16))





