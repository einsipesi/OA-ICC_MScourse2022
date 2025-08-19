
library(readxl)
library(ggplot2)
stressindex_data <- read_excel("stressindex_data.xlsx", 
                               col_types = c("text", "text", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric"))
#View(stressindex_data)


# Colorblind-friendly palette
driver_palette <- c(
  "temperature" = "#E69F00",     # orange
  "pH" = "#56B4E9",       # sky blue
  "lithium" = "#009E73",       # bluish green
  "multi-driver" = "#F0E442"        # yellow
)


# Create a scatter plot of stress index vs gr with points filled by treatment
png("stressindex.png", width = 8, height = 6, units = "in", res = 300)
ggplot(stressindex_data, aes(x = stress_index, y = gr, fill = Treatment)) +
       geom_point(shape = 21, size = 4, color = "black") +
       scale_fill_manual(values = driver_palette) +
        xlab(expression(paste("Stress Index")))+
        ylab(expression(paste("Growth rate ("*mu*"m"%.%"day"^-1*")")))+
       #labs(x = "Stress Index", y = "Growth rate ("*mu*"m"%.%"day"^-1*")", fill = "Treatment") +
       theme_minimal(base_size = 14)+
        theme(
          axis.text       = element_text(size = 13),
          axis.title.y    = element_text(size = 16),
          legend.text     = element_text(size = 14),
          legend.title    = element_text(size = 16),
          plot.title      = element_text(size = 16, face = "bold")
       )
dev.off()



