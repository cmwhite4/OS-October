# LDbase.org | Open Science October 2023 | Week 3 | Math Environment
# Christine White

# Dataset: Hart, S. A., Ganley, C., & Purpura, D. (2021). Home Math Environment Study (1–). 

##### Set-up -------------------------------------------------------------------

# Libraries
library(tidyverse)
library(ggimage)
library(extrafont)
library(png)
library(grid)

# Read in data & subset to relevant vars
data <- read.csv("FinalCleanedData.csv", na.strings=c(".")) %>%
  filter(floor(age_yr) == 5) %>%
  select(mathathomeuse15,mathathomeuse27,
         mathathomeuse26, mathathomeuse10)

# Looking at data
data2 <- data.frame(response = 1:6, 
                    n = sapply(data, FUN = table))

# Prepping for plot
data2$Frequency <- recode(data2$response,
                         "1" = "Never", "2" = "Monthly or Less", "3" = "Few Times Per Month",
                         "4" = "1 Time Per Week", "5" = "2-4 Times Per Week",
                         "6" = "Almost Daily")

datalong <- pivot_longer(data2, n.mathathomeuse15:n.mathathomeuse10)

datalong$label <- recode(datalong$name,
                         "n.mathathomeuse15" = "use calendars and dates?", 
                         "n.mathathomeuse27" = "count out money?", 
                         "n.mathathomeuse10" = "measure ingredients?",
                         "n.mathathomeuse26" = "interact with clocks?")


##### Plot ---------------------------------------------------------------------

# Reading in image PNGs 
clock <- readPNG("clock.png")
g1 <- rasterGrob(clock, interpolate=TRUE)

cal <- readPNG("cal.png")
g2 <- rasterGrob(cal, interpolate=TRUE)

cup <- readPNG("cup.png")
g3 <- rasterGrob(cup, interpolate=TRUE)

money <- readPNG("money.png")
g4 <- rasterGrob(money, interpolate=TRUE)

# make sure in correct order for fct_inorder()
datalong <- datalong %>% arrange(rev(response))

plot <- ggplot(datalong, aes(x = label, y = value, fill = fct_inorder(Frequency))) + 
  geom_bar(position = "fill",stat = "identity") + coord_flip() +
  theme_classic() + scale_fill_manual(values = c("#ffffcc", 
                                                 "#c7e9b4",
                                                 "#7fcdbb",
                                                 "#41b6c4",
                                                 "#2c7fb8",
                                                 "#253494")) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(hjust = 1.2,
                                   size = 10),
        legend.title = element_blank(),
        text = element_text(family = "Courier")) + 
  annotation_custom(g2, xmin=3.5, xmax=4.5, 
                    ymin=0.29, ymax=.49) + 
  annotation_custom(g3, xmin=2.5, xmax=3.5, 
                    ymin=0.16, ymax=.36) + 
  annotation_custom(g1, xmin=1.5, xmax=2.5, 
                    ymin=0.35, ymax=.55) +
  annotation_custom(g4, xmin=0.5, xmax=1.5, 
                    ymin=0.65, ymax=.85) + 
  scale_y_continuous(labels = scales::percent_format())


# Making title
titletext <- grobTree(textGrob("On average, how often do you and your [five-year-old] child...",
                              x = unit(1, "lines"), y = unit(0, "lines"),
                              hjust = 0, vjust = -1,
                              gp=gpar(fontsize=12, col="black",
                                      fontfamily = "Courier")))


# Making well text
welltext <- grobTree(textGrob("Dataset: Hart, S. A., Ganley, C., & Purpura, D. (2021). Home Math Environment Study (1–). LDbase. http://ldbase.org/projects/49d02437-600d-45ab-ac52-04c91ad6502a",
                               x = unit(1, "lines"), y = unit(0, "lines"),
                               hjust = 0, vjust = -1,
                               gp=gpar(fontsize=6, col="black",
                                       fontfamily="Courier")))

# Combining plots
plot2 <- cowplot::plot_grid(plot, welltext,  ncol=1, rel_heights = c(1, .2))

finalplot <- cowplot::plot_grid(titletext, plot2,  ncol=1, rel_heights = c(.1, 1))

finalplot
