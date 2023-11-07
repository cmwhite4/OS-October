# LDbase.org | Open Science October 2023 | Week 4 | Longitudinal
# Christine White

# Dataset: Petrill, S., Thompson, L., DeThorne, L. S., Plomin, R., & Schatschneider, C. (2021). 
# Western Reserve Reading and Math Project.

##### Set-up -------------------------------------------------------------------

# Load packages
library(stringr)
library(tidyverse)
library(ggplot2)
library(extrafont)
library(cowplot)
library(wesanderson)
library(grid)
library(gridExtra)

# Read in data
data <- read.csv("WRRMP Data Full.csv", header = TRUE)

# To find the variables
colnames(data)[str_detect(colnames(data), "home1")==TRUE]

# Data prep
data_sub <- data %>% filter(randomTwin==1) %>% 
  select(fid, amhome3, bmhome3, cmhome3, dmhome3, emhome3, fmhome3) %>%
  na.omit() # Item: "It's a real zoo in our home"

colnames(data_sub) <- c("ID", "Wave1", "Wave2", "Wave3", "Wave4", "Wave5", "Wave6")

##### Plot ---------------------------------------------------------------------
sub1 <- data_sub %>% slice_sample(n=5) 

plot1 <- sub1 %>% pivot_longer(cols = c(Wave1:Wave6), names_to = "Wave",
                          values_to = "Rating") %>%
  ggplot() + geom_line(aes(x=Wave, y=Rating, group = factor(ID), color = factor(ID)),
                       alpha = .7) + theme_minimal() + 
  scale_x_discrete(labels = c("Wave 1", " ", " ", " ", " ", "Wave 6")) + 
  scale_y_continuous(limits = c(0, 5),
                     labels = c(" ", "1", "2", "3", "4", "5")) +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5)) +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8, face = "bold"),
        text = element_text(family = "Trebuchet MS", face = "bold"),
        legend.position = "none")

plot1

sub2 <- data_sub %>% filter(! ID %in% unique(sub1$ID)) %>%
  slice_sample(n=5)

plot2 <- sub2 %>% pivot_longer(cols = c(Wave1:Wave6), names_to = "Wave",
                                     values_to = "Rating") %>%
  ggplot() + geom_line(aes(x=Wave, y=Rating, group = factor(ID), color = factor(ID)),
                       alpha = .7) + theme_minimal() + 
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5)) + 
  scale_y_continuous(limits = c(0, 5)) + 
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "Trebuchet MS"),
        legend.position = "none")


sub3 <- data_sub %>% filter(! ID %in% c(unique(sub1$ID), 
                                        unique(sub2$ID))) %>%
  slice_sample(n=5)


plot3 <- sub3 %>% pivot_longer(cols = c(Wave1:Wave6), names_to = "Wave",
                                     values_to = "Rating") %>%
  ggplot() + geom_line(aes(x=Wave, y=Rating, group = factor(ID), color = factor(ID)),
                       alpha = .7) + theme_minimal() + 
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5)) + 
  scale_y_continuous(limits = c(0, 5)) + 
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "Trebuchet MS"),
        legend.position = "none")


sub4 <- data_sub %>% filter(! ID %in% c(unique(sub1$ID), 
                                        unique(sub2$ID),
                                        unique(sub3$ID))) %>%
  slice_sample(n=5)


plot4 <- sub4 %>% pivot_longer(cols = c(Wave1:Wave6), names_to = "Wave",
                                     values_to = "Rating") %>%
  ggplot() + geom_line(aes(x=Wave, y=Rating, group = factor(ID), color = factor(ID)),
                       alpha = .7) + theme_minimal() + 
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5)) + 
  scale_y_continuous(limits = c(0, 5),
                     labels = c("  ", "  ", "  ", "  ", "  ", "  ")) + 
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        text = element_text(family = "Trebuchet MS"),
        legend.position = "none")

sub5 <- data_sub %>% filter(! ID %in% c(unique(sub1$ID), 
                                        unique(sub2$ID),
                                        unique(sub3$ID),
                                        unique(sub4$ID))) %>%
  slice_sample(n=5)

plot5 <- sub5 %>% pivot_longer(cols = c(Wave1:Wave6), names_to = "Wave",
                                     values_to = "Rating") %>%
  ggplot() + geom_line(aes(x=Wave, y=Rating, group = factor(ID), color = factor(ID)),
                       alpha = .7) + theme_minimal() + 
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5)) + 
  scale_y_continuous(limits = c(0, 5)) + 
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "Trebuchet MS"),
        legend.position = "none")


sub6 <- data_sub %>% filter(! ID %in% c(unique(sub1$ID), 
                                        unique(sub2$ID),
                                        unique(sub3$ID),
                                        unique(sub4$ID),
                                        unique(sub5$ID))) %>%
  slice_sample(n=5)


plot6 <- sub6 %>% pivot_longer(cols = c(Wave1:Wave6), names_to = "Wave",
                                     values_to = "Rating") %>%
  ggplot() + geom_line(aes(x=Wave, y=Rating, group = factor(ID), color = factor(ID)),
                       alpha = .7) + theme_minimal() + 
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5)) +
  scale_y_continuous(limits = c(0, 5)) + 
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "Trebuchet MS"),
        legend.position = "none")


combo <- cowplot::plot_grid(plot1, plot2, plot3,
                   plot4, plot5, plot6,
                   ncol = 3)


titletext <- grobTree(textGrob("The [in]stability of CHAOS",
                               x = unit(1, "lines"), y = unit(0, "lines"),
                               hjust = .05, vjust = -.7,
                               gp=gpar(fontsize=14, col="black",
                                       fontfamily = "Trebuchet MS")))

subtitletext <- grobTree(textGrob("Each plot contains data for a random sample of 5 caregivers who completed the Confusion, Hubbub, & Order scale (CHAOS).\nThe line represents their rating in response to the item, 'It's a real zoo in our home' over 6 waves of data collection, with \na rating of 1 meaning 'Definitely Untrue' and 5 meaning 'Definitely True'.",
                               x = unit(1, "lines"), y = unit(0, "lines"),
                               hjust = 0, vjust = -.2,
                               gp=gpar(fontsize=8, col="black",
                                       fontfamily = "Trebuchet MS")))



welltext <- grobTree(textGrob("Dataset: Petrill, S., Thompson, L., DeThorne, L. S., Plomin, R., & Schatschneider, C. (2021). Western Reserve Reading and Math Project. \nLDbase. https://doi.org/10.33009/ldbase.1643647076.d4b2",
                              x = unit(1, "lines"), y = unit(0, "lines"),
                              hjust = 0, vjust = -1,
                              gp=gpar(fontsize=6, col="black",
                                      fontfamily="Trebuchet MS")))

finalplot <- plot_grid(titletext, subtitletext, combo, welltext, ncol = 1,
          rel_heights = c(.2, .2, 1, .2))

finalplot
