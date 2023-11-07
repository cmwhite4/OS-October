##### Set-up -------------------------------------------------------------------
setwd("~/Desktop/Open Science October/Week 1")
library(haven)
library(tidyverse)
library(ggplot2)
library(extrafont)
library(grid)
library(gridExtra)
library(ggpubr)

# Read in data
data <- read_sav("R305A180293_child-level_CT_v35.sav") %>%
  select(childid, tcage_t1, pl_ss1_t1:pl_ss10_t1) %>%
  filter(tcage_t1 < 5)

# Get proportion correct for each prompt
data2 <- data %>% pivot_longer(pl_ss1_t1:pl_ss10_t1) %>%
  group_by(name, tcage_t1) %>%
  summarise(n=n(),
            n_correct = sum(value, na.rm = T),
            prop_correct = n_correct/n)

data3 <- data2 %>% select(tcage_t1, prop_correct) %>%
  pivot_wider(names_from = tcage_t1, 
              values_from = prop_correct)

colnames(data3) <- c("Prompt", "Three", "Four")


##### Plot ---------------------------------------------------------------------

axistext <- c("Point to the table!",
"Knock on the table!",
"Stack your hands!",
"Turn the paper over!",
"Pick up the paper!",
"Open your hand!",
"Lift one foot!",
"Point to the door!",
"Put your feet together!",
"Touch your ear!")

plot <- ggplot(data3) + 
  geom_segment(aes(x=Prompt, xend = Prompt, y = Three, yend = Four)) + 
  geom_point(aes(x=Prompt, y = Three), color = "#c44535", size = 4) +
  geom_point(aes(x=Prompt, y = Four), color = "#1a7278", size = 4) +
  scale_x_discrete(labels = rev(axistext)) +
  scale_y_continuous(breaks = c(.2, .8),
                     limits = c(0, 1),
                     labels = c("20% Correct", "80% Correct")) +
  coord_flip() + 
  ggtitle("Simon Says...") + 
  theme_minimal() +
  theme(plot.title = element_text(family = "Luminari", 
                             color = "#494744", hjust = -.6,
                             size = 20),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 12),
        plot.background = element_rect(fill = "#F3ECE3"),
        panel.grid.major = element_line(color = "white")) +
  annotate("text",
           x = c(10, 10),
           y = c(.25, 0.75),
           label = c("3 Year Olds", "4 Year Olds"),
           family = "", size=4, color = c("#c44535", "#1a7278")) 

plot
