# LDbase.org | Open Science October 2023 | Week 2 | VOCO
# Christine White

##### Set-up -------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggtext)
library(cowplot)
library(extrafont)
library(grid)
library(gridExtra)

# Read in data
data <- read.csv("VOCO clean.csv", header = T)[-1]
data <- data[duplicated(data)==FALSE,] # Remove duplicated rows

# Pre-test data
pre <- data %>% filter(TOW1_TST==1 & TOW2_TST==1 &
                         COND==1) %>% 
  select(SID, TOWS1_1:TOWS1_104) %>%
  pivot_longer(TOWS1_1:TOWS1_104) 

# Post-test data 
post <- data %>% filter(TOW1_TST==1 & TOW2_TST==1 &
                          COND==1) %>% 
  select(SID, TOWS2_1R:TOWS2_104R) %>%
  pivot_longer(TOWS2_1R:TOWS2_104R)

##### Plots --------------------------------------------------------------------

preplot <- ggplot(pre, aes(x=factor(SID), 
                           y=fct_inorder(factor(name)))) +
  geom_tile(aes(fill = factor(value))) +
  scale_fill_manual(values = c("thistle", "mediumpurple4")) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(color = "black",
                                       fill = "black"),
        plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_text(color = "white",
                                     size = 8)) +
  guides(fill="none") +  labs(
    title = "<span style='color:#FFFFFF;font-size:11pt'>How often did 9th grade students read words correctly 
    <span style='color:#d8bfd8;'>**before**</span>
    &
    <span style='color:#c6e2ff;font-weight:bold'>**after**</span> intervention?
    </span>", subtitle = "In each plot, the darker shade represents correct responses. The target words increase in difficulty along the \ny-axis with the easiest words at the bottom and the most difficult words at the top. Interestingly, there were no \ncorrect reponses for the six most difficult words at either timepoint!")


postplot <- ggplot(post, aes(x=factor(SID), y=fct_inorder(factor(name)))) +
  geom_tile(aes(fill = factor(value))) +
  scale_fill_manual(values = c("slategray1", "skyblue4")) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(color = "black",
                                       fill = "black")) + 
  guides(fill="none")

# Combine pre and post plot
gg <- cowplot::plot_grid(preplot, 
                         postplot,  
                         ncol=1, 
                         rel_heights = c(1, .8))

# Add well text
welltext <- grobTree( rectGrob(gp=gpar(fill="black")), 
                   textGrob("Dataset: Vaughn, S., Swanson, E., Roberts, G., Martinez, L., Wanzek, J., Simmons, D., Clemens, N., & Fogarty, M. (2021). \nVocabulary and Comprehension (VoCO) Project. http://www.ldbase.org/datasets/38a9b8a2-65e3-425a-8480-5d3e94b37f45",
                            x = unit(1, "lines"), y = unit(0, "lines"),
                            hjust = 0, vjust = -1,
                            gp=gpar(fontsize=7, col="white")))


final_plot <- cowplot::plot_grid(gg, welltext,  ncol=1, rel_heights = c(1, .1))
final_plot
