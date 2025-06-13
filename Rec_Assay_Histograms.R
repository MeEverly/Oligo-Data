#### INSTALL PACKAGES AND LOAD LIBRARYS ####
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("colorspace")
library("ggplot2")
library("gridExtra")
library("tidyr")
library("dplyr")
library("colorspace") 

#### RETRIEVE DATA ####
setwd("C:\\Users\\CSV Data Files")            # CHANGE TO YOUR WORKING DIRECTORY
data<-read.csv("NAME.csv", skip = 1)          # CHANGE FILE NAMES
#print(data)                                  # OPTIONAL - PRINT DATA TO CONSOLE

#### OPTIONAL - CHOOSE COLOR PALETTE WITH COLORSPACE #### 
#pal <- choose_palette()
#pal(7)

#Palette 1: "#070707" "#4A0046" "#870061" "#C30E62" "#E85D48" "#F0A773" "#FDF5EB"
#Palette 2: "#005256" "#006B6E" "#008688" "#60A3A5" "#9EC1C2" "#D5E0E1" "#E5DCE2" "#CFB2C5" "#B98AAA" "#A3618F" "#8F3377" "#7F0064"
#Palette 3: "#003B91" "#612D99" "#8C2595" "#A72E89" "#B84375" "#C05C5B" "#C0763B" "#B98E1B" "#ABA626" "#96BC52" "#7BD082" "#63E2B1" "#68EFDC" "#B0F4FA"

#### ORGANIZE DATA FOR BAR PLOT ####
data <- data %>%                                                    # PLOT DATA IN ORDER OF APPERANCE IN CSV FILE
  mutate(RowOrder = row_number())
data_long <- data %>%                                               # RESHAPE DATA FOR EASIER PLOTTING
  pivot_longer(cols = starts_with("Probe"),
               names_to = "Probe_Set", values_to = "Probe") %>%
  mutate(Response = case_when(
    Probe_Set == "Probe" ~ Response,
    Probe_Set == "Probe.1" ~ Response.1,
    TRUE ~ NA_real_),
    se = case_when(
      Probe_Set == "Probe" ~ se,
      Probe_Set == "Probe.1" ~ se.1,
      TRUE ~ NA_real_)) %>%
  filter(!is.na(Response)) %>%
  arrange(RowOrder)

#### GRAPH APPEARANCE ####
create_plot <- function(data, title) {
  ggplot(data, aes(x = factor(Probe, levels = unique(Probe)), y = Response)) +
    geom_bar(stat = "identity", fill = "#045175ff", width = 0.5, alpha = 1) +                  # EDIT BAR "FILL" COLOR
    geom_errorbar(aes(ymin = pmax(Response - se, 0), ymax = Response + se), width = 0.2) +     # EDIT ERROR BAR "WIDTH"
    labs(title = NULL, x = NULL, y = "% Recognition") +
    
    theme_classic() +
    theme(
      # X-axis ticks and line 
      axis.text.x = element_text(color = "black", size = 8, family = "sans", angle = 45, hjust = 1, vjust = 1),
      axis.title.x = element_text(color = "black", size = 8, family = "sans"),
      axis.line.x = element_line(color = "black", linewidth = 0.3),
      axis.ticks.x = element_line(color = "black", linewidth = 0.3),
     
      # Y-axis ticks and line 
      axis.text.y = element_text(color = "black", size = 8, family = "sans"),
      axis.title.y = element_text(vjust = 2, color = "black", size = 8, family = "sans"),
      axis.line.y = element_line(color = "black", linewidth = 0.3),
      axis.ticks.y = element_line(color = "black", linewidth = 0.3),
      
      # Gridlines
      panel.grid.major.y = element_line(color = "grey95", linewidth = 0.3)) +                  # REMOVE/HIDE LINE OR EDIT COLOR/WIDTH
    
    scale_x_discrete(drop = FALSE) +
    
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 105),                                                                      # EDIT VALUES TO FIT DATA
      breaks = seq(0, 100, by = 20))                                                           # EDIT VALUES TO FIT DATA
}

#### ORGANIZE DATA BY PROBE_SET ####
plots <- lapply(unique(data_long$Probe_Set), function(probe_set) {
  subset_data <- data_long %>% filter(Probe_Set == probe_set)
  title <- paste("Data for", gsub("Probe", "", probe_set))
  create_plot(subset_data, title)
})

#### ARRANGE PLOTS ####
grid.arrange(grobs = plots, ncol = 2)

