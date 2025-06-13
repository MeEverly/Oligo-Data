#### INSTALL PACKAGES AND LOAD LIBRARYS ####
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("colorspace")
library("ggplot2")
library("gridExtra")
library("colorspace") 

#### RETRIEVE DATA ####
setwd("C:\\Users\\CSV Files")            # CHANGE TO YOUR WORKING DIRECTORY
data1<-read.csv("NAME.csv", skip = 1)    # CHANGE FILE NAMES
data2<-read.csv("NAME.csv", skip = 1)    # CHANGE VALUE FOR SKIP IF NEEDED (SKIPS ROWS IN CSV FILE)
                                         # ADD, DELETE, OR HIDE DATA FRAME LINES

#### OPTIONAL - CHOOSE COLOR PALETTE WITH COLORSPACE #### 
#pal <- choose_palette()
#pal(7)

#Palette 1: "#070707" "#4A0046" "#870061" "#C30E62" "#E85D48" "#F0A773" "#FDF5EB"
#Palette 2: "#005256" "#006B6E" "#008688" "#60A3A5" "#9EC1C2" "#D5E0E1" "#E5DCE2" "#CFB2C5" "#B98AAA" "#A3618F" "#8F3377" "#7F0064"
#Palette 3: "#003B91" "#612D99" "#8C2595" "#A72E89" "#B84375" "#C05C5B" "#C0763B" "#B98E1B" "#ABA626" "#96BC52" "#7BD082" "#63E2B1" "#68EFDC" "#B0F4FA"

#### GRAPH APPEARANCE ####
plot_data <- function(data, legend_names, line_colors, line_types, 
                      y_limits = c(0, 0.18),                          # EDIT VALUES TO FIT DATA
                      y_breaks = seq(0, 0.18, by = 0.09)) {           # EDIT VALUES TO FIT DATA
  df_names <- names(legend_names)
  p <- ggplot()
  # Create multiple plots
  for (i in seq_along(df_names)) {
    p <- p + geom_line(data = data, 
                       aes_string(x = paste0("Wavelength", ifelse(i == 1, "", paste0(".", i - 1))),
                                  y = paste0("Absorbance", ifelse(i == 1, "", paste0(".", i - 1))),
                                  color = shQuote(df_names[i]),
                                  linetype = shQuote(df_names[i])), 
                       linewidth = 0.8)                               # EDIT VALUE TO CHANGE WIDTHS OF PLOT LINES
  }
  # Customize plot
  p + scale_color_manual(values = line_colors, labels = legend_names) +
    scale_linetype_manual(values = line_types, labels = legend_names) +
    guides(color = guide_legend(override.aes = list(linetype = line_types))) +
    labs(title = NULL, x = "Wavelength (nm)", y = "Absorbance (a.u.)", color = NULL, linetype = NULL) +
    
    scale_x_continuous(expand = c(0, 0),
                       limits = c(320, 370),                          # EDIT VALUES TO FIT DATA
                       breaks = seq(320, 370, by = 10)) +             # EDIT VALUES TO FIT DATA
    
    scale_y_continuous(expand = c(0, 0), 
                       limits = y_limits, 
                       breaks = y_breaks) +

    coord_cartesian(clip = "off") +
    
    theme_classic() +
    theme(
      # X-axis ticks and line 
      axis.text.x = element_text(color = "black", size = 8, family = "sans"),
      axis.title.x = element_text(color = "black", size = 10, family = "sans"),
      axis.line.x =  element_line(color = "black", linewidth = 0.8),
      axis.ticks.x = element_line(color = "black", linewidth = 0.8),
      
      # Y-axis ticks and line 
      axis.text.y = element_text(color = "black", size = 8, family = "sans"),
      axis.title.y = element_text(vjust = 2, color = "black", size = 10, family = "sans"),
      axis.line.y =  element_line(color = "black", linewidth = 0.8),
      axis.ticks.y = element_line(color = "black", linewidth = 0.8),
      
      # Plot
      panel.background = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      
      # Legend
      legend.text = element_text(color = "black", size = 8, family = "sans", margin = margin(l = 1, unit = "pt")),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.width = unit(0.3, "cm"),
      legend.position = c(0, 1.05),                                  # EDIT VALUES TO CHANGE LOCATION OF LEGEND
      legend.justification = c(0, 1),
      legend.spacing.y = unit(0, "cm"),                              # EDIT VALUES TO CHANGE SPACE BETWEEN LEGENED ELEMENTS
      legend.key.height = unit(0.3, "cm"))
}

#### CODE FOR DATA 1 ####
legend_names1 <- c(                                                  # DEFINE CUSTOM LEGEND NAMES
  "df1.1" = "INVX",
  "df1.2" = "ON1:INVX",
  "df1.3" = "ON2:INVX",
  "df1.4" = "ON3:INVX",
  "df1.5" = "ON4:INVX")
line_colors1 <- c(                                                   # DEFINE CUSTOM LINE COLORS
  "df1.1" = "#0080beff", 
  "df1.2" = "#045175ff",
  "df1.3" = "#ff7d7dff",
  "df1.4" = "#7b0457ff",
  "df1.5" = "#ad6d9eff")
line_types1 <- c(                                                    # DEFINE CUSTOM LINE TYPES
  "df1.1" = "solid", 
  "df1.2" = "solid",
  "df1.3" = "solid",
  "df1.4" = "solid",
  "df1.5" = "solid")
plot1 <- plot_data(data1, legend_names1, line_colors1, line_types1)

#### CODE FOR DATA 2 ####
legend_names2 <- c(
  "df2.1" = "INVY",
  "df2.2" = "ON5:INVY",
  "df2.3" = "ON6:INVY",
  "df2.4" = "ON7:INVY") 
line_colors2 <- c(
  "df2.1" = "#0080beff", 
  "df2.2" = "#045175ff",
  "df2.3" = "#ff7d7dff",
  "df2.4" = "#7b0457ff")
line_types2 <- c(
  "df2.1" = "solid", 
  "df2.2" = "solid",
  "df2.3" = "solid",
  "df2.4" = "solid")
plot2 <- plot_data(data2, legend_names2, line_colors2, line_types2)

#### ARRANGE PLOTS ####
grid.arrange(plot1, plot2, ncol = 2)
