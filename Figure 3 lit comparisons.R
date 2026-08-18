library(readxl)
library(ggplot2)

# Import Excel file
#data <- read_excel("~/Desktop/LPUVcompRn.xlsx")
data <- read_excel("~/Desktop/LPUVcompRnshort.xlsx")

# Make study a factor so it keeps the order from your file
data$study <- factor(data$study, levels = unique(data$study))
data$virus <- factor(data$virus, levels = rev(sort(unique(data$virus))))

#wes anderson color palette

library(wesanderson)
pLP <- ggplot(data, aes(x = rate, y = virus, color = study, shape = study)) +
  guides(
    color = guide_legend(title = "LP-UV (254 nm)"),
    shape = guide_legend(title = "LP-UV (254 nm)")
  )+
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "Bae & Shin (2016)" = wes_palette("GrandBudapest1")[4],
      "Battigelli et al. (1993)" = wes_palette("Cavalcanti1")[1],
      "Beck et al. (2017)" = wes_palette("Darjeeling2")[2], 
      "Calgua et al. (2014)" = wes_palette("GrandBudapest1")[1], 
      "Chang et al. (1985)" = wes_palette("Cavalcanti1")[2],
      "de Roda Husman et al. (2004)" = wes_palette("GrandBudapest1")[3], 
      "Gerba et al. (2002)" = wes_palette("Moonrise3")[2],
      "Harris et al. (1987)" = wes_palette("Cavalcanti1")[3],
      #"Kim et al. (2017) x 10^2"= 
      "Maier et al. (1995)" = wes_palette("Cavalcanti1")[5],
      "Meng & Gerba (1996)" = wes_palette("Moonrise3")[1],
      "Nuanualsuwan et al. (2002)" = wes_palette("Cavalcanti1")[4],
      "Oguma et al. (2016)" = wes_palette("Moonrise3")[4],
      "Park et al. (2011)" = wes_palette("GrandBudapest2")[4],
      "Rattanakul & Oguma (2018)" = wes_palette("Moonrise2")[1],
      "Ryu et al. (2018)" = wes_palette("BottleRocket2")[3],
      "Shin et al. (2005)" =wes_palette("GrandBudapest2")[3], 
      "Sholtes et al. (2016)" = wes_palette("Moonrise3")[3],
      "Simonet & Gantzer (2006)" = wes_palette("GrandBudapest2")[2], 
      "Thurston-Enriquez et al. (2003)" = wes_palette("Moonrise3")[5],
      "Ye et al. (2018)" = wes_palette("GrandBudapest1")[2],
      "This study" = wes_palette("GrandBudapest2")[1]
    )
    ) +
  scale_shape_manual(
    values = c(
      "Bae & Shin (2016)" = 1,
      "Battigelli et al. (1993)" = 2,
      "Beck et al. (2017)" = 17, 
      "Calgua et al. (2014)" = 15, 
      "Chang et al. (1985)" = 10,
      "de Roda Husman et al. (2004)" = 3, 
      "Gerba et al. (2002)" = 7,
      "Harris et al. (1987)" = 5,
      #"Kim et al. (2017) x 10^2"= 43,
      "Maier et al. (1995)" = 6,
      "Meng & Gerba (1996)" = 11,
      "Nuanualsuwan et al. (2002)" = 14,
      "Oguma et al. (2016)" = 13,
      "Park et al. (2011)" = 4, 
      "Rattanakul & Oguma (2018)" = 20,
      "Ryu et al. (2018)" = 12,
      "Shin et al. (2005)" = 16, 
      "Sholtes et al. (2016)" = 9,
      "Simonet & Gantzer (2006)" = 17, 
      "Thurston-Enriquez et al. (2003)" = 12,
      "Ye et al. (2018)" = 8,
      "This study" = 18
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 0.45, by = 0.05)
  ) +
  theme_bw() +
  labs(
    title = "a) LP-UV (254 nm)",
    y = "",
    x = "First-order inactivation rate constants (cm²/mJ)",
    color = "Study"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  )

ggsave("LPUVcomp_plot.png", width = 12, height = 4.5, units = "in", dpi = 300)

##############################. 255. ################################################

# Import Excel file
#data <- read_excel("~/Desktop/255nmcompR.xlsx")
data <- read_excel("~/Desktop/255nmcompRshort.xlsx")

# Make study a factor so it keeps the order from your file
data$study <- factor(data$study, levels = unique(data$study))
data$virus <- factor(data$virus, levels = rev(sort(unique(data$virus))))

#wes anderson color palette
#GrandBudapest1
#GrandBudapest2
library(wesanderson)

p255 <- ggplot(data, aes(x = rate, y = virus, color = study, shape = study)) +
  guides(
    color = guide_legend(title = "LED-UV 255 nm"),
    shape = guide_legend(title = "LED-UV 255 nm")
  )+
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "Aoyagi et al. (2011)" = wes_palette("Zissou1")[1],
      "Martino et al. (2021)" = wes_palette("Darjeeling2")[2],
      "This study" = wes_palette("GrandBudapest2")[1] 
    )
  ) +
  scale_shape_manual(
    values = c(
      "Aoyagi et al. (2011)" = 16, 
      "Martino et al. (2021)" = 43,
      "This study" = 18 
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 0.65, by = 0.1)
  ) +
  theme_bw() +
  labs(
    title = "b) LED-UV (255 nm)",
    y = "",
    x = "First-order inactivation rate constants (cm²/mJ)",
    color = "Study"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  )
ggsave("255UVcomp_plot.png", width = 8.5, height = 4.5, units = "in", dpi = 300)

##############################. 265. ################################################

# Import Excel file
#data <- read_excel("~/Desktop/265nmcompR.xlsx")
data <- read_excel("~/Desktop/265nmcompRshort.xlsx")

# Make study a factor so it keeps the order from your file
data$study <- factor(data$study, levels = unique(data$study))
data$virus <- factor(data$virus, levels = rev(sort(unique(data$virus))))

#wes anderson color palette
#GrandBudapest1
#GrandBudapest2
library(wesanderson)

p265 <- ggplot(data, aes(x = rate, y = virus, color = study, shape = study)) +
  guides(
    color = guide_legend(title = "LED-UV 265 nm"),
    shape = guide_legend(title = "LED-UV 265 nm")
  )+
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "Beck et al. (2017) - 260 nm" = wes_palette("Darjeeling2")[2], 
      "Canh et al. (2023)" = wes_palette("Zissou1")[4],
      #"Kim et al. (2017) x 10^2 - 266 nm"= wes_palette("Darjeeling2")[2],
      "Martino et al. (2021)" = wes_palette("Darjeeling2")[2],
      "Oguma et al. (2019)" = wes_palette("Zissou1")[2],
      "Rattanakul & Oguma (2018)" = wes_palette("Moonrise2")[1],
      "Sholtes et al. (2016) - 260 nm" = wes_palette("Moonrise3")[3],
      "Woo et al. (2019) - 260 nm" = wes_palette("Cavalcanti1")[2],
      "This study" =wes_palette("GrandBudapest2")[1] 
    ),
  ) +
  scale_shape_manual(
    values = c(
      "Beck et al. (2017) - 260 nm" = 17,
      "Canh et al. (2023)" = 1, 
      "Martino et al. (2021)" = 43,
      #"Kim et al. (2017) x 10^2 - 266 nm"= 43,
      "Oguma et al. (2019)" = 15, 
      "Rattanakul & Oguma (2018)" = 20,
      "Sholtes et al. (2016) - 260 nm" = 9,
      "Woo et al. (2019) - 260 nm" = 7, 
      "This study" = 18
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 0.65, by = 0.1)
  ) +
  theme_bw() +
  labs(
    title = "c) LED-UV (265 nm)",
    y = "",
    x = "First-order inactivation rate constants (cm²/mJ)",
    color = "Study"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  )
ggsave("265UVcomp_plot.png", width = 9.8, height = 4.5, units = "in", dpi = 300)

###############################  285.  #############################################
# Import Excel file
#data <- read_excel("~/Desktop/285nmcompR.xlsx")
data <- read_excel("~/Desktop/285nmcompRshort.xlsx")

# Make study a factor so it keeps the order from your file
data$study <- factor(data$study, levels = unique(data$study))
data$virus <- factor(data$virus, levels = rev(sort(unique(data$virus))))

#wes anderson color palette
#GrandBudapest1
#GrandBudapest2
library(wesanderson)

p285 <- ggplot(data, aes(x = rate, y = virus, color = study, shape = study)) +
  guides(
    color = guide_legend(title = "LED-UV 285 nm"),
    shape = guide_legend(title = "LED-UV 285 nm")
  )+
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "Aoyagi et al. (2011) - 280 nm" = wes_palette("Zissou1")[1], 
      "Beck et al. (2017) - 280 nm" = wes_palette("Darjeeling2")[2], 
      "Canh et al. (2023) - 280 nm" = wes_palette("Zissou1")[4],
      "Martino et al. (2021)" = wes_palette("Darjeeling2")[2],
      #"Kim et al. (2017) x 10^2 - 279 nm"= wes_palette("Darjeeling2")[2],
      "Oguma et al. (2016)" = wes_palette("Moonrise3")[4],
      "Oguma et al. (2019) - 280 nm" = wes_palette("Zissou1")[2],
      "Rattanakul & Oguma (2018) - 280 nm" = wes_palette("Moonrise2")[1],
      "Woo et al. (2019) - 280 nm" = wes_palette("Cavalcanti1")[2],
      "This study" = wes_palette("GrandBudapest2")[1]
    )
  ) +
  scale_shape_manual(
    values = c(
      "Aoyagi et al. (2011) - 280 nm" = 16, 
      "Beck et al. (2017) - 280 nm" = 17, 
      "Canh et al. (2023) - 280 nm" = 1,
      "Martino et al. (2021)" = 43,
      #"Kim et al. (2017) x 10^2 - 279 nm"= 43,
      "Oguma et al. (2016)" = 13,
      "Oguma et al. (2019) - 280 nm" = 15,
      "Rattanakul & Oguma (2018) - 280 nm" = 20,
      "Woo et al. (2019) - 280 nm" = 7, 
      "This study" = 18
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 0.4, by = 0.05)
  ) +
  theme_bw() +
  labs(
    title = "d) LED-UV (285 nm)",
    y = "",
    x = "First-order inactivation rate constants (cm²/mJ)",
    color = "Study"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  )
ggsave("285UVcomp_plot.png", width = 9.8, height = 4.5, units = "in", dpi = 300)

########################### combining all the plots to one graphic 
#install.packages("patchwork")
library(patchwork)

small_theme <- theme(
  #plot.title = element_text(size = 10, face = "bold"),
  #axis.title = element_text(size = 9),
  #axis.text = element_text(size = 8),
  #margins = margin(0, 0, 0, 0),
  #plot.margin = margin(0, 0, 0, 0),
  legend.title = element_text(size = 7),
  legend.text = element_text(size = 6),
  legend.key.size = unit(0.3, "cm"),
  legend.spacing.y = unit(0.02, "cm"),
  legend.margin = margin(0, 0, 0, 0),
  legend.box.margin = margin(0, 0, 0, 0)
)

pLP  <- pLP  + small_theme
p255 <- p255 + small_theme
p265 <- p265 + small_theme
p285 <- p285 + small_theme

combined_plot <- pLP / p255 / p265 / p285 +
  plot_layout(heights = c(1, 1, 1, 1))

 ggsave(
  "UV_plots.png",
  combined_plot,
  width = 8,
  height = 10,
  units = "in",
  dpi = 600
)
