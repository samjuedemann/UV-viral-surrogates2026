library(readxl)
library(ggplot2)
library(wesanderson)
library(patchwork)

files <- list(
  list(file = "./fig2_data/LP_254.xlsx",  title = "a) LP-UV (254 nm)"),
  list(file = "./fig2_data/LED_255.xlsx", title = "b) LED-UV (255 nm)"),
  list(file = "./fig2_data/LED_265.xlsx", title = "c) LED-UV (265 nm)"),
  list(file = "./fig2_data/LED_285.xlsx", title = "d) LED-UV (285 nm)")
)

palette_col <- c(wes_palette("Royal1")[2], wes_palette("Chevalier1")[2], 
                 wes_palette("Royal2")[5],wes_palette("Zissou1")[1], 
                 wes_palette("Royal2")[3],  wes_palette("IsleofDogs1")[1],
                 wes_palette("FantasticFox1")[1], wes_palette("Royal1")[1])

names(palette_col) <- c("ADV", "ECV", "FCV", "MS2", "P22", "phi6", "phiX174", "Qbeta")

point_shape <- c(ADV = 0, ECV = 1, FCV = 2, MS2 = 15,
                 P22 = 16, phi6 = 17, phiX174 = 18, Qbeta = 20)

line_type <- c(ADV = "solid", ECV = "solid", FCV = "solid", MS2 = "solid",
              P22 = "solid", phi6 = "solid", phiX174 = "solid", Qbeta = "solid")

################### create the plot ############################
process_and_plot <- function(data, title, show_xlab = TRUE, show_ylab = TRUE) {
  
  # Make sure virus is treated as a factor in the desired order
  data$virus <- factor(data$virus,
    levels = c("ADV", "ECV", "FCV", "MS2", "P22", "phi6", "phiX174", "Qbeta"))
 
  averages <- aggregate(logred ~ virus + fluence, data = data, FUN = mean)
  sds <- aggregate(logred ~ virus + fluence, data = data, FUN = sd)
  
  # Rename SD column
  names(sds)[names(sds) == "logred"] <- "sd"
  
  # Combine averages and SD
  averages <- merge(averages, sds, by = c("virus", "fluence"))
  
  p <- ggplot(
    averages,
    aes(x = fluence, y = logred, color = virus, shape = virus)) +
    
    # Error bars
    geom_errorbar(
      aes(ymin = logred - sd, ymax = logred + sd),
      width = 0, linewidth = 0.5, color = "black"
      ) +
    
    # Points
    geom_point(size = 3) +
    
    # Regression lines through origin
    geom_smooth(data = data,
      aes(x = fluence, y = logred, color = virus, ),
      method = "lm",
      formula = y ~ x - 1,
      se = FALSE,
      linewidth = 0.8
      ) +
    
    # Colors
    scale_color_manual(
      values = palette_col,
      name = "Virus"
    ) +
    
    # Shapes
    scale_shape_manual(
      values = point_shape,
      name = "Virus"
    ) +
    
    # Same x and y limits
    scale_x_continuous(expand = expansion(mult = c(0, 0.03))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    coord_cartesian(ylim = c(0, 8)) +
    
    # Labels
    labs(
      title = title,
      x = if(show_xlab) "Fluence (mJ/cm²)" else NULL,
      y = if(show_ylab) expression(LRV~(log(N[0]/N[t]))) else NULL
    ) +
    
    # Theme
    theme_bw() +
    
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text = element_text(size = 11),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      legend.key.width = unit(1.2,"cm"),
      legend.position = "right",
      
      # Keep the plot areas the same size
      plot.margin = margin(5.5,5.5,5.5,5.5)
    )
  return(p)
}

# LP-UV
data_LP <- read_excel(files[[1]]$file)
p1 <- process_and_plot(
  data_LP, files[[1]]$title,
  show_xlab = TRUE,
  show_ylab = TRUE
)

# 255 nm
data_255 <- read_excel(files[[2]]$file)
p2 <- process_and_plot(
  data_255, files[[2]]$title,
  show_xlab = TRUE,
  show_ylab = TRUE
)

# 265 nm
data_265 <- read_excel(files[[3]]$file)
p3 <- process_and_plot(
  data_265, files[[3]]$title,
  show_xlab = TRUE,
  show_ylab = TRUE
)

# 285 nm
data_285 <- read_excel(files[[4]]$file)
p4 <- process_and_plot(
  data_285, files[[4]]$title,
  show_xlab = TRUE,
  show_ylab = TRUE
)

################## combine into one plot #############################
combined_plot <- (p1 + p2) /(p3 + p4) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave(
  "~/Desktop/Fig2summary.png",
  combined_plot,
  width = 9,
  height = 6,
  units = "in",
  dpi = 600
)

# Display figure
combined_plot

######################## slopes SE and R2 table ###########################
process_and_calculate_slopes <- function(data, title) {
  viruses <- c("ADV", "ECV", "FCV", "MS2", "P22", 
               "phi6", "phiX174", "Qbeta")
  results <- lapply(viruses, function(v) {
    
    # Extract virus data
    d <- data[data$virus == v, c("fluence", "logred")]
    
    # Skip if no data
    if(nrow(d) < 2){
      return(data.frame(UV = title, Virus = v,
        Slope = NA, SE = NA, R2 = NA))
    }
    
    # Fit model
    model <- lm(logred ~ fluence - 1, data = d)
    
    # Extract coefficient information
    coef_summary <- summary(model)$coefficients
    
    data.frame(UV = title, Virus = v,
      Slope = coef_summary[1, "Estimate"], SE = coef_summary[1, "Std. Error"],
      R2 = summary(model)$r.squared)
  })
  do.call(rbind, results)
}

# Empty table
slopes_df <- data.frame()
# Loop through files
for (file_info in files) {
  data <- read_excel(file_info$file)
  slopes_df <- rbind(slopes_df,
    process_and_calculate_slopes(data, file_info$title)
  )
}
#install.packages("writexl")
library(writexl)
write_xlsx(slopes_df, "updated_slopes.xlsx")
