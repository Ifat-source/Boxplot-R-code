# ===============================
# LIBRARIES
# ===============================
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(dunn.test)

# ===============================
# DEFINE OUTPUT PATH
# ===============================
output_path <- "F:/BUET Thesis/Flood Prediction Model by Artificial Inteligance/Evolving Earth/Revewer comment from Evolving Earth"

dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# ===============================
# LOAD DATA
# ===============================
df <- read_excel(file.choose())

# ===============================
# CLEAN COLUMN NAMES
# ===============================
names(df) <- trimws(names(df))

df <- df %>%
  rename(
    Model = `Model name`,
    RMSE  = `RMSE`,
    NSE   = `NSE`,
    PBIAS = `PBIAS (%)`,
    nRMSE = `nRMSE (%)`
  )

# ===============================
# MODEL ORDER
# ===============================
df$Model <- factor(df$Model, levels = c("ANN", "SVM", "RF", "GBM"))

# ===============================
# PERFORMANCE MATRIX
# ===============================
performance_matrix <- df %>%
  group_by(Model) %>%
  summarise(
    RMSE_mean  = mean(RMSE, na.rm = TRUE),
    RMSE_sd    = sd(RMSE, na.rm = TRUE),
    
    nRMSE_mean = mean(nRMSE, na.rm = TRUE),
    nRMSE_sd   = sd(nRMSE, na.rm = TRUE),
    
    NSE_mean   = mean(NSE, na.rm = TRUE),
    NSE_sd     = sd(NSE, na.rm = TRUE),
    
    PBIAS_mean = mean(PBIAS, na.rm = TRUE),
    PBIAS_sd   = sd(PBIAS, na.rm = TRUE)
  )

# ===============================
# SAVE EXCEL
# ===============================
wb <- createWorkbook()
addWorksheet(wb, "Performance_Matrix")
writeData(wb, "Performance_Matrix", performance_matrix)

saveWorkbook(
  wb,
  file.path(output_path, "GOF_Statistical_Results.xlsx"),
  overwrite = TRUE
)

# ===============================
# BOXPLOT FUNCTION
# ===============================
create_boxplot <- function(data, y_var, file_name,
                           y_label,
                           y_min = NULL, y_max = NULL, step = NULL) {
  
  p <- ggplot(data, aes(x = Model, y = .data[[y_var]], fill = Model)) +
    
    geom_boxplot(
      width = 0.4,
      color = "black",
      alpha = 0.7,
      outlier.color = "red"
    ) +
    
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      size = 3,
      fill = "red"
    ) +
    
    scale_fill_brewer(palette = "Set2") +
    
    labs(
      title = paste("Boxplot of", y_var),
      x = "ML Model",
      y = y_label
    ) +
    
    theme_bw(base_size = 13) +
    
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      
      # 🔥 ONLY CHANGE (AXIS NUMBERS BOLD & BLACK)
      axis.text.x = element_text(color = "black", face = "bold"),
      axis.text.y = element_text(color = "black", face = "bold"),
      
      plot.title = element_text(hjust = 0.5, face = "bold"),
      aspect.ratio = 0.8
    )
  
  # ===============================
  # Y RANGE
  # ===============================
  if(!is.null(y_min) & !is.null(y_max)) {
    p <- p + coord_cartesian(ylim = c(y_min, y_max))
  }
  
  # ===============================
  # Y INTERVAL
  # ===============================
  if(!is.null(step) & !is.null(y_min) & !is.null(y_max)) {
    p <- p + scale_y_continuous(
      breaks = seq(y_min, y_max, by = step)
    )
  }
  
  # ===============================
  # SAVE FIGURE
  # ===============================
  ggsave(
    filename = file.path(output_path, file_name),
    plot = p,
    width = 10,
    height = 7,
    dpi = 600
  )
  
  return(p)
}

# ===============================
# CREATE PLOTS
# ===============================

p1 <- create_boxplot(df, "RMSE", "RMSE.png",
                     "RMSE (m³/sec)", 0, 12000, 1000)

p2 <- create_boxplot(df, "nRMSE", "nRMSE.png",
                     "nRMSE (%)", 0, 100, 10)

p3 <- create_boxplot(df, "NSE", "NSE.png",
                     "NSE", 0.2, 1, 0.1)

p4 <- create_boxplot(df, "PBIAS", "PBIAS.png",
                     "PBIAS (%)", -25, 1, 5)

# ===============================
# DISPLAY
# ===============================
print(p1)
print(p2)
print(p3)
print(p4)
