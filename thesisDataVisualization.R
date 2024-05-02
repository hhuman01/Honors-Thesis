library(cowplot)
library(dplyr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(mgcv)
library(gridExtra)
library(patchwork)

create_plot_grid <- function(plot1, plot2, mo_breaks, xlims) {
  if (!inherits(plot1, "gg")) {
    stop("plot1 is not a valid ggplot object.")
  }

  # check if plot1 has been built successfully
  plot1_build <- ggplot_build(plot1)
  if (is.null(plot1_build)) {
    stop("failed to build plot1.")
  }
  # Apply x-axis limits to plot2
  timeline_plot_spec <- plot2 + 
    scale_x_date(
      limits = xlims,
      breaks = mo_breaks, 
      date_labels = "%b %Y") +
    theme(axis.ticks.x = element_line(color = "black"), 
          axis.ticks.length = unit(0.2, "cm"))
 
  grid <- plot_grid(plot1, timeline_plot_spec, ncol = 1, align = "v", axis = "l", rel_heights = c(2, 1)) +
    plot_layout(heights = c(2, 1), nrow = 1, ncol = 1)
  
  return(grid)
}

# data manipulation
df1 <- read.csv("tumorBurden4.csv")
df1$Date <- as.Date(mdy(df1$Date))

df2 <- read.csv("glucoseLevels.csv")
df2$Date <- as.Date(mdy(df2$Date))

df3 <- read.csv("a1cLevels.csv")
df3$Date <- as.Date(mdy(df3$Date))

df4 <- read.csv("treatmentChanges.csv")
df4$Start1 <- as.Date(df4$Start1, format = "%m/%d/%Y")
df4$End1 <- as.Date(df4$End1, format = "%m/%d/%Y")
df4$Start2 <- as.Date(df4$Start2, format = "%m/%d/%Y")
df4$End2 <- as.Date(df4$End2, format = "%m/%d/%Y")

df5 <- read.csv("glucoseXSTimeline.csv")
df5$Start1 <- as.Date(df5$Start1, format = "%m/%d/%Y")
df5$End1 <- as.Date(df5$End1, format = "%m/%d/%Y")
df5$Start2 <- as.Date(df5$Start2, format = "%m/%d/%Y")
df5$End2 <- as.Date(df5$End2, format = "%m/%d/%Y")

merged_df <- merge(merge(df1, df2, by = "Date", all = TRUE), df3, by = "Date", all = TRUE)

# plots
desired_order <- c("Resection Surgery", "GBM Diagnosis", "Metformin 800mg/day", "Metformin 1700mg/day" , "Glipizide Intake", "Radiation", "Chemotherapy", "Avastin")
df4$Treatment <- factor(df4$Treatment, levels = desired_order)
timeline_plot <- ggplot(df4, aes(y = Treatment, color = Treatment)) +
  geom_segment(aes(x = Start1, xend = End1, yend = Treatment), size = 5) +
  geom_segment(aes(x = Start2, xend = End2, yend = Treatment), size = 5) +  
  geom_text(aes(x = Start1, label = Treatment), vjust = 0.5, hjust = -0.1, size = 3, color = "black") +
  labs(x = "Date",
       y = element_blank(),
       color = "Treatment") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 12)) +
  xlim(as.Date("2020-10-01"), as.Date("2024-4-30"))
#df4 <- df4[, !(names(df4) %in% c("GapStart", "GapEnd"))]

df5$Treatment <- factor(df5$Treatment, levels = desired_order)
timeline_plot_xs <- ggplot(df5, aes(y = Treatment, color = Treatment)) +
  geom_segment(aes(x = Start1, xend = End1, yend = Treatment), size = 5) +
  geom_segment(aes(x = Start2, xend = End2, yend = Treatment), size = 5) +  
  geom_text(aes(x = Start1, label = Treatment), vjust = 0.5, hjust = -0.1, size = 3, color = "black") +
  labs(x = "Date",
       y = element_blank(),
       color = "Treatment") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 12)) +
  xlim(as.Date("2023-10-30"), as.Date("2024-4-30"))

resectionCavity <- ggplot(data = df1, aes(x = Date, y = EllipResection)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = FLAIR), vjust = -0.5, hjust = .4, size = 3) +
  labs(x = element_blank(), y = "Resection Cavity Size (cc)") +  
  ggtitle("Resection Cavity Size over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_blank()) +
  scale_x_date(
    limits = c(as.Date("2022-08-14"), as.Date("2024-04-30")),
    breaks = "3 months", 
    date_labels = "%b %Y")

resectionCavityLog <- ggplot(data = df1, aes(x = Date, y = log)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = FLAIR), vjust = -0.5, hjust = 0.75, size = 5) +
  labs(x = "Date", y = "log10 Resection Cavity Size (cc)") +  
  ggtitle("Resection Cavity Size over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_blank()) +
  scale_x_date(
    limits = c(as.Date("2022-08-14"), as.Date("2024-04-30")),
    breaks = "3 months", 
    date_labels = "%b %Y")

# xlim(as.Date("2022-08-14"), as.Date("2023-4-30"))

glucoseLevelsLong <- ggplot(data = df2, aes(x = Date, y = ReadingGlucose)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "Glucose Level(mg/dL)") +  
  ggtitle("Glucose Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_date(limits = c(as.Date("2020-10-01"), as.Date("2024-04-30")),
                breaks = "6 months", 
                date_labels = "%b %Y")


glucoseLevelsShort <- ggplot(data = df2, aes(x = Date, y = ReadingGlucose)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "Glucose Level(mg/dL)") +  
  ggtitle("Glucose Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_date(limits = c(as.Date("2022-09-01"), as.Date("2024-04-30")),
               breaks = "6 months", 
               date_labels = "%b %Y")

glucoseLevelsXShort <- ggplot(data = df2, aes(x = Date, y = ReadingGlucose)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "Glucose Level(mg/dL)") +  
  ggtitle("Glucose Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_date(limits = c(as.Date("2023-10-30"), as.Date("2024-04-30")),
               breaks = "6 months", 
               date_labels = "%b %Y")

a1cLevelsShort <- ggplot(data = df3, aes(x = Date, y = A1C)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "A1C Levels (%)") +  
  ggtitle("A1C Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_date(limits = c(as.Date("2022-09-01"), as.Date("2024-04-30")),
               breaks = "6 months", 
               date_labels = "%b %Y")

a1cLevelsLong <- ggplot(data = df3, aes(x = Date, y = A1C)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "A1C Levels (%)") +  
  ggtitle("A1C Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_date(limits = c(as.Date("2020-10-01"), as.Date("2024-04-30")),
               breaks = "6 months", 
               date_labels = "%b %Y")


timeline_plot_res <- timeline_plot +  
  scale_x_date(
  limits = c(as.Date("2022-08-14"), as.Date("2024-04-30")),
  breaks = "3 months", 
  date_labels = "%b %Y") +
  theme(axis.ticks.x = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))

timeline_plot_gluC <- timeline_plot +
  scale_x_date(
    limits = c(as.Date("2020-10-01"), as.Date("2024-04-30")),
    breaks = "6 months", 
    date_labels = "%b %Y") +
  theme(axis.ticks.x = element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))



rc_plot_grid <- create_plot_grid(resectionCavity, timeline_plot, '3 months',  c(as.Date("2022-08-14"), as.Date("2024-04-30")))

gl_plot_grid <- create_plot_grid(glucoseLevelsLong, timeline_plot, '6 months', c(as.Date("2020-10-01"), as.Date("2024-04-30"))) 

gls_plot_grid <- create_plot_grid(glucoseLevelsShort, timeline_plot, '6 months', c(as.Date("2022-09-01"), as.Date("2024-04-30")))

glxs_plot_grid <- create_plot_grid(glucoseLevelsXShort, timeline_plot_xs, '6 months', c(as.Date("2023-10-30"), as.Date("2024-04-30")))

a1cs_plot_grid <- create_plot_grid(a1cLevelsShort, timeline_plot, '6 months',  c(as.Date("2022-09-01"), as.Date("2024-04-30")))

a1cl_plot_grid <- create_plot_grid(a1cLevelsLong, timeline_plot, '6 months',  c(as.Date("2020-10-01"), as.Date("2024-04-30")))


# save all plots
plots <- list(resectionCavity = resectionCavity, 
              resectionCavityLog = resectionCavityLog, 
              glucoseLevelsLong = glucoseLevelsLong, 
              glucoseLevelsShort = glucoseLevelsShort, 
              glucoseLevelsXShort = glucoseLevelsXShort, 
              a1cLevelsShort = a1cLevelsShort,
              a1cLevelsLong = a1cLevelsLong
              )

plot_grids <- list(resectionCavity = rc_plot_grid, 
                   glucoseLevelsLong = gl_plot_grid, 
                   glucoseLevelsShort = gls_plot_grid, 
                   glucoseLevelsXShort = glxs_plot_grid, 
                   a1cLevelsShort = a1cs_plot_grid,
                   a1cLevelsLong = a1cl_plot_grid
)

plot_dir <- "C:/Users/hanna/Desktop/school/Honors Thesis"
if (!file.exists(plot_dir)) {
  dir.create(plot_dir)
}
for (name in names(plots)) {
  plot = plots[[name]]
  filename <- file.path(plot_dir, paste0(name, ".png")) 
  ggsave(filename, plot = plot, width = 10, height = 6, units = "in")
}

for (name in names(plot_grids)) {
  plot = plot_grids[[name]]
  filename <- file.path(plot_dir, paste0(name, "_grid.png")) 
  ggsave(filename, plot = plot, width = 10, height = 6, units = "in")
}

# statistical analysis
merged_df$MonthYear <- format(merged_df$Date, "%Y-%m")
aggregated_df <- merged_df %>%
  group_by(MonthYear) %>%
  summarise(AvgEllipResection = mean(EllipResection, na.rm = TRUE),
            AvgReadingGlucose = mean(ReadingGlucose, na.rm = TRUE),
            AvgA1C = mean(A1C, na.rm = TRUE))
#write.csv(aggregated_df, "aggregated_data.csv", row.names = FALSE)

df6 <- read.csv("aggregatedData2.csv")
df6$Date <- as.Date(mdy(df6$AvgDate))

correlation_glucose <- cor(df6$AvgEllipRes, df6$AvgGlucose, use = "complete.obs")
print(paste0("Correlation between resection cavity size and glucose levels: ", round(correlation_glucose, 2)))
cor_test_glucose <- cor.test(df6$AvgEllipRes, df6$AvgGlucose)
print(paste("p-value:", cor_test_glucose$p.value))

correlation_a1c <- cor(df6$AvgEllipRes, df6$AvgA1C2, use = "complete.obs")
print(paste0("Correlation between resection cavity size and A1C levels: ", round(correlation_a1c, 2)))
cor_test_a1c <- cor.test(df6$AvgEllipRes, df6$AvgA1C2)
print(paste("p-value:", cor_test_a1c$p.value))

#data frame for glucose - omits NA values 
df6_clean1 <- na.omit(df6[, c("AvgEllipRes", "AvgGlucose")])

#linear fit equation for glucose v resection cavity size
lm_model_glucose <- lm(AvgGlucose ~ AvgEllipRes, data = df6_clean1)
coef_glucose <- coef(lm_model_glucose)
intercept_glucose <- coef_glucose[1]
slope_glucose <- coef_glucose[2]
equation_glucose <- paste("y =", round(slope_glucose, 2), "x +", round(intercept_glucose, 2))

#correlation plot for glucose v resection cavity size
correlation_glucose_plot <- ggplot(df6_clean1, aes(x = AvgEllipRes, y = AvgGlucose)) +
  geom_point() +
  geom_line()+
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text(x = max(df6_clean1$AvgEllipRes), y = max(df6_clean1$AvgGlucose), label = equation_glucose, hjust = 1, vjust = 1, color = "blue") +  
  labs(title = "Relationship between Resection Cavity Size and Glucose Levels", x = "Resection Cavity Size (cc)", y = "Glucose Levels (mg/dL)") +
  theme_minimal()
correlation_glucose_label <- paste("Correlation =", round(correlation_glucose, 2), ", p-value =", round(cor_test_glucose$p.value, 2))
correlation_glucose_plot <- correlation_glucose_plot +
  annotate("text", x = max(df6_clean1$AvgEllipRes), y = max(df6_clean1$AvgGlucose), label = equation_glucose, hjust = 1, vjust = 1, color = "blue") +
  annotate("text", x = max(df6_clean1$AvgEllipRes), y = max(df6_clean1$AvgGlucose) - 20, label = correlation_glucose_label, hjust = 1, vjust = 1, color = "blue")

ggsave("correlation_glucose_plot.png", correlation_glucose_plot, width = 8, height = 6, units = "in", dpi = 300, bg = "white")

#data frame for a16 - omits NA values 
df6_clean2 <- na.omit(df6[, c("AvgEllipRes", "AvgA1C2")])

#linear fit equation for a1c v resection cavity size
lm_model_a1c <- lm(AvgA1C2 ~ AvgEllipRes, data = df6_clean2)
coef_a1c <- coef(lm_model_a1c)
intercept_a1c <- coef_a1c[1]
slope_a1c <- coef_a1c[2]
equation_a1c <- paste("y =", round(slope_a1c, 2), "x +", round(intercept_a1c, 2))

#correlation plot for a1c v resection cavity size
correlation_a1c_plot <- ggplot(df6_clean2, aes(x = AvgEllipRes, y = AvgA1C2)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text(x = max(df6_clean2$AvgEllipRes), y = max(df6_clean2$AvgA1C2), label = equation_a1c, hjust = 1, vjust = 1, color = "blue") +  
  labs(title = "Relationship between Resection Cavity Size and A1C Levels", x = "Resection Cavity Size (cc)", y = "A1C Levels (%)") +
  theme_minimal()
correlation_a1c_label <- paste("Correlation =", round(correlation_a1c, 2), ", p-value =", round(cor_test_a1c$p.value, 2))
correlation_a1c_plot <- correlation_a1c_plot +
  annotate("text", x = max(df6_clean2$AvgEllipRes), y = max(df6_clean2$AvgA1C2), label = equation_a1c, hjust = 1, vjust = 1, color = "blue") +
  annotate("text", x = max(df6_clean2$AvgEllipRes), y = max(df6_clean2$AvgA1C2) - 0.5, label = correlation_a1c_label, hjust = 1, vjust = 1, color = "blue")

ggsave("correlation_a1c_plot.png", correlation_a1c_plot, width = 8, height = 6, units = "in", dpi = 300, bg = "white")
