library(cowplot)
library(dplyr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(mgcv)
library(gridExtra)

# data manipulation
df1 <- read.csv("tumorBurden.csv")
df1$Date <- as.Date(mdy(df1$Date))

df2 <- read.csv("glucoseLevels.csv")
df2$Date <- as.Date(mdy(df2$Date))

df3 <- read.csv("a1cLevels.csv")
df3$Date <- as.Date(mdy(df3$Date))


merged_df <- merge(merge(df1, df2, by = "Date", all = TRUE), df3, by = "Date", all = TRUE)

# colors <- c("red", "darkblue", "blue", "lightblue", "green", "lightgreen", "darkgreen", "purple", "orange", "yellow")
# meanings <- c("GBM Diagnosis", "Metformin Dose Change (1700mg to 850mg)", "Metformin Dose Change (850mg to 1700mg)", "Glipizide Dose Change (5 mg to 0 mg)", 
#               "Start of Chemotherapy and Radiation", "Start of Avastin", "End of Radiation", "Deferment of Chemotherapy/Avastin due to abscess",
#               "End of Chemotherapy", "End of Avastin")

# plots
resectionCavity <- ggplot(data = df1, aes(x = Date, y = Resection)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-14")), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2024-01-05")), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = as.numeric(as.Date("2024-02-01")), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), linetype = "dashed", color = "lightblue") +
  geom_vline(xintercept = as.numeric(as.Date("2022-10-10")), linetype = "dashed", color = "green") +
  geom_vline(xintercept = as.numeric(as.Date("2023-02-02")), linetype = "dashed", color = "lightgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2022-11-18")), linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-02")), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-29")), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2023-05-27")), linetype = "dashed", color = "orange") +
  geom_vline(xintercept = as.numeric(as.Date("2023-08-10")), linetype = "dashed", color = "yellow") +
  geom_text(aes(label = FLAIR), vjust = -0.5, hjust = 0.75, size = 5) +
  labs(x = "Date", y = "Resection Cavity Size (cc)") +  
  ggtitle("Resection Cavity Size over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlim(as.Date("2022-09-14"), as.Date("2023-10-30"))

resectionCavityLog <- ggplot(data = df1, aes(x = Date, y = log)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-14")), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2024-01-05")), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = as.numeric(as.Date("2024-02-01")), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), linetype = "dashed", color = "lightblue") +
  geom_vline(xintercept = as.numeric(as.Date("2022-10-10")), linetype = "dashed", color = "green") +
  geom_vline(xintercept = as.numeric(as.Date("2023-02-02")), linetype = "dashed", color = "lightgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2022-11-18")), linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-02")), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-29")), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2023-05-27")), linetype = "dashed", color = "orange") +
  geom_vline(xintercept = as.numeric(as.Date("2023-08-10")), linetype = "dashed", color = "yellow") +
  geom_text(aes(label = FLAIR), vjust = -0.5, hjust = 0.75, size = 5) +
  labs(x = "Date", y = "log10 Resection Cavity Size (cc)") +  
  ggtitle("Resection Cavity Size over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlim(as.Date("2022-09-14"), as.Date("2023-10-30"))

glucoseLevelsLong <- ggplot(data = df2, aes(x = Date, y = ReadingGlucose)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-14")), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2024-01-05")), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = as.numeric(as.Date("2024-02-01")), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), linetype = "dashed", color = "lightblue") +
  geom_vline(xintercept = as.numeric(as.Date("2022-10-10")), linetype = "dashed", color = "green") +
  geom_vline(xintercept = as.numeric(as.Date("2023-02-02")), linetype = "dashed", color = "lightgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2022-11-18")), linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-02")), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-29")), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2023-05-27")), linetype = "dashed", color = "orange") +
  geom_vline(xintercept = as.numeric(as.Date("2023-08-10")), linetype = "dashed", color = "yellow") +
  labs(x = "Date", y = "Glucose Level(mg/dL)") +  
  ggtitle("Glucose Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlim(as.Date("2020-10-01"), as.Date("2024-04-01"))  

glucoseLevelsShort <- ggplot(data = df2, aes(x = Date, y = ReadingGlucose)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-14")), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2024-01-05")), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = as.numeric(as.Date("2024-02-01")), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), linetype = "dashed", color = "lightblue") +
  geom_vline(xintercept = as.numeric(as.Date("2022-10-10")), linetype = "dashed", color = "green") +
  geom_vline(xintercept = as.numeric(as.Date("2023-02-02")), linetype = "dashed", color = "lightgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2022-11-18")), linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-02")), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-29")), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2023-05-27")), linetype = "dashed", color = "orange") +
  geom_vline(xintercept = as.numeric(as.Date("2023-08-10")), linetype = "dashed", color = "yellow") +
  labs(x = "Date", y = "Glucose Level(mg/dL)") +  
  ggtitle("Glucose Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlim(as.Date("2022-09-01"), as.Date("2024-04-01"))  

glucoseLevelsXShort <- ggplot(data = df2, aes(x = Date, y = ReadingGlucose)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-14")), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2024-01-05")), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = as.numeric(as.Date("2024-02-01")), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), linetype = "dashed", color = "lightblue") +
  geom_vline(xintercept = as.numeric(as.Date("2022-10-10")), linetype = "dashed", color = "green") +
  geom_vline(xintercept = as.numeric(as.Date("2023-02-02")), linetype = "dashed", color = "lightgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2022-11-18")), linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-02")), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-29")), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2023-05-27")), linetype = "dashed", color = "orange") +
  geom_vline(xintercept = as.numeric(as.Date("2023-08-10")), linetype = "dashed", color = "yellow") +
  labs(x = "Date", y = "Glucose Level(mg/dL)") +  
  ggtitle("Glucose Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlim(as.Date("2023-10-30"), as.Date("2024-04-01")) 

a1cLevelsShort <- ggplot(data = df3, aes(x = Date, y = A1C)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-14")), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2024-01-05")), linetype = "dashed", color = "darkblue") +
  geom_vline(xintercept = as.numeric(as.Date("2024-02-01")), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2023-11-30")), linetype = "dashed", color = "lightblue") +
  geom_vline(xintercept = as.numeric(as.Date("2022-10-10")), linetype = "dashed", color = "green") +
  geom_vline(xintercept = as.numeric(as.Date("2023-02-02")), linetype = "dashed", color = "lightgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2022-11-18")), linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-02")), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2023-03-29")), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2023-05-27")), linetype = "dashed", color = "orange") +
  geom_vline(xintercept = as.numeric(as.Date("2023-08-10")), linetype = "dashed", color = "yellow") +
  labs(x = "Date", y = "A1C Levels (%)") +  
  ggtitle("A1C Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlim(as.Date("2022-09-01"), as.Date("2024-04-01"))  

a1cLevelsLong <- ggplot(data = df3, aes(x = Date, y = A1C)) +
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-09-14")), color = "GBM Diagnosis"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2024-01-05")), color = "Metformin Dose Change (1700mg to 850mg)"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2024-02-01")), color = "Metformin Dose Change (850mg to 1700mg)"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-11-30")), color = "Glipizide Dose Change (5 mg to 0 mg)"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-10-10")), color = "Start of Chemotherapy and Radiation"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-02-02")), color = "Start of Avastin"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-11-18")), color = "End of Radiation"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-03-02")), color = "Deferment of Chemotherapy/Avastin"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-03-29")), color = "Deferment of Chemotherapy/Avastin"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-05-27")), color = "End of Chemotherapy"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-08-10")), color = "End of Avastin"), linetype = "dashed") + 
   labs(x = "Date", y = "A1C Levels (%)") +  
  ggtitle("A1C Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("GBM Diagnosis" = "red", 
                                "Metformin Dose Change (1700mg to 850mg)" = "darkblue", 
                                "Metformin Dose Change (850mg to 1700mg)" = "blue", 
                                "Glipizide Dose Change (5 mg to 0 mg)" = "lightblue", 
                                "Start of Chemotherapy and Radiation" = "green", 
                                "Start of Avastin" = "lightgreen", 
                                "End of Radiation" = "darkgreen", 
                                "Deferment of Chemotherapy/Avastin" = "purple", 
                                "End of Chemotherapy" = "orange", 
                                "End of Avastin" = "yellow"),
                     breaks = c("GBM Diagnosis",
                                "Start of Chemotherapy and Radiation",
                                "End of Radiation",
                                "Start of Avastin",
                                "Deferment of Chemotherapy/Avastin",
                                "End of Chemotherapy",
                                "End of Avastin",
                                "Glipizide Dose Change (5 mg to 0 mg)",
                                "Metformin Dose Change (1700mg to 850mg)", 
                                "Metformin Dose Change (850mg to 1700mg)")) +
  guides(color = guide_legend(title = NULL, override.aes = list(shape = 15))) +
  xlim(as.Date("2020-10-01"), as.Date("2024-04-01"))

# save all plots
plots <- list(resectionCavity = resectionCavity, 
              resectionCavityLog = resectionCavityLog, 
              glucoseLevelsLong = glucoseLevelsLong, 
              glucoseLevelsShort = glucoseLevelsShort, 
              glucoseLevelsXShort = glucoseLevelsXShort, 
              a1cLevelsShort = a1cLevelsShort
              )

for (plot_name in names(plots)) {
  filename <- paste0("plot_", plot_name, ".png")
  ggsave(filename, plot = plots[[plot_name]], path = ".", width = 8, height = 6) 
}

ggsave(filename, plot = a1cLevelsLong, path = ".", width = 16, height = 6)

#arrange all plots on one image - not working

# plot_a1cLevelsLong <- arrangeGrob(a1cLevelsLong, widths = c(5, 1))
# plot_grid <- plot_grid(
#   resectionCavity, 
#   resectionCavityLog, 
#   glucoseLevelsLong, 
#   glucoseLevelsShort, 
#   glucoseLevelsXShort, 
#   a1cLevelsShort,
#   plot_a1cLevelsLong,  # Use the arranged plot
#   ncol = 2
# )
# 
# ggsave("plot_allGraphs.png", plot = plot_grid, path = ".", width = 12, height = 10)

# statistical analysis
merged_df$MonthYear <- format(merged_df$Date, "%Y-%m")
aggregated_df <- merged_df %>%
  group_by(MonthYear) %>%
  summarise(AvgResection = mean(Resection, na.rm = TRUE),
            AvgReadingGlucose = mean(ReadingGlucose, na.rm = TRUE),
            AvgA1C = mean(A1C, na.rm = TRUE))

correlation_glucose <- cor(aggregated_df$AvgResection, aggregated_df$AvgReadingGlucose, use = "complete.obs")
print(paste0("Correlation between resection cavity size and glucose levels: ", round(correlation_glucose, 2)))
cor_test_glucose <- cor.test(aggregated_df$AvgResection, aggregated_df$AvgReadingGlucose)
print(paste("p-value:", cor_test_glucose$p.value))

correlation_a1c <- cor(aggregated_df$AvgResection, aggregated_df$AvgA1C, use = "complete.obs")
print(paste0("Correlation between resection cavity size and A1C levels: ", round(correlation_a1c, 2)))
cor_test_a1c <- cor.test(aggregated_df$AvgResection, aggregated_df$AvgA1C)
print(paste("p-value:", cor_test_a1c$p.value))
