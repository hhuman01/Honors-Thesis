#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("cowplot")
# install.packages("lubridate")
library(cowplot)
library(dplyr)
library(ggplot2)
library(lubridate)

df1 <- read.csv("tumorBurden.csv")
df1$Date <- as.Date(mdy(df1$Date))

df2 <- read.csv("glucoseLevels.csv")
df2$Date <- as.Date(mdy(df2$Date))

df3 <- read.csv("a1cLevels.csv")
df3$Date <- as.Date(mdy(df3$Date))


# merged_df <- merge(merge(df1, df2, by = "Date", all = TRUE), df3, by = "Date", all = TRUE)
# View(merged_df)

# merged_df$Date <- as.Date(mdy(merged_df$Date))

resectionCavity <- ggplot(data = df1, aes(x = Date, y = Resection)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-14")), linetype = "dashed", color = "red") +
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
  labs(x = "Date", y = "Glucose Level(mg/dL)") +  
  ggtitle("Glucose Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlim(as.Date("2020-10-01"), as.Date("2024-04-01"))  

glucoseLevelsShort <- ggplot(data = df2, aes(x = Date, y = ReadingGlucose)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-14")), linetype = "dashed", color = "red") +
  labs(x = "Date", y = "Glucose Level(mg/dL)") +  
  ggtitle("Glucose Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlim(as.Date("2022-09-01"), as.Date("2024-04-01"))  

a1cLevels <- ggplot(data = df3, aes(x = Date, y = A1C)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-14")), linetype = "dashed", color = "red") +
  labs(x = "Date", y = "A1C Levels (%)") +  
  ggtitle("A1C Levels over Time") +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlim(as.Date("2020-10-01"), as.Date("2024-04-01"))  
