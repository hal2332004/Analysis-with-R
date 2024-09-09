library(ggplot2)
library(plotly)

#read csv
data <- read.csv("C:/Users/5530/Desktop/drive license data.csv", header=TRUE)

# format date
data$NGAY_SINH <- as.Date(data$NGAY_SINH, format="%Y-%m-%d")
data$Ngay_Hieu_Luc <- as.Date(data$ngay_hieu_luc, format="%Y-%m-%d")

# birth to age
data$age <- as.numeric(difftime(data$Ngay_Hieu_Luc, data$NGAY_SINH, units = "weeks")) / 52.25

# 6 types of drive lisence
unique(data$loai_bang)

# A1
A1_data <- subset(data, loai_bang == "A1")
mean_age_A1 <- mean(A1_data$age, na.rm = TRUE)
min_age_A1 <- min(A1_data$age, na.rm = TRUE)
max_age_A1 <- max(A1_data$age, na.rm = TRUE)

mean_age_A1
min_age_A1
max_age_A1

# A2
A2_data <- subset(data, loai_bang == "A2")
mean_age_A2 <- mean(A2_data$age, na.rm = TRUE)
min_age_A2 <- min(A2_data$age, na.rm = TRUE)
max_age_A2 <- max(A2_data$age, na.rm = TRUE)

mean_age_A2
min_age_A2
max_age_A2

# A3
A3_data <- subset(data, loai_bang == "A3")
mean_age_A3 <- mean(A3_data$age, na.rm = TRUE)
min_age_A3 <- min(A3_data$age, na.rm = TRUE)
max_age_A3 <- max(A3_data$age, na.rm = TRUE)

mean_age_A3
min_age_A3
max_age_A3

# B2
B2_data <- subset(data, loai_bang == "B2")
mean_age_B2 <- mean(B2_data$age, na.rm = TRUE)
min_age_B2 <- min(B2_data$age, na.rm = TRUE)
max_age_B2 <- max(B2_data$age, na.rm = TRUE)

mean_age_B2
min_age_B2
max_age_B2

# B1
B1_data <- subset(data, loai_bang == "B1")
mean_age_B1 <- mean(B1_data$age, na.rm = TRUE)
min_age_B1 <- min(B1_data$age, na.rm = TRUE)
max_age_B1 <- max(B1_data$age, na.rm = TRUE)

mean_age_B1
min_age_B1
max_age_B1

# C
C_data <- subset(data, loai_bang == "C")
mean_age_C <- mean(C_data$age, na.rm = TRUE)
min_age_C <- min(C_data$age, na.rm = TRUE)
max_age_C <- max(C_data$age, na.rm = TRUE)

mean_age_C
min_age_C
max_age_C

#BOX PLOT
p <- ggplot(data, aes(x = loai_bang, y = age, fill = loai_bang)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, color = "black", size = 1) +
  ggtitle("Age Distribution by License Category") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )


p_interactive <- ggplotly(p)
p_interactive


