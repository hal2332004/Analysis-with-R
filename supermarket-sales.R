install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)


data <- read.csv("C:/Users/5530/Desktop/supermarket_sales - Sheet1.csv", header=TRUE)

data$date <- as.Date(data$Date, format="%d/%m/%Y")

#data$date <- as.POSIXct(data$Date, format="%Y-%m-%d %H:%M:%S")

data$day <- as.integer(format(data$date, "%d"))
data$month <- as.integer(format(data$date, "%m"))
data$year <- as.integer(format(data$date, "%Y"))
data$Time <- strptime(data$Time, format="%H:%M")
data$hour <- format(data$Time, "%H")
data$hour <- as.integer(data$hour)
unique(data$hour)
#class(data$Date)

##############################################################

# Plot 0: Gender Count
ggplot(data, aes(x=Gender, fill = Gender)) + geom_bar() + ggtitle("Gender Count") + theme_minimal() +
theme(
  panel.grid.major = element_line(color = "black", size = 0.5),  # Màu và độ dày của lưới chính
  panel.grid.minor = element_line(color = "lightblue", size = 0.25),  # Màu và độ dày của lưới phụ
  axis.title = element_text(size = 14, face = "bold"),  # Kích thước và kiểu chữ của tiêu đề trục
  axis.text = element_text(size = 12),  # Kích thước chữ của nhãn trục
  plot.title = element_text(size = 16, face = "bold")  # Kích thước và kiểu chữ của tiêu đề biểu đồ
)
   
# Plot 1: Distribution of Sales by City
ggplot(data, aes(x=City, y=Total)) +
  geom_boxplot() +
  ggtitle("Distribution of Sales by City") +
  theme_minimal()

      
# Plot 2: Sales Distribution Across Different Product Lines
ggplot(data, aes(x=`Product.line`, y=Total)) +
  geom_boxplot() +
  ggtitle("Sales Distribution Across Different Product Lines") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

library(ggplot2)
library(plotly)
p <- ggplot(data, aes(x = `Product.line`, y = Total, fill = `Product.line`)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, color = "black", size = 1) +
  ggtitle("Sales Distribution Across Different Product Lines") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, angle = 45, hjust = 1)
  )

p_interactive <- ggplotly(p)
p_interactive


# Plot 3: Average Rating by Customer Type and Gender
ggplot(data, aes(x=`Customer.type`, y=Rating, fill=Gender)) +
  geom_bar(stat="summary", fun="mean", position="dodge") +
  ggtitle("Average Rating by Customer Type and Gender") +
  theme_minimal()

# Plot 4: Payment Method Distribution
ggplot(data, aes(x=Payment, fill = Payment)) +
  geom_bar() +
  ggtitle("Payment Method Distribution") +
  theme_minimal()

# Plot 5: Ratings by Branch
######### CÁCH 1 2 ###########
ggplot(data, aes(x = Branch, y = Rating, fill= Branch)) +
  geom_boxplot() +  # Tạo biểu đồ hộp
  ggtitle("Ratings by Branch") +
  theme_minimal() 


install.packages("plotly")
library(ggplot2)
library(plotly)

p <- ggplot(data, aes(x = Branch, y = Rating, fill = Branch)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, color = "black", size = 1) +
  ggtitle("Ratings by Branch") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )

p_interactive <- ggplotly(p) # Chuyển đổi ggplot thành biểu đồ tương tác với plotly
p_interactive








