# Install necessary packages if not already installed

# install.packages("plotly")
# install.packages("dplyr") 
# install.packages("tidyr") 
# install.packages("GGally")
# install.packages("tidyverse")
# install.packages("tidyquant")

# Importing libraries
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(GGally)
library(quantmod)

### Importing data ###
data = read.csv("AAPL Yahoo.csv", header=TRUE)
date = as.Date(data$Date, format="%Y-%m-%d")


### Line plots
# Closing date // Date
ggplot(data, aes(x = Date, y = Close, group = 1)) +
  geom_line() +
  geom_line()

# Stock Prices // with Date
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Open, color = "Open", group = 1)) +
  geom_line(aes(y = High, color = "High", group = 2)) +
  geom_line(aes(y = Low, color = "Low", group = 3)) +
  geom_line(aes(y = Close, color = "Close", group = 4)) +
  scale_color_manual(values = c("Open" = "blue", "High" = "green", "Low" = "red", "Close" = "black")) +
  labs(title = "Stock Prices", x = "Date", y = "Price")

### Bar chart - Volume // Date
ggplot(data, aes(x = Date, y = Volume)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Stock Volume", x = "Date", y = "Volume")

### Scatter plot for - Open // Close data
ggplot(data, aes(x = Open, y = Close, color = factor(Open < Close))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Open vs Close Price", x = "Open Price", y = "Close Price", color = "Close > Open") +
  guides(color = guide_legend(title = "Close > Open"))


### Stock volume of close and open values
ggplot(data, aes(x = Date, y = Volume, fill = factor(Open < Close))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Stock Volume by Close > Open", x = "Date", y = "Volume") +
  guides(fill = guide_legend(title = "Close > Open"))

# create a dataframe of positive and negative changes

# create a new column with daily change
data$Change <- data$Close - data$Open

change_df <- data.frame(
  Change = c(sum(data$Change[data$Change > 0]), sum(data$Change[data$Change < 0])),
  Label = c("Positive", "Negative")
)

### Pie chart for changes
ggplot(change_df, aes(x = "", y = Change, fill = Label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_label(aes(label = paste0(Label, "\n", format(Change, big.mark = ","))), position = position_stack(vjust = 0.5))


### Box plot - for distribution of Close price
ggplot(data, aes(x = "Close", y = Close)) +
  geom_boxplot() +
  labs(title = "Distribution of Close Prices")


### Box plot of close price and frequency
ggplot(data, aes(x = Close)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Close Prices", x = "Close Price", y = "Frequency")


### Density plot of close prices
ggplot(data, aes(x = Close)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "Density Plot of Close Prices", x = "Close Price", y = "Density")


### Heatmap of close and open prices
data %>%
  select(Date, Open, Close) %>%
  gather(key = "Variable", value = "Value", -Date) %>%
  ggplot(aes(x = Variable, y = Date, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Heatmap of Open and Close Prices", x = NULL, y = "Date")


### Scatterplot Matrix of Open, High, Low and Close Prices
ggpairs(data[, c("Open", "High", "Low", "Close")], 
        columns = c("Open", "High", "Low", "Close"),
        lower = list(continuous = "smooth", method = c(method = "lm"))) +
  labs(title = "Scatterplot Matrix of Open, High, Low and Close Prices")


## )) interesting one
ggplot(data, aes(x = Date, y = Close, group = 1)) +
  geom_line(color = "blue") +
  geom_label(aes(label = format(Close, digits = 4, nsmall = 2), hjust = 0), nudge_x = 3, nudge_y = -3) +
  labs(title = "Stock Prices", x = "Date", y = "Price") +
  theme_bw()


# panel.plot with r on this data 

#my_fn <- function(data, mapping, method="loess", ...){
  #p <- ggplot(data = data, mapping = mapping) + 
   # geom_point() + 
    #geom_smooth(method=method, ...)
  #p
#}

# Default loess curve    
# ggpairs(data[, c("Open", "High", "Low", "Close")], lower = list(continuous = my_fn))
# LM curve    
# ggpairs(data[, c("Open", "High", "Low", "Close")], lower = list(continuous = wrap(my_fn, method="lm")))


### Plotly graph for line
data$Date <- as.Date(data$Date)

plot_ly(data, x = ~Date, type = "candlestick",
        open = ~Open, close = ~Close,
        high = ~High, low = ~Low) %>%
  layout(title = "Candlestick Chart")

### Scatter plot with plotly:

fig <- plot_ly(data, x = ~Close, y = ~Volume, type = "scatter", mode = "markers") %>%
  layout(title = "Scatter Plot of Volume vs. Closing Price",
         xaxis = list(title = "Closing Price"),
         yaxis = list(title = "Volume"))

fig
### Bar chart with plotly:

fig <- plot_ly(data, x = ~Date, y = ~Volume, type = "bar") %>%
  layout(title = "Volume over time",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Volume"))

fig

### Candlestick chart with plotly:

fig <- plot_ly(data, type = "candlestick", x = ~Date, open = ~Open, close = ~Close, high = ~High, low = ~Low) %>%
  layout(title = "Candlestick Chart",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Price"))

fig

### Line plot with plotly:

fig <- plot_ly(data, x = ~Date, y = ~Close, type = "scatter", mode = "lines") %>%
  layout(title = "Stock price over time",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Closing Price"))

fig


# Load data
# Load required packages

# Set start and end dates
start_date <- "2010-12-31"
end_date <- "2013-12-31"

# Download data
symbol <- "AAPL"
getSymbols(symbol, src = "yahoo", from = start_date, to = end_date)

# Calculate moving averages
SMA20 <- SMA(Cl(AAPL), n = 20)
SMA50 <- SMA(Cl(AAPL), n = 50)
SMA100 <- SMA(Cl(AAPL), n = 100)

# Create a data frame of OHLC data
df <- data.frame(Date = index(AAPL),
                 Open = as.numeric(Op(AAPL)),
                 High = as.numeric(Hi(AAPL)),
                 Low = as.numeric(Lo(AAPL)),
                 Close = as.numeric(Cl(AAPL)))

# Create a plotly chart
fig <- plot_ly(df, type = "candlestick",
               x = ~Date, open = ~Open, high = ~High,
               low = ~Low, close = ~Close, name = "AAPL")

# Add SMA20, SMA50, and SMA100 to the chart
fig <- fig %>% add_lines(x = index(SMA20), y = SMA20, 
                         name = "SMA20", line = list(color = "blue"))
fig <- fig %>% add_lines(x = index(SMA50), y = SMA50, 
                         name = "SMA50", line = list(color = "green"))
fig <- fig %>% add_lines(x = index(SMA100), y = SMA100, 
                         name = "SMA100", line = list(color = "red"))

# Add chart labels and colors
fig <- fig %>% layout(title = paste0("AAPL Candlestick Chart with SMAs"),
                      xaxis = list(title = "Date", 
                                   rangeslider = list(visible = FALSE)),
                      yaxis = list(title = "Price"),
                      legend = list(x = 0.02, y = 0.98, 
                                    bgcolor = "#E2E2E2", 
                                    bordercolor = "#FFFFFF", 
                                    borderwidth = 2))

# Display the chart
fig
