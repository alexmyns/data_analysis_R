# install.packages("quantmod") # for stock price data
# install.packages("pageviews") # for Wikipedia pageviews
library(quantmod)
library(pageviews)
library(dplyr)

# Downloading the stock price data for Nokia from Yahoo Finance using the getSymbols function
# from the quantmod package. Nokia stock price data:

start_date <- as.Date("2020-07-15")
end_date <- as.Date("2022-07-15")
nok_yahoo = getSymbols("NOK", src = "yahoo", from = start_date, to = end_date)

# Downloading the Wikipedia pageviews data for Nokia using the article_pageviews function 
# from the pageviews package. The pageviews data:

start_date <- as.Date("2020-07-15")
end_date <- as.Date("2022-07-15")
nokia_pageviews <- article_pageviews("Nokia", project = "en.wikipedia", start = start_date, end = end_date)

# convert date index to a column in both datasets
nok_stock <- data.frame(Date = index(NOK), stock=NOK$NOK.Close)
row.names(nok_stock) <- 1:nrow(nok_stock)

nok_pageviews <- data.frame(Date = as.Date(nokia_pageviews$date),Views=nokia_pageviews$views)

# merge the datasets based on the date
nok_merged <- merge(nok_stock, nok_pageviews, by = "Date", all = TRUE)
nok_merged <- na.omit(nok_merged)

# plot the stock price and pageviews data

plot(nok_merged$Date, nok_merged$NOK.Close, type = "l", xlab = "Date", ylab = "Stock Price")
par(new = TRUE)
plot(nok_merged$Date, nok_merged$views, type = "l", 
     main = "Nokia Wikipedia Page Views", xlab="Date", ylab="Number of page views", col="blue")
legend("topright", legend = c("Stock Price", "Pageviews"), col = c("black", "blue"), lty = c(1, 1))

# Line plot to show the trend of stock prices over time:

ggplot(data = nok_merged, aes(x = nok_merged$Date, y = nok_merged$NOK.Close)) +
  labs( main = "Nokia Stock over time", 
        x="Date", 
        y="Stock price") +
  geom_line()

# Histogram to visualize the distribution of views:

ggplot(data = nok_merged, aes(x = nok_merged$Views)) +
  geom_histogram(binwidth = 1000, color = "white", fill = "blue") +
  labs(x = "Number of Views", y = "Frequency", 
       title = "Distribution of Views on the Page")

# Scatter plot to see if there's any relationship between the stock price and views:

ggplot(data = nok_merged, aes(x = nok_merged$NOK.Close, y = nok_merged$Views)) +
  geom_point() +
  labs(x = "Closing Stock Price", y = "Number of Views", 
       title = "Relationship between Stock Price and Views")

# Boxplot to show the distribution of stock prices by month:

ggplot(data = nok_merged, aes(x = months(Date), y = NOK.Close)) +
  geom_boxplot() +
  labs(x = "Month", y = "Closing Stock Price", 
       title = "Distribution of Stock Prices by Month")


