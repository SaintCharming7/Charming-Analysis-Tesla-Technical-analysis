# Charming-Analysis-Tesla-Technical-analysis
Codes used in the TSLA article: https://econhangover.blogspot.com/2019/12/hey-charming-here-this-is-first-blog.html

# First import the libraries
library(tidyverse)
library(quantmod)
library(zoo)
library(qcc)
library(gridExtra)
library(corrplot)
library(GGally)
library(moments)
library(ggplot2)

# choose TSLA data from yahoo finance
Tesla <-  getSymbols("TSLA", src = "yahoo", from = "2014-10-01", to = "2019-12-04", auto.assign = FALSE) 



# TSLA stock price evolution 

price_tesla <- Tesla %>% 
  ggplot (aes(x = index(Tesla), y = Tesla[,6]))+
  geom_line(color = "red") +
  labs(
    title = "Tesla's performance in the past years",
    x = "Date",
    y = " Price"
  ) +
  theme(plot.title = element_text(hjust = 0.5), element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "8 months")
price_tesla

# Price returns in a ln scale, pls note that the function 'log' in R is equal to Ln in mathematics

tesla_ret <- diff(log(Tesla[,6]))
tesla_ret <- tesla_ret[-1,]

graf_ret_tesla <- ggplot(tesla_ret, aes(x = index(tesla_ret), y = tesla_ret)) + geom_line(color = "slateblue3") +
  ggtitle("Tesla's returns") + xlab("Date") + ylab("Returns") +
  theme(plot.title = element_text(hjust = 0.5),element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "8 months")
graf_ret_tesla

# moving averages

MA_teslatesla_ma <- subset(Tesla, index(Tesla) >= "2014-10-01")
tesla_ma22 <- rollmean(tesla_ma[,6], 22, fill = list(NA, NULL, NA), align = "right")
tesla_ma126 <- rollmean(tesla_ma[,6], 126, fill = list(NA, NULL, NA), align = "right")
tesla_ma$ma22 <- coredata(tesla_ma22)
tesla_ma$ma126 <- coredata(tesla_ma126)
MA_tesla <- ggplot(tesla_ma, aes(x = index(tesla_ma))) + geom_line(aes(y = tesla_ma[,6], color = "Tesla")) + 
  ggtitle("MA Tesla") +
  geom_line(aes(y = tesla_ma$ma22, color = "Ma22")) +
  geom_line(aes(y = tesla_ma$ma126, color = "Ma126")) +
  xlab("Date") + ylab("Price ($)") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "8 months") +
  scale_colour_manual("Séries", values=c("Tesla"="goldenrod3", "Ma22"="firebrick4", "Ma126"="seagreen"))
MA_tesla

# Volatility
vol.tesla <- volatility(Tesla[ ,6])

graf_vol_tesla <- ggplot(vol.tesla, aes(x = as.Date(index(vol.tesla)), 
                                        y = vol.tesla))+ geom_line(color = "indianred3") +
  ggtitle("volatility Tesla") + xlab("Date") + ylab("Volatility") +
  theme(plot.title = element_text(hjust = 0.5),element_blank())+
  scale_x_date(date_labels = "%b %y", date_breaks = "9 months")
graf_vol_tesla

# Organizing everything in one place
grid.arrange(price_tesla,graf_ret_tesla,MA_tesla, graf_vol_tesla, ncol = 2, nrow = 2)

# FINITO!
