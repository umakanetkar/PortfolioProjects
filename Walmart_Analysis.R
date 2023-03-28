#Installing Packages
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(readxl)
library(scales)
library(maps)
#install.packages("plotrix")
library(plotrix)
library(stargazer)
#install.packages('tm')
library(tm)
#install.packages('tidytext')
library(tidytext)
library(ggplot2)
library(psych)
library(plotly)
library(lubridate)
library(fastDummies)
library(recipes)
library(Metrics)
library(tidyverse) 
library(doBy)
library(foreign)
library(knitr)
library(lmtest)
library(readstata13)
library(sandwich)
library(stargazer)
library(factoextra)

#####
#Loading the dataset and its basic understanding
#####
data<-read_csv("Walmart.csv")
data

#column names
names(data)
#summary
summary(data)

#checking null values
sum(is.na(data))
colSums(is.na(data))

#checking date format
format_table <- parse_guess(data$Date)
unique(format_table) #has differnt formats



#convert to one format
data$Date <- dmy(data$Date)

# Extract day, month and year into separate columns
data$Day <- format(data$Date, "%d")
data$Month <- format(data$Date, "%m")
data$Year <- format(data$Date, "%Y")
data$Week <- week(data$Date)

#EDA
length(unique(data$Store))
length(unique(data$Unemployment))

min(data$Date)
max(data$Date)
#We have data from Feb 2010 to October 2012

#unique stores
unique(data$Store) # total of 45 stores
#unique 
unique(data$Holiday_Flag)
#temp range
range(data$Temperature)
boxplot(data$Temperature, main = "Temperature Range", ylab = "Temperature")

###Store Data
Store_data<-data %>% dplyr::group_by(Store) %>% 
  dplyr::summarise(Total_Sales=sum(Weekly_Sales), 
                   Average_Unemployment = mean(Unemployment))
###yearlyData
year_data<-data %>% dplyr::group_by(Year) %>% 
  dplyr::summarise(Total_Sales=sum(Weekly_Sales), 
                   Average_Unemployment = mean(Unemployment),
                   total_holidays = sum(Holiday_Flag))

# barplot for yearly sales
barplot(height=year_data$Total_Sales, names=year_data$Year , 
        density=c(5,10,20) , angle=c(0,45,90) , col="brown" ,
        ylim = c(0, 2500000000), ylab="Total Sales",
        xlab = "Year",main = 'Total Sales in every year')
lines(year_data$Total_Sales, lwd = 3, col = 'red', pch = 21, bg = 'white')
#The total sales across various years. 2012 has less sales because it has data till october only.

###Month Data
month_data<-data %>% dplyr::group_by(Month, Store) %>% 
  dplyr::summarise(Total_Sales=sum(Weekly_Sales), 
                   Average_Unemployment = mean(Unemployment),
                   total_holidays = first(Holiday_Flag))

# Create a 3D pie chart using plotly for Sales Across every month
fig <- plot_ly(month_data, labels = ~Month, values = ~Total_Sales, type = "pie",
               textinfo = "label+percent", 
               hoverinfo = "text",
               hovertext = paste(month_data$Month, ": ", month_data$Total_Sales),
               hole = 0.4)
# Define a custom color palette
custom_colors <- c("#FFC300", "#FF5733", "#C70039", "#900C3F", "#581845")

# Update the pie chart layout
fig <- fig %>% layout(title = "Sales on each Month",
                      scene = list(aspectmode = "cube"),
                      legend = list(x = -0.2, y = 0.5, orientation = "v"),
                      margin = list(l = 100, r = 100, b = 100, t = 100),
                      colorway = custom_colors)

# Display the chart
fig

###year_Month data
year_month_data<-data %>% dplyr::group_by(Year,Month) %>% 
  dplyr::summarise(Total_Sales=sum(Weekly_Sales), 
                   Average_Unemployment = mean(Unemployment),
                   total_holidays = sum(Holiday_Flag))

# convert month to a factor so it's plotted in the correct order
year_month_data$month <- factor(year_month_data$Month, levels = c("1", "2", "3","4","5","6","7","8","9"
                                                                  ,"10","11","12"))

# plot monthly sales for different years
#Monthly sales trend across different years
ggplot(year_month_data, aes(x = Month, y = Total_Sales, group = Year, 
                            color = factor(Year))) +
  geom_line(size = 1.50) + geom_point(size = 3.0) + 
  labs(title = "Monthly Sales by Year", x = "Month", y = "Sales", color = "Year")+ 
  scale_y_continuous(labels = comma)+ theme(panel.grid.major = element_blank(), 
                                            panel.grid.minor = element_blank())

# Unemployment for different years
ggplot(year_month_data, aes(x = Month, y = Average_Unemployment, group = Year, 
                            color = factor(Year))) +
  geom_line(size = 1.50) + geom_point(size = 3.0) + 
  labs(title = "Average_Unemployment by Year", x = "Month", y = "Average_Unemployment", color = "Year")+ 
  scale_y_continuous(labels = comma)+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Month_WEek_Data
week_month_data<-data %>% dplyr::group_by(Month,Week) %>% 
  dplyr::summarise(Total_Sales=sum(Weekly_Sales), 
                   Average_Unemployment = mean(Unemployment),
                   holiday_flag = first(Holiday_Flag))
# plot monthly sales for different years
ggplot(week_month_data, aes(x = Week, y = Total_Sales, group = Month, 
                            color = factor(Month))) +
  geom_line(size = 1.50) + geom_point(size = 3.0) + 
  labs(title = "Weekly Sales by Month", x = "Week", y = "Sales", color = "Year")+ 
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# create a plotly object
p <- plot_ly(week_month_data, x = ~Week, y = ~Total_Sales, color = ~Month, colors = "Set1",
             type = "scatter", mode = "lines+markers", line = list(width = 2),
             marker = list(size = 5, opacity = 0.7), hovertemplate = "Sales: %{y:,.0f}<br>Week: %{x}<br>")

# customize the plot layout
p <- p %>% layout(title = "Weekly Sales by Month", xaxis = list(title = "Week"), yaxis = list(title = "Sales"),
                  legend = list(title = "Month"), hoverlabel = list(font = list(size = 12)))

# display the plot
p


#Week Vs Holiday
# create a plotly object
p <- plot_ly(week_month_data, x = ~Week, y = ~holiday_flag, color = ~Month, colors = "Set1",
             type = "scatter", mode = "lines+markers", line = list(width = 2),
             marker = list(size = 5, opacity = 0.7), hovertemplate = "Sales: %{y:,.0f}<br>Week: %{x}<br>Month: %{color}")

# customize the plot layout
p <- p %>% layout(title = "Weekly holidays by Month", xaxis = list(title = "Week"), yaxis = list(title = "Sales"),
                  legend = list(title = "Month"), hoverlabel = list(font = list(size = 12)))

# display the plot
p

# plot monthly sales for different years
ggplot(Store_data, aes(x = Store, y = Total_Sales)) +
  geom_line(size = 1.50, color = 'red') + geom_point(size = 3.0) + 
  labs(title = "Weekly Sales acorss stores", x = "Stores", y = "Sales")+ 
  scale_y_continuous(labels = comma)+ theme(panel.grid.major = element_blank(), 
                                            panel.grid.minor = element_blank())

#Month_WEek_Data
UE_data<-data %>% dplyr::group_by(Unemployment) %>% 
  dplyr::summarise(Total_Sales=sum(Weekly_Sales), 
                   holiday_flag = first(Holiday_Flag))


# plot monthly sales for different years
ggplot(UE_data, aes(x = Unemployment, y = Total_Sales)) +
  geom_line(size = 0.7, color = 'purple') + geom_point(size = 2.0) + 
  labs(title = "Sales Vs Unemployment Trend", x = "Unemployment Rate", y = "Sales")+ 
  scale_y_continuous(labels = comma)+ theme(panel.grid.major = element_blank(), 
                                            panel.grid.minor = element_blank())


# Create a box plot
ggplot(data, aes(x = as_factor(Holiday_Flag), y = Weekly_Sales)) +
  geom_boxplot() +
  xlab("Holiday Flag") +
  ylab("Total Sales") +
  ggtitle("Box Plot of Total Sales by Holiday Flag")

ggplot(data, aes(Temperature))+
  geom_histogram(bins=100, color = 'black')+
  labs(title = 'Temperature Distribution',
       y='Weekly sales',
       x='Temperature')

ggplot(data, aes(Temperature, Weekly_Sales))+
  geom_point(alpha =1/10, color = 'red')+
  labs(title = 'Temperature against Weekly Sales',
       y='Weekly sales',
       x='Temperature')

#creating temp categories
data$Temp_Category <- ifelse(data$Temperature < 50, "cold", 
                             ifelse(data$Temperature < 78, "moderate", "hot"))

#Creating New Column Holiday
data$Holiday <- ifelse((data$Date == "2010-02-12" )| (data$Date == "2011-02-11") | (data$Date == "2012-02-10"), "Super Bowl",
                       ifelse((data$Date == max(data$Date[data$Date < ymd("2010-02-12")])) |
                                (data$Date == max(data$Date[data$Date < ymd("2011-02-11")])) |
                                (data$Date == max(data$Date[data$Date < ymd("2012-02-10")])), "Week Before Super Bowl",
                      ifelse((data$Date == min(data$Date[data$Date > ymd("2010-02-12")])) | 
                                       (data$Date == min(data$Date[data$Date > ymd("2012-02-10")])) , "Week After Super Bowl",
                      ifelse((data$Date == "2010-09-10" )| (data$Date == "2011-09-09") | (data$Date == "2012-09-07"), "Labour Day", 
                      ifelse((data$Date == max(data$Date[data$Date < ymd("2010-09-10")])) | 
                                                     (data$Date == max(data$Date[data$Date < ymd("2011-09-09")])) |
                                                     (data$Date == max(data$Date[data$Date < ymd("2012-09-07")])) , "Week Before Labour Day",
                      ifelse((data$Date == min(data$Date[data$Date > ymd("2010-09-10")])) |
                                                    (data$Date == min(data$Date[data$Date > ymd("2012-09-07")])) , "Week After Labour Day",
                      ifelse((data$Date == "2010-11-26") | (data$Date == "2011-11-25") | (data$Date == "2012-11-23"), "Thanksgiving",
                      ifelse((data$Date == max(data$Date[data$Date < ymd("2010-11-26")]))|
                                                    (data$Date == max(data$Date[data$Date < ymd("2011-11-25")])) , "Week Before Thanksgiving",
                      ifelse(data$Date == min(data$Date[data$Date > ymd("2010-11-26")]), "Week After Thanksgiving",
                      ifelse((data$Date == "2010-12-31")|(data$Date == "2011-12-30") | (data$Date == "2012-12-28"), "Christmas",
                      ifelse((data$Date == max(data$Date[data$Date < ymd("2010-12-31")])) |
                                 (data$Date == max(data$Date[data$Date < ymd("2011-12-30")])), "Week Before Christmas",
                      ifelse(data$Date == min(data$Date[data$Date > ymd("2010-12-31")]), "Week After Christmas",
                      ifelse(data$Date == min(data$Date[data$Date > ymd("2011-02-12")]), "Week After Super Bowl",
                      ifelse(data$Date == min(data$Date[data$Date > ymd("2011-09-10")]), "Week After Labour Day",
                      ifelse(data$Date == min(data$Date[data$Date > ymd("2011-11-26")]), "Week After Thanksgiving",
                      ifelse(data$Date == min(data$Date[data$Date > ymd("2011-12-31")]), "Week After Christmas", "Normal Day"))))))))))))))))

table(data$Holiday)

mydata <- data %>%
  group_by(Date, Holiday) %>%
  summarize(Total_Sales = sum(Weekly_Sales))

mydata$Holiday <- factor(mydata$Holiday, levels = c("Week Before Super Bowl", "Super Bowl", "Week After Super Bowl", "Normal Day", "Week Before Labour Day", "Labour Day", "Week After Labour Day", "Week Before Thanksgiving", "Thanksgiving", "Week After Thanksgiving", "Week Before Christmas", "Christmas", "Week After Christmas"))

shapes <- c(15, 17, 16, 1, 6, 8, 7, 11, 18, 10, 2, 3, 4) # Set shapes for all categories

ggplot(mydata, aes(x = Date, y = Total_Sales, color = Holiday, shape = Holiday)) +
  geom_point(aes(color = Holiday, 
                 shape = Holiday),size = 3) +
  geom_line(aes(group = 1), size = 1, color = 'black') +
  labs(x = "Date", y = "Total Sales", title = "Total Sales by Date and Holiday") +
  theme_minimal() +
  scale_shape_manual(values = shapes) # Manually set shapes for all categories

data %>%
  ggplot(aes(Fuel_Price, Weekly_Sales)) +
  geom_point() +
  labs(title = "Holiday Sales vs Temperature",
       y = "Weekly Sales",
       x = "Temperature") +
  theme_bw()

ggplot(data, aes(x = Fuel_Price, y = Weekly_Sales)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Fuel Price", y = "Weekly Sales") +
  ggtitle("Impact of Fuel Price on Weekly Sales")


#holiday sales
holiday_sales <- data %>% group_by(Holiday, Store) %>%
  summarize(Total_Sales = sum(Weekly_Sales)) %>%
  pivot_wider(names_from = Holiday, values_from = Total_Sales)

data %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date) %>%
  summarize(Weekly_Sales = mean(Weekly_Sales, na.rm = TRUE)) %>%
  ggplot(aes(x = Date, y = Weekly_Sales)) +
  geom_line(color = "grey") +
  labs(title = "Sales",
       subtitle = "Sales peak around Thanksgiving and Christmas",
       x = "Date",
       y = "Weekly sales") +
  theme_bw()

ggplot(data, aes(y = Holiday, x = Weekly_Sales)) +
  geom_boxplot() +
  labs(y = "Holiday", x = "Weekly Sales", title = "Weekly sales")

#### mean sales by holiday
hs <- data %>%
  group_by(Holiday) %>%
  summarize(Avg_Sales = mean(Weekly_Sales))

#print plot in order of holidays
hs$Holiday <- factor(hs$Holiday, levels = c("Week Before Super Bowl", "Super Bowl", "Week After Super Bowl",
                                            "Week Before Labour Day", "Labour Day", "Week After Labour Day",
                                            "Week Before Thanksgiving", "Thanksgiving", "Week After Thanksgiving",
                                            "Week Before Christmas", "Christmas", "Week After Christmas","Normal Day"))
#plot
ggplot(hs, aes(y = Holiday, x = Avg_Sales)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(title = "Average Sales by type of day", x = "Average Sales", y = "Event") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  geom_text(aes(label = scales::dollar_format()(Avg_Sales)), hjust = 1.1, na.rm = TRUE)

#weekly sales
data %>% ggplot(aes(Weekly_Sales, reorder(as_factor(Store), Weekly_Sales)))+
  geom_boxplot()+   labs(title = 'Difference in weekly sales',
       x='Weekly sales', y='Store') + scale_x_continuous()


weekly_summary <- data %>%
  group_by(Store) %>%
  summarize(mean_weekly_sales = mean(Weekly_Sales))

ggplot(weekly_summary, aes(x = mean_weekly_sales, y = reorder(factor(Store), mean_weekly_sales))) +
  geom_bar(stat = "identity", fill='purple', width = 0.3) +
  labs(x = "Weekly Sales Amount", y = "Stores", title = "Average weekly sales per store") +
  theme_minimal()

#monthly sales
weekly_sales = data %>% group_by(Date) %>% summarize(sum(Weekly_Sales))

monthly_sales = data %>% group_by(Year,Month,Store) %>% summarize(total_sales = sum(Weekly_Sales),
                                                                  avg_unemployment = mean(Unemployment), avg_CPI = mean(CPI),
                                                                  avg_fuel = mean(Fuel_Price),
                                                                  holidays = sum(Holiday_Flag))

#sales per year monthly representation
ggplot(monthly_sales, aes(x = paste0(Year, "-", Month), y = total_sales)) +
  geom_line(aes(color = factor(Year)), size = 1.5, alpha = 0.7) +
  labs(title = "Total Sales per Month by Year",
       x = "Month", y = "Total Sales",
       color = "Year") +
  scale_x_discrete(labels = function(x) format(as.Date(paste0(x, "-01"), "%Y-%m-%d"), "%b"),
                   expand = c(0.01, 0)) +
  scale_y_continuous(labels = scales::comma,
                     expand = c(0, 0.01)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#sales summary by year
yearly_sales_by_store <- data %>% 
  group_by(Year, Store) %>% 
  summarize(Total_Sales = sum(Weekly_Sales)) %>%
  pivot_wider(names_from = Year, values_from = Total_Sales)

###########
#store sales analysis
###########

# Group the data by Store and sum the sales for each store
sales_by_store <- aggregate(data$Weekly_Sales, by=list(Store=data$Store), sum)
names(sales_by_store)[2] <- "sales"
#calc percentage
sales_by_store$percent_sales <- 100 * sales_by_store$sales / sum(sales_by_store$sales)

ggplot(sales_by_store, aes(x=sales, y=reorder(Store, sales))) +
  geom_bar(stat="identity", fill = "#FDB813", color = "yellow", width = 0.8) +
  xlab("Total Sales") +
  ylab("Store") +
  ggtitle("Total Sales by Store") + theme_minimal() +
  geom_text(aes(label=paste0(round(percent_sales, 1),"%")), 
            hjust=-0.5, size=4, fontface="bold")+
  geom_text(aes(label = paste0("$", round(sales/1000, 1), "K")), vjust = 0.5, hjust = 1.0,color = "black", 
            size = 3, fontface = "bold") +scale_x_continuous(labels = scales::comma_format())



#####
# growth
#####
data_yearly <- data %>%
  group_by(Year) %>%
  summarise(Total_Sales = sum(Weekly_Sales))

# Calculate year-on-year growth rate
data_yearly_growth <- data_yearly %>%
  mutate(Growth_Rate = Total_Sales / lag(Total_Sales, na.rm = TRUE) - 1)

# yearly growth 
ggplot(data_yearly, aes(x = Year, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Year", y = "Total Sales", title = "Yearly Sales Performance")+
  scale_y_continuous(labels = scales::comma)

#### quarterly
data_quarterly <- data %>%
  group_by(Year , Quarter = quarter(Date)) %>%
  summarise(Total_Sales = sum(Weekly_Sales))

# quarterly performance
ggplot(data_quarterly, aes(x = Quarter, y = Total_Sales, fill = factor(Year))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Quarter", y = "Total Sales", title = "Quarterly Sales by Year") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  guides(fill = guide_legend(title = "Year", title.position = "top", ncol = 3))+
  scale_y_continuous(labels = scales::comma)

## quarter 1 sales growth
data_q1 <- data_quarterly %>% filter(Quarter == 1)
# calculate percentage change in sales for Q1
data_q1$Percent_growth <- 100*(data_q1$Total_Sales - lag(data_q1$Total_Sales))/lag(data_q1$Total_Sales)
data_q1 <- data_q1 %>% mutate(Percent_growth = replace_na(Percent_growth, 0))

data_q1_11_12 <- data_q1 %>% filter(Year %in% c(2011, 2012))

# plot the percentage increase in sales for Q1 of 2011 and 2012
ggplot(data_q1_11_12, aes(x = Year, y = Percent_growth, fill = Year)) +
  geom_col() +
  scale_fill_manual(values = c("#FF7F00", "#1F78B4")) +
  labs(x = "Year", y = "% Increase in Sales", title = "Percentage Increase in Q1 Sales  for 2011-2012") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))+
  scale_y_continuous(labels = scales::comma)

## quarter 2 sales growth
data_q2 <- data_quarterly %>% filter(Quarter == 2)
# calculate percentage change in sales for Q1
data_q2$Percent_growth <- 100*(data_q2$Total_Sales - lag(data_q2$Total_Sales))/lag(data_q2$Total_Sales)
data_q2 <- data_q2 %>% mutate(Percent_growth = replace_na(Percent_growth, 0))

data_q2_11_12 <- data_q2 %>% filter(Year %in% c(2011, 2012))

# plot the percentage increase in sales for Q1 of 2011 and 2012
ggplot(data_q2_11_12, aes(x = Year, y = Percent_growth, fill = Year)) +
  geom_col() +
  scale_fill_manual(values = c("#FF7F00", "#1F78B4")) +
  labs(x = "Year", y = "% Increase in Sales", title = "Percentage Increase in Q2 Sales for 2011-2012") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))+
  scale_y_continuous(labels = scales::comma)

## quarter 3 sales growth
data_q3 <- data_quarterly %>% filter(Quarter == 3)
# calculate percentage change in sales for Q1
data_q3$Percent_growth <- 100*(data_q3$Total_Sales - lag(data_q3$Total_Sales))/lag(data_q3$Total_Sales)
data_q3 <- data_q3 %>%  mutate(Percent_growth = replace_na(Percent_growth, 0))

data_q3_11_12 <- data_q3 %>% filter(Year %in% c(2011, 2012))

# plot the percentage increase in sales for Q1 of 2011 and 2012
ggplot(data_q3_11_12, aes(x = Year, y = Percent_growth, fill = Year)) +
  geom_col() +
  scale_fill_manual(values = c("#FF7F00", "#1F78B4")) +
  labs(x = "Year", y = "% Increase in Sales", title = "Percentage change in Q3 Sales for 2011-2012") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))+ scale_y_continuous(labels = scales::comma)


## quarter 4 sales growth
data_q4 <- data_quarterly %>% filter(Quarter == 4)
# calculate percentage change in sales for Q1
data_q4$Percent_growth <- 100*(data_q4$Total_Sales - lag(data_q4$Total_Sales))/lag(data_q4$Total_Sales)
data_q4 <- data_q4 %>%  mutate(Percent_growth = replace_na(Percent_growth, 0))

data_q4_11_12 <- data_q4 %>% filter(Year %in% c(2011, 2012))

# plot the percentage increase in sales for Q1 of 2011 and 2012
ggplot(data_q4_11_12, aes(x = Year, y = Percent_growth, fill = Year)) +
  geom_col() +
  scale_fill_manual(values = c("#FF7F00", "#1F78B4")) +
  labs(x = "Year", y = "% Increase in Sales", title = "Percentage change in Q4 Sales for 2011-2012") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

#############
# clustering
#############
ms2 <- monthly_sales

###########
# by store
###########

#multiple entries exist for stores in ms2 aggregate them
ms2 <-data %>% group_by(Store) %>% summarize(avg_weekly_sales = mean(Weekly_Sales),
                                       avg_unemp = mean(Unemployment),
                                       avg_CPI = mean(CPI),
                                       avg_fuel = mean(Fuel_Price),
                                       avg_temp = mean(Temperature),
                                       holidays = ifelse((mean(Holiday_Flag)>0),1, 0))




ms2_norm <- scale(ms2[c("avg_weekly_sales", "avg_unemp", "avg_CPI","avg_fuel","avg_temp")])

#set seed
set.seed(1000)
# #finding the optimal clusters using silhouette method
# fviz_nbclust(ms2_norm, kmeans, method = "silhouette")
#optimal number of clusters based on ss
fviz_nbclust(ms2_norm,kmeans,method = "wss")

#perform clustering
cluster.ms2<-kmeans(ms2_norm[,],5,nstart=20)
# Assign cluster 
ms2$cluster <- cluster.ms2$cluster
#visualize cluster
fviz_cluster(cluster.ms2, ms2_norm)



#analysis using k-means on recency, frequency and monetary value
stores_cluster <-ms2 %>% group_by(cluster) %>%
  dplyr::summarise(total_weekly_sales = sum(avg_weekly_sales),
                   avg_weekly_sales = mean(avg_weekly_sales),
                   avg_unemployment = mean(avg_unemp),
                   avg_CPI = mean(avg_CPI),
                   avg_fuel = mean(avg_fuel),
                   num_stores = n())
view(unique(stores_cluster))

stores_cluster_table <- ms2 %>%
  group_by(cluster) %>%
  summarise(stores = paste(Store, collapse = ", "))

# Print table
view(stores_cluster_table)

##########
# by month
##########

monthly_data <- data %>% group_by(Month) %>%
  summarise(total_monthly_sales = sum(Weekly_Sales),
            avg_monthly_sales = mean(Weekly_Sales),
            avg_unemp = mean(Unemployment),
            avg_CPI = mean(CPI),
            avg_fuel = mean(Fuel_Price),
            avg_temp = mean(Temperature),
            holidays = ifelse((mean(Holiday_Flag)>0),1, 0))

m<- monthly_data[c(1,3,4,5,6,7,8)]
m$month <- factor(m$Month)
m
library(corrplot)
cor_mat <- cor(m[, c("avg_monthly_sales", "avg_unemp", "avg_CPI", "avg_fuel", "avg_temp", "holidays")])

# Create correlation plot
corrplot(cor_mat, title = "Correlation plot",type = "upper", method = "circle", tl.col = "black", diag = FALSE)
  
monthly_norm <- scale(monthly_data[c("total_monthly_sales", "avg_unemp", "avg_CPI","avg_fuel","avg_temp","holidays")])

#set seed
set.seed(1000)
#finding the optimal clusters using silhouette method
fviz_nbclust(monthly_norm, kmeans, method = "silhouette")
#optimal number of clusters based on ss
fviz_nbclust(monthly_norm,kmeans,method = "wss")

#perform clustering
cluster.monthly_norm<-kmeans(monthly_norm[,],4,nstart=20)
# Assign cluster 
monthly_data$cluster <- cluster.monthly_norm$cluster
#visualize cluster
fviz_cluster(cluster.monthly_norm, monthly_norm)

cluster.monthly_norm$cluster

#analysis using k-means on recency, frequency and monetary value
monthly_cluster <-monthly_data %>% group_by(cluster) %>%
  dplyr::summarise(total_monthly_sales = sum(total_monthly_sales),
                   avg_monthly_sales = mean(avg_monthly_sales),
                   avg_unemployment = mean(avg_unemp),
                   avg_CPI = mean(avg_CPI),
                   avg_fuel = mean(avg_fuel),
                   avg_temp = mean(avg_temp),
                   )
view(unique(monthly_cluster))

monthly_cluster_table <- monthly_data %>%
  group_by(cluster) %>%
  summarise(Months = paste(Month, collapse = ", "))

# Print table
view(monthly_cluster_table)


###############
#Regression Modeling
###############
cse=function(reg) {
  rob=sqrt(diag(vcovHC(reg, type="HC1")))
  return(rob)
}
lr1 = lm(Weekly_Sales ~ CPI, data=data) 

lr2 = lm(Weekly_Sales ~ Holiday_Flag , data=data)

lr3 = lm(Weekly_Sales ~ CPI + Temperature + Fuel_Price, data=data)

lr4 = lm(Weekly_Sales ~ Holiday_Flag + Temperature + Fuel_Price + CPI + Unemployment, data=data)


stargazer(lr1, lr2, lr3, lr4,   se =list(cse(lr1), cse(lr2), cse(lr3), cse(lr4)),
          title="Regression Analysis", type="text", 
          star.cutoffs=NA, df=FALSE, digits=3, omit.stat=c( "f"))

###First we are building regression models with one variable and adding more variables to that to increase the
###R^2. The highest R^2 we got is 2.5% which is significantly hight compared to the base model.
###Building a Predictive linear model with those variables.

set.seed(500)

# First, fit the generalized linear model
glm_mod_1 <- glm(Weekly_Sales ~ Holiday_Flag + Temperature + Fuel_Price + CPI + Unemployment,
                 data = data)

# Print a summary of the model
summary(glm_mod_1)

# Use the model to make predictions on the data
glm_predict <- predict(glm_mod_1)

# Calculate the residuals (predicted - actual) for the model
glm_resid <- residuals(glm_mod_1)

# Define a function to calculate MAPE
MAPE <- function(y_actual, y_predict) {
  mean(abs((y_actual - y_predict) / y_actual)) * 100
}

# Define a function to calculate R squared
RSQUARE <- function(y_actual, y_predict) {
  cor(y_actual, y_predict)^2
}

# Calculate the MAPE and R squared for the model
paste('MAPE is', MAPE(data$Weekly_Sales, glm_predict), '%')
paste('R square is', RSQUARE(data$Weekly_Sales, glm_predict))

#The Predictive Linear Model with the variables Holiday_Flag, Temperature, Fuel_Price, CPI, Unemployment
#has a MAPE score of 66% and R-Square of 2.5%. 

#This model is not performing as we expected. So we are using stores as one of the variable as well.
##The weekly sales depends on stores as well. Stores in differnt location has differnt weekly sales.

#Factoring store as the previous regression models were not explainning the model much
walmart<-data%>%
  mutate(Store = as.factor(Store))

# First, fit the generalized linear model
lr5 <- lm(Weekly_Sales ~ Store + Temperature + Holiday_Flag + Unemployment,
          data = walmart)
lr6 <- lm(Weekly_Sales ~ Store + Unemployment+ CPI + Holiday + Temperature + 
            Store*Unemployment + Unemployment*CPI + Holiday*Temperature, data = walmart)

stargazer(lr5, lr6,   se =list(cse(lr5), cse(lr6)),
          title="Regression Analysis", type="text", 
          star.cutoffs=NA, df=FALSE, digits=3, omit.stat=c( "f"))

###After converting stores as factor we are devloping two models. We have also added interactive terms in our 
#second regression. The R-Square has increased to 95% which is a big difference compared to the previous model. 

#Now we are building predictive linear regression models for these two models. 


# First, fit the generalized linear model
glm_mod_2 <- glm(Weekly_Sales ~ Store + Temperature + Holiday_Flag + Unemployment,
                 data = walmart)

# Print a summary of the model
summary(glm_mod_2)

# Use the model to make predictions on the data
glm_predict <- predict(glm_mod_2)

# Calculate the residuals (predicted - actual) for the model
glm_resid <- residuals(glm_mod_2)

# Define a function to calculate MAPE
MAPE <- function(y_actual, y_predict) {
  mean(abs((y_actual - y_predict) / y_actual)) * 100
}

# Define a function to calculate R squared
RSQUARE <- function(y_actual, y_predict) {
  cor(y_actual, y_predict)^2
}

# Calculate the MAPE and R squared for the model
paste('MAPE is', MAPE(data$Weekly_Sales, glm_predict), '%')
paste('R square is', RSQUARE(data$Weekly_Sales, glm_predict))

###Model 3

# First, fit the generalized linear model
glm_mod_3 <- glm(Weekly_Sales ~ Store + Unemployment + CPI + Holiday+ Temperature + Unemployment*CPI + Holiday*Temperature,
                 data = walmart)
options(scipen = 999)
# Print a summary of the model
summary(glm_mod_3)

# Use the model to make predictions on the data
glm_predict <- predict(glm_mod_3)

# Calculate the residuals (predicted - actual) for the model
glm_resid <- residuals(glm_mod_3)

# Define a function to calculate MAPE
MAPE <- function(y_actual, y_predict) {
  mean(abs((y_actual - y_predict) / y_actual)) * 100
}

# Define a function to calculate R squared
RSQUARE <- function(y_actual, y_predict) {
  cor(y_actual, y_predict)^2
}

# Calculate the MAPE and R squared for the model
paste('MAPE is', MAPE(data$Weekly_Sales, glm_predict), '%')
paste('R square is', RSQUARE(data$Weekly_Sales, glm_predict))

##This final model has a MAPE score of 7.58% and R-Squared of 95%. So this model could explain 95% of the variance in the data.


