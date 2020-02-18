#Marketing Mix
#A manufacturer of a consumer goods brand wants to know how much additional sales they created 
#that can be at least associated or explained by their three marketing activities: 
#TV advertisements, online banner advertisements, and direct-mail sales promotions.

# Data provided: 3 years of monthly sales data for their product, 
#as well as the average retail prices and the necessary advertising data, 
#split separately into their 5 regional markets.

#importing data
cinnamontea <- read.csv('Data_CinnamonTeaStudent_11.csv')
dataset_original <- read.csv('Data_CinnamonTeaStudent_11.csv')
#Encoding Categorical Data of Month
#cinnamontea$Jan <- 1L * (cinnamontea1$month == "Jan")
cinnamontea$Jan <- as.numeric(cinnamontea$month == 'Jan')
cinnamontea$Feb <- as.numeric(cinnamontea$month == 'Feb')
cinnamontea$Mar <- as.numeric(cinnamontea$month == 'Mar')
cinnamontea$Apr <- as.numeric(cinnamontea$month == 'Apr')
cinnamontea$May <- as.numeric(cinnamontea$month == 'May')
cinnamontea$Jun <- as.numeric(cinnamontea$month == 'Jun')
cinnamontea$Jul <- as.numeric(cinnamontea$month == 'Jul')
cinnamontea$Aug <- as.numeric(cinnamontea$month == 'Aug')
cinnamontea$Sep <- as.numeric(cinnamontea$month == 'Sep')
cinnamontea$Oct <- as.numeric(cinnamontea$month == 'Oct')
cinnamontea$Nov <- as.numeric(cinnamontea$month == 'Nov')
cinnamontea$Dec <- as.numeric(cinnamontea$month == 'Dec')
#cinnamontea = subset(cinnamontea, select = -c(month))

#Encoding Categorical Data of Region
#capital <- region %in% 'Capital'
cinnamontea$North <- as.numeric(cinnamontea$region == 'TheNorth')
cinnamontea$South <- as.numeric(cinnamontea$region == 'TheSouth')
cinnamontea$West <- as.numeric(cinnamontea$region == 'West')
cinnamontea$East <- as.numeric(cinnamontea$region == 'East')
cinnamontea$Capital <- as.numeric(cinnamontea$region == 'Capital')
#cinnamontea = subset(cinnamontea, select = -c(region))
#Number of Month 
cinnamontea$num_month <- match(cinnamontea$month, month.abb)

#Classifying Year
cinnamontea$year1 <- as.numeric(time >= 1 & time <= 12)
cinnamontea$year2 <- as.numeric(time >= 13 & time <= 24)
cinnamontea$year3 <- as.numeric(time >= 25 & time <= 36)

# Quarter 
cinnamontea$q1 <- as.numeric(time %% 12 == 1 | time %% 12 == 2 | time %% 12 == 3)
cinnamontea$q2 <- as.numeric(time %% 12 == 4 | time %% 12 == 5 | time %% 12 == 6)
cinnamontea$q3 <- as.numeric(time %% 12 == 7 | time %% 12 == 8 | time %% 12 == 9)
cinnamontea$q4 <- as.numeric(time %% 12 == 10 | time %% 12 == 11 | time %% 12 == 0)

# Season 
cinnamontea$spring <- as.numeric(time %% 12 == 3 | time %% 12 == 4 | time %% 12 == 5)
cinnamontea$summer <- as.numeric(time %% 12 == 6 | time %% 12 == 7 | time %% 12 == 8)
cinnamontea$autumn <- as.numeric(time %% 12 == 9 | time %% 12 == 10 | time %% 12 == 11)
cinnamontea$winter <- as.numeric(time %% 12 == 0 | time %% 12 == 1 | time %% 12 == 2)
cinnamontea$season[cinnamontea$spring==1] <- "spring"
cinnamontea$season[cinnamontea$summer==1] <- "summer"
cinnamontea$season[cinnamontea$autumn==1] <- "autumn"
cinnamontea$season[cinnamontea$winter==1] <- "winter"

# Promotion M-1
library(dplyr)
cinnamontea$ad1_lastmonth <- lag(cinnamontea$ad1)
cinnamontea$ad2_lastmonth <- lag(cinnamontea$ad2)
cinnamontea$prom_lastmonth <- lag(cinnamontea$prom)
#replace missing value to 0
cinnamontea[is.na(cinnamontea)] <- 0
# Revenue
cinnamontea$revenue <- cinnamontea$sales * cinnamontea$price

# Assigning Variables
sales <- cinnamontea$sales
price <- cinnamontea$price
tv_ad <- cinnamontea$ad1
online_ad <- cinnamontea$ad2
mail_prom <- cinnamontea$prom
time <- cinnamontea$time
month <- cinnamontea$month
num_month <- cinnamontea$num_month
region <- cinnamontea$region
revenue <- cinnamontea$revenue
season <- cinnamontea$season

# Observing the data for sales driven by one independent marketing activities
only_tv <- subset(cinnamontea, ad2 == 0 & prom == 0)
only_online <- subset(cinnamontea, ad1 == 0 & prom == 0)
only_mail <- subset(cinnamontea, ad1 == 0 & ad2 == 0)

# Observing the data yearly
year_1 <- subset(cinnamontea, year1 == 1)
year_2 <- subset(cinnamontea, year2 == 1)
year_3 <- subset(cinnamontea, year3 == 1)

#Importing library for visualisation
library(ggplot2)

#Simple linear regression of each Marketing activities + Visualisation
# Sales & tv ad
model_tv_ad <- lm(sales ~ tv_ad)
summary(model_tv_ad)
only_tv_ad <- lm(only_tv$sales ~ only_tv$ad1)
summary(only_tv_ad)
# Visualising sales & tv ad
ggplot() +
  geom_point(aes(x = tv_ad, y = sales),
             colour = 'skyblue', size=1.1) +
  geom_line(aes(x = tv_ad, y = predict(model_tv_ad)),
            colour = 'steelblue', size=0.7) +
  geom_line(aes(x = only_tv$ad1, y = predict(only_tv_ad)),
            colour = 'limegreen', size=0.7) +  
  ggtitle('Regression') +
  xlab('TV Advertising') +
  ylab('Sales')

# Sales & online ad
model_online_ad <- lm(sales ~ online_ad)
summary(model_online_ad)
only_online_ad <- lm(only_online$sales ~ only_online$ad2)
summary(only_online_ad)
# Visualising sales & online ad
ggplot() +
  geom_point(aes(x = online_ad, y = sales),
             colour = 'skyblue', size=1.1) +
  geom_line(aes(x = online_ad, y = predict(model_online_ad)),
            colour = 'steelblue', size=0.7) +
  geom_line(aes(x = only_online$ad2, y = predict(only_online_ad)),
            colour = 'limegreen', size=0.7) +  
  ggtitle('Regression') +
  xlab('Online Advertising') +
  ylab('Sales')

# Sales & direct mail promotion
model_mail <- lm(sales ~ mail_prom)
summary(model_mail)
only_mail_prom <- lm(only_mail$sales ~ only_mail$prom)
summary(only_mail_prom)
# Visualising sales & mail prom
ggplot() +
  geom_point(aes(x = mail_prom, y = sales),
             colour = 'skyblue', size=1.1) +
  geom_line(aes(x = mail_prom, y = predict(model_mail)),
            colour = 'steelblue', size=0.7) +
  geom_line(aes(x = only_mail$prom, y = predict(only_mail_prom)),
            colour = 'limegreen', size=0.7) +  
  ggtitle('Regression') +
  xlab('Direct Mail Promotion') +
  ylab('Sales')

#Simple linear regression of Price
model_price <- lm(sales ~ price)
summary(model_price)
# Visualising sales & price
ggplot() +
  geom_point(aes(x = price, y = sales),
             colour = 'skyblue', size=1.1) +
  geom_line(aes(x = price, y = predict(model_price)),
            colour = 'steelblue', size=0.7) +
  ggtitle('Regression of Sales by Price') +
  xlab('Price') +
  ylab('Sales')

# Visualising sales by month
#install.packages('ggplot2')
ggplot(data = NULL, aes(x = num_month, y = sales)) + 
  geom_boxplot(aes(group=num_month)) +
  scale_x_continuous(breaks=seq(0,12,1),
                     labels = c("", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  ggtitle("Monthly Sales") + 
  ylab("Sales") +
  xlab("Month")
# Visualising price by month
#install.packages('ggplot2')
ggplot(data = NULL, aes(x = num_month, y = price)) + 
  geom_boxplot(aes(group=num_month)) +
  geom_smooth(mapping=aes(x = num_month, y = price, 
                          color = "#55DDE0"), 
              size=0.6, se=FALSE, span = 0.4) +
  scale_x_continuous(breaks=seq(0,12,1),
                     labels = c("", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  ggtitle("Monthly Price Range") + 
  ylab("Price") +
  xlab("Month")

# Visualising sales by year
#install.packages('ggplot2')
graph_year = ggplot() + 
  geom_smooth(mapping=aes(x = year_1$num_month, y = year_1$sales, 
                                      color = "Year 1"), 
                          size=0.6, se=FALSE, span = 0.4) +
  geom_smooth(mapping=aes(x = year_2$num_month, y = year_2$sales,
                                      color = "Year 2"), 
                          size=0.6, se=FALSE, span = 0.4) +
  geom_smooth(mapping=aes(x = year_3$num_month, y = year_3$sales,
                                      color = "Year 3"), 
                          size=0.6, se=FALSE, span = 0.4) +
  geom_smooth(mapping=aes(x = cinnamontea$num_month, y = cinnamontea$sales,
                                      color = "Average"),
                          size=0.6, linetype = "dashed", span = 0.4) +
  scale_x_continuous(breaks=seq(0,12,1),
                                   labels = c("", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  ggtitle("Monthly Sales by Year") + 
  scale_color_manual(name = "Legend", 
                     values = c("Year 1" = "#55DDE0", 
                                "Year 2" = "#F26419",
                                "Year 3" = "#F6AE2D",
                                "Average" = "#2F4858")) +
  theme(legend.position = "right") +
  ylab("Sales") +
  xlab("Month") +
  theme_minimal()
graph_year  

# Visualising sales by year - Span default
#install.packages('ggplot2')
graph_year = ggplot() + 
  geom_smooth(mapping=aes(x = year_1$num_month, y = year_1$sales, 
                          color = "Year 1"), 
              size=0.6, se=FALSE, span = 0.75) +
  geom_smooth(mapping=aes(x = year_2$num_month, y = year_2$sales,
                          color = "Year 2"), 
              size=0.6, se=FALSE, span = 0.75) +
  geom_smooth(mapping=aes(x = year_3$num_month, y = year_3$sales,
                          color = "Year 3"), 
              size=0.6, se=FALSE, span = 0.75) +
  geom_smooth(mapping=aes(x = cinnamontea$num_month, y = cinnamontea$sales,
                          color = "Average"),
              size=0.6, linetype = "dashed", span = 0.75) +
  scale_x_continuous(breaks=seq(0,12,1),
                     labels = c("", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  ggtitle("Monthly Sales by Year") + 
  scale_color_manual(name = "Legend", 
                     values = c("Year 1" = "#55DDE0", 
                                "Year 2" = "#F26419",
                                "Year 3" = "#F6AE2D",
                                "Average" = "#2F4858")) +
  theme(legend.position = "right") +
  ylab("Sales") +
  xlab("Month") +
  theme_minimal()
graph_year  
                
# Visualising sales by region
ggplot(data = NULL, aes(x = region, y = sales)) + 
  geom_boxplot(aes(group=region),
               colour = c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419")) +
  ggtitle("Sales by Region") + 
  ylab("Sales") +
  xlab("Region")
#trying pie chart
pie = ggplot(cinnamontea, aes(x="", y=sum(cinnamontea$sales[region])/sum(cinnamontea$sales), fill=region)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Sales by Region")
pie

#Visualising difference for 3 marketing activities
boxplot(tv_ad, online_ad, mail_prom, 
        main="Marketing Activities",
        names = c("tv_ad", "online_ad", "mail_prom"),
        color = c('skyblue', 'steelblue', 'grey'))
#Visualising price range
ggplot(data = NULL, aes(x = season, y = price)) + 
  geom_boxplot(aes(group=season)) +
  scale_x_continuous(breaks=seq(0,4,1),
                     labels = c("", "Spring", "Summer", "Autumn", "Winter")) +
  ggtitle("Seasonal Price") + 
  ylab("Price") +
  xlab("Season")


# table of sum per month
table <- aggregate(cinnamontea$ad1, by=list(cinnamontea$num_month), FUN=sum)
table$ad2 <- aggregate(cinnamontea$ad2, by=list(cinnamontea$num_month), FUN=sum)
table$prom <- aggregate(cinnamontea$prom, by=list(cinnamontea$num_month), FUN=sum)
# Visualising Marketing Activities by month
marketing <- ggplot() +
          geom_col(aes(x = num_month, y = tv_ad),
                   width=0.25,
                   colour="#55DDE0",
                   fill="#55DDE0",
                   position=position_dodge()) +
          geom_col(aes(x = num_month, y = online_ad),
                   width=0.25,
                   colour="#33658A",
                   fill="#33658A",
                   position=position_dodge()) +
          geom_col(aes(x = num_month, y = mail_prom),
                   width=0.25,
                   colour="#F6AE2D",
                   fill="#F6AE2D",
                   position=position_dodge()) + 
          scale_color_manual(name = "Marketing Type", 
                     values = c("#55DDE0" = "TV Ads" , 
                                "#33658A" = "Online Ads",
                                "#F6AE2D" = "Mail Promotion")) +
          theme(legend.position = "right") +
          ggtitle('Marketing Activities by Month') +
          xlab('Month') +
          ylab('Marketing Activities') +
          scale_fill_brewer(palette="Paired") +
          scale_x_continuous(breaks=seq(0,12,1),
                             labels = c("", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
          theme_minimal()
marketing = marketing + layer(geom = "bar", position = "dodge")
marketing

#Our sales often show a lot of variation. 
#Can you explain to us possible sources of the variation, other than our marketing activities?
#Multiple Regression with only marketing activities
model1 <- lm(sales ~ tv_ad + online_ad + mail_prom)
summary(model1)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(42)
split = sample.split(cinnamontea$sales, SplitRatio = 0.8)
training_set = subset(cinnamontea, split == TRUE)
test_set = subset(cinnamontea, split == FALSE)

#Do some basic exploration of the trainingset only.
#plot(training_set)
#pairs(training_set)
#summary(training_set)

# Fitting Multiple Linear Regression with all variables
model_all <- lm(sales ~ price + ad1 + ad2 + prom + 
                  Jan + Feb + Mar + Apr + May + Jun + 
                  Jul + Aug + Sep + Oct + Nov + Dec +
                  North + South + West + East + Capital +
                  ad1_lastmonth + ad2_lastmonth + prom_lastmonth,
                data = training_set)
summary(model_all)

# Feature selection 
if (!require("DAAG")){install.packages("DAAG")};library(DAAG) 
if (!require("olsrr")){install.packages("olsrr")};library(olsrr) 
forward <- ols_step_forward_p(model_all) 
plot(forward)
summary(forward)
forward
backward <- ols_step_backward_p(moel_all) 
plot(backward)
summary(backward)
backward
stepward <- ols_step_both_p(model_all) 
plot(stepward)
summary(stepward)
#regressor <- lm(sales ~ price + tv_ad + mail_prom + as.factor(region) + as.factor(month))
#summary(regressor)

# Fitting Multiple Linear Regression with significant variables only
# result of forward & both/stepward
model_selection <- lm(sales ~ prom + Capital + Dec + prom_lastmonth + 
                      Jan + Feb + Mar + 
                      South + ad1 + Nov + ad1_lastmonth + 
                      North + price  + Apr,
                data = training_set)
summary(model_selection)
model_selection
#points(x = training_set$time, y = model_selection$fitted.values,pch=18,col=rgb(0.5,0,0,.8),type="p")

#Validation and robustness checks. Build more trust in the model. Is it over-fitted? Others? 
require(DAAG)
cv_train <- cv.lm(sales ~ prom + Dec + Capital + prom_lastmonth + 
                    Jan + Mar + Feb + 
                    South + Nov + ad1 + North + 
                    price + ad1_lastmonth + Apr,
                  data=training_set,
                  m = nrow(training_set),
                  plotit = FALSE)
head(cv_train)
#Predicting Values
y_pred = predict.lm(model_selection, newdata = test_set)
pred_all = predict.lm(model_selection, newdata = cinnamontea)
# Compute the average square prediction error using the data in 'cvresult'.
train_cv_mspe <- sqrt(sum((cv_train[,"sales"] - cv_train[,"cvpred"]) ^2 ) / nrow(training_set))
train_cv_mspe
train_model_mspe <- sqrt(sum((cv_train[,"sales"] - cv_train[,"Predicted"]) ^2 ) / nrow(training_set))
train_model_mspe
#To obtain the average square prediction error we need to divide this sum by the number of points in the training_set.
test_mspe <- sqrt(sum((test_set[,"sales"] - y_pred) ^2 ) / nrow(test_set)) # cross validation mean squared predition error.
test_mspe
#MAPE Mean absolute percentage error
#Visualising Bias^2 vs Variance Trade-off

# Answer to How many packs did we sell associated to the advertisements and promotions? 
total_tv <- sum(tv_ad)*model_tv_ad$coefficients[2]
total_online <- sum(online_ad)*model_online_ad$coefficients[2]
total_mail <- sum(mail_prom)*model_mail$coefficients[2]
print("How much sales can be associated with types of marketing:")
print(total_tv)
print(total_online)
print(total_mail)
# Using Multivariate Regression, the answer will be:
sum_tv <- sum(tv_ad)*model_test$coefficients[11]
sum_mail <- sum(mail_prom)*model_test$coefficients[2]
total_packs <- sum_tv+sum_mail
print("Total packs sold associated to ads & promotion:")
print(total_packs)

#2 Our TV ads cost us 2 million Pounds, our Banners 500,000 Pounds. Which one is more efficient?
# None of them are efficient!! But if i have to choose, then it's the TV ad
# Calculating the ratio of cost efficiency
cost_tv <- 2000000
cost_online <- 500000
eff_tv <- total_tv*mean(price) / cost_tv
eff_online <- total_online*mean(price) / cost_online
print(eff_tv)
print(eff_online)

#Results should be supported (in the slides) by a well-fitting model
#Should show at least one figure/table/result to build trust in the model.
# Visualising the Regression Model results 
#install.packages('ggplot2')
ggplot() +
  geom_line(mapping=aes(x = time, y = sales,
                        colour = 'Real Sales'),
            size=0.5) +
  geom_line(mapping=aes(x = time, y = pred_all,
                        colour = 'Predicted Sales'),
            size=0.5) +
  scale_color_manual(values = c(
    'Real Sales' = 'skyblue',
    'Predicted Sales' = 'steelblue')) +
  labs(color = 'Legend') +
  scale_x_continuous(breaks=seq(0,36,12),
                     labels=c("Year 0", "Year 1", "Year 2", "Year 3")) +
  ggtitle('Multivariable Regression of Cinnamon Tea Sales') +
  xlab('Time') +
  ylab('Sales')

# Visualising the Regression Model results with a smooth line 
#install.packages('ggplot2')
ggplot() +
  geom_smooth(mapping=aes(x = time, y = sales,
                        colour = 'Real Sales'),
            size=0.7, se=FALSE, span=0.04) +
  geom_smooth(mapping=aes(x = time, y = pred_all,
                        colour = 'Predicted Sales'),
            size=0.7, se=FALSE, span=0.04) +
  scale_color_manual(values = c(
    'Real Sales' = 'skyblue',
    'Predicted Sales' = 'steelblue')) +
  labs(color = 'Legend') +
  scale_x_continuous(breaks=seq(0,36,12),
                     labels=c("Year 0", "Year 1", "Year 2", "Year 3")) +
  ggtitle('Multivariable Regression of Cinnamon Tea Sales') +
  xlab('Time') +
  ylab('Sales')

#Assumption tests and limitations of the model/ analysis. 
#Are common assumptions met? What are remaining dangers in the case/data/unobserved drivers? 
plot(model_selection)
hist(resid(model_selection))
# Install and load package “car”:
if (!require("car")){install.packages("car")};
library(car)
durbinWatsonTest(model_selection) #providesDurbinWatsonstatisticstotestfor autocorrelation
vif(model_selection) #providesvarianceinflationfactors,tocheckformulticollinearity 
# Is our model plagued by autocorrelation or multi-collinearity?
#Residual Standard Error (RSE), or sigma test:
sigma(model_selection)/mean(test_set$sales)
#component+residual test for each variable
crPlots(model_selection)
#homoscedasticity
install.packages("lmtest")
require(lmtest)
lmtest::bptest(model_selection)

#Explore scatter plots:
pairs(cinnamontea)
#Obtain the covariance matrix for all variables in cinnamontea
cov(cinnamontea)

#other testing
#T-Test simple
t.test(tv_ad, sales)
t.test(online_ad, sales)
t.test(mail_prom, sales)

#test data aov simple
summary(aov(sales ~ tv_ad))
summary(aov(sales ~ online_ad))
summary(aov(sales ~ mail_prom))

#testing simple model
plot(model_tv_ad)
hist(resid(model_tv_ad))
plot(model_online_ad)
hist(resid(model_online_ad))
plot(model_mail)
hist(resid(model_mail))

# Correlations test
cor(sales,tv_ad,method="pearson") #default
cor(sales,tv_ad,use="complete.obs") #disregardmissingdata 
cor(sales,online_ad,method="pearson") #default
cor(sales,online_ad,use="complete.obs") #disregardmissingdata 
cor(sales,mail_prom,method="pearson") #default
cor(sales,mail_prom,use="complete.obs") #disregardmissingdata 
cor(cinnamontea[,2:6],use="pairwise.complete.obs") #correlationtableforvariables
plot(cinnamontea[,2:6])

# Trying out something else
# Validation for Forecasting
# Splitting the dataset into the train_forecast set and test_forecast set
train_forecast <- subset(cinnamontea, time <= 24)
test_forecast <- subset(cinnamontea, time >= 25)

# Fitting Multiple Linear Regression with all variables
model_forecast_all <- lm(sales ~ price + ad1 + ad2 + prom + 
                       Jan + Feb + Mar + Apr + May + Jun + 
                       Jul + Aug + Sep + Oct + Nov + Dec +
                       North + South + West + East + Capital +
                       ad1_lastmonth + ad2_lastmonth + prom_lastmonth,
                     data = train_forecast)
summary(model_forecast_all)

# Feature selection for forecast
if (!require("DAAG")){install.packages("DAAG")};library(DAAG) 
if (!require("olsrr")){install.packages("olsrr")};library(olsrr) 
forward_forecast <- ols_step_forward_p(model_forecast_all, penter = 0.02) 
plot(forward_forecast)
summary(forward_forecast)
forward_forecast
backward_forecast <- ols_step_backward_p(model_forecast_all, penter = 0.02) 
plot(backward_forecast)
summary(backward_forecast)
backward_forecast
stepward_forecast <- ols_step_both_p(model_forecast_all, penter = 0.02) 
plot(stepward_forecast)
summary(stepward_forecast)

# Fitting Multiple Linear Regression with all variables
model_forecast <- lm(sales ~ price + ad1 + ad2 + prom + 
                       Jan + Feb + Mar + Apr + May + Jun + 
                       Nov + Dec +
                       North + South + Capital +
                       ad1_lastmonth + prom_lastmonth,
                     data = train_forecast)
summary(model_forecast)

#Validation and robustness checks. Build more trust in the model. Is it over-fitted? Others? 
require(DAAG)
cv_forecast <- cv.lm(sales ~ price + ad1 + ad2 + prom + 
                    Jan + Feb + Mar + Apr + May + Jun + 
                    Nov + Dec +
                    North + South + Capital +
                    ad1_lastmonth + prom_lastmonth,
                  data = train_forecast,
                  m = nrow(train_forecast),
                  plotit = FALSE)
head(cv_forecast)
#Predicting Values
y_pred_forecast = predict.lm(model_forecast, newdata = test_forecast)
pred_all_forecast = predict.lm(model_forecast, newdata = cinnamontea)
# Compute the average square prediction error using the data in 'cvresult'.
forecast_cv_mspe <- sqrt(sum((cv_forecast[,"sales"] - cv_forecast[,"cvpred"]) ^2 ) / nrow(train_forecast))
forecast_cv_mspe
forecast_model_mspe <- sqrt(sum((cv_forecast[,"sales"] - cv_forecast[,"Predicted"]) ^2 ) / nrow(train_forecast))
forecast_model_mspe
#To obtain the average square prediction error we need to divide this sum by the number of points in the training_set.
forecast_test_mspe <- sqrt(sum((test_forecast[,"sales"] - y_pred_forecast) ^2 ) / nrow(test_forecast)) # cross validation mean squared predition error.
forecast_test_mspe

# Visualising training vs test & forecast
library(fpp)
library(forecast)
# Visualising the Regression Model results for forecasting with a smooth line 
#install.packages('ggplot2')
ggplot() +
  geom_smooth(mapping=aes(x = time, y = sales,
                          colour = 'Real Sales'),
              size=0.7, se=FALSE, span=0.05) +
  geom_smooth(mapping=aes(x = time, y = pred_all_forecast,
                          colour = 'Predicted Sales'),
              size=0.7, se=FALSE, span=0.05) +
  scale_color_manual(values = c(
    'Real Sales' = 'skyblue',
    'Predicted Sales' = 'steelblue')) +
  labs(color = 'Legend') +
  scale_x_continuous(breaks=seq(0,36,12),
                     labels=c("Year 0", "Year 1", "Year 2", "Year 3")) +
  ggtitle('Multivariate Regression of Cinnamon Tea Sales + Forecast for Year 3') +
  xlab('Time') +
  ylab('Sales')