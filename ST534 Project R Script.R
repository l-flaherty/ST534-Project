##########Code Written By Liam Flaherty For ST534 Final Project##########
#####1. Initial Data######
###1a. Load in data and required packages###
library(tidyverse)
library(scales)
library(forecast)
library(tseries)

path="C:/Users/LiamFlaherty/Documents/Academics/ST534 Time Series/Project/weight.csv"
weight=read.csv(path)

weight=weight|>
  mutate(Date=as.Date(Date)) |>
  mutate(Day=weekdays(Date)) |>
  select(Date, Day, Weight, Lower, Upper, HR)

str(weight)
summary(weight)



###1b. Split into training and test###
train=weight[which(weight$Date<"2023-02-01"),]     
test=weight[which(weight$Date>="2023-02-01"),]     




#####2. Exploratory Data Analysis#####
###2a. Heart Rate###
ggplot(train, aes(x=Date, y=HR)) +
  geom_line(color="red", linewidth=1) +
  labs(title="Evolution Of Heart Rate",
       x="Date",
       y="Heart Rate") +
  scale_x_date(
    date_breaks="1 month",
    date_labels="%b %Y") +
  theme_bw() +
  theme(
    plot.title=element_text(hjust=0.5, size=16),  #Center the title#
    axis.text=element_text(size=14),  
    axis.title=element_text(size=14),  
    axis.text.x=element_text(angle=45, hjust=1))

hist(train$HR,
     main="Histogram Of HR (8/2022 - 3/2023)",
     xlab="Heart Rate",
     ylab="Frequency",
     xlim=c(45,70),
     col="Red")

sd(train$HR)



###2b. Weight###
ggplot(train, aes(x=Date, y=Weight)) +
  geom_line(color="blue", linewidth=1) +
  labs(title="Evolution Of Weight",
       x="Date",
       y="Weight") +
  scale_x_date(
    date_breaks="1 month",
    date_labels="%b %Y") +
  theme_bw() +
  theme(
    plot.title=element_text(hjust=0.5, size=16),  #Center the title#
    axis.text=element_text(size=14), 
    axis.title=element_text(size=14),  
    axis.text.x=element_text(angle=45, hjust=1))





#####3. Analysis#####
###3a. Convert to time series###
ts_weight=ts(train$Weight)              #convert to time series object#
ts_hr=ts(train$HR)



###3b. White Noise Test###
#Clear that weight is not white noise#
whitenoise6=Box.test(ts_hr,                #Do we need to fit model?#
                     lag=6, 
                     type="Ljung-Box")   
whitenoise6                                #p small \implies yes#

whitenoise12=Box.test(ts_hr,               #Do we need to fit model?#
                      lag=12, 
                      type="Ljung-Box")   
whitenoise12                               #p small \implies yes#



###3c. Test for stationarity###
adf_result_weight=suppressWarnings(adf.test(ts_weight))  
adf_result_weight                          #difference needed#

adf_result_hr=suppressWarnings(adf.test(ts_hr))  
adf_result_hr                               #stationary#



###3c. Initial P/ACF For HR###
par(mfrow=c(1,2))                   #split the display to show two figures in one plot#

acf(ts_hr, 
    main=paste0("Autocorrelation Function For", "\n", "Heart Rate Data"),
    lag.max=30,
    ci.col="blue", 
    col="red", 
    lwd=4)

pacf(ts_hr, 
     main=paste0("Partial Autocorrelation Function For", "\n", "Heart Rate Data"),
     lag.max=30,
     ci.col="blue", 
     col="red", 
     lwd=4)

par(mfrow=c(1,1))                    #back to one figure per plot#



###3d. Initial P/ACF For Weight###
par(mfrow=c(1,2))

acf(ts_weight, 
    main=paste0("Autocorrelation Function For", "\n", "Weight Data"),
    lag.max=30,
    ci.col="blue", 
    col="red", 
    lwd=4)

pacf(ts_weight, 
     main=paste0("Partial Autocorrelation Function For", "\n", "Weight Data"),
     lag.max=30,
     ci.col="blue", 
     col="red", 
     lwd=4)

par(mfrow=c(1,1)) 



###3e. P/ACF For Weight With Regular Difference###
train_diff=diff(ts_weight, lag=1)

par(cex.axis=1.5, cex.lab=1.5)
plot(train_diff,
     main="Time Series Of Differenced Weight Data, Lag=1",
     xlab="Time",
     ylab="Difference",
     cex.axis=2,
     cex.lab=2,
     col="blue",
     lwd=2)

par(mfrow=c(1,2))
acf(train_diff, 
    main=paste0("Autocorrelation Function For", "\n", "Differenced Weight Data"),
    lag.max=30,
    ci.col="blue", 
    col=ifelse((0:30 %% 7)==0, "forestgreen", "red"), 
    lwd=4)

pacf(train_diff, 
     main=paste0("Partial Autocorrelation Function For", "\n", "Differenced Weight Data"),
     lag.max=30,
     ci.col="blue", 
     col=ifelse((0:30 %% 7)==0, "forestgreen", "red"), 
     lwd=4)

par(mfrow=c(1,1))



###3f. P/ACF For Weight With Just Seasonal Difference###
ts_weight_sdiff=diff(ts_weight, lag=7)                  #just D=1#

par(mfrow=c(1,2))

acf(ts_weight_sdiff, 
    main=paste0("Autocorrelation Function For", "\n", "Seasonal Differenced Weight Data"),
    lag.max=30,
    ci.col="blue", 
    col=ifelse((0:30 %% 7)==0, "forestgreen", "red"), 
    lwd=4)

pacf(ts_weight_sdiff, 
     main=paste0("Partial Autocorrelation Function For", "\n", "Seasonal Differenced Weight Data"),
     lag.max=30,
     ci.col="blue", 
     col=ifelse((0:30 %% 7)==0, "forestgreen", "red"), 
     lwd=4)

par(mfrow=c(1,1))



###3g. P/ACF For Weight With Both Differences###
ts_weight_both=diff(diff(ts_weight, lag=7), lag=1)               #D=1, d=1#

par(mfrow=c(1,2))

acf(ts_weight_both, 
    main=paste0("Autocorrelation Function For", "\n", "Seasonal And Regularly Differenced Weight"),
    lag.max=30,
    ci.col="blue", 
    col=ifelse((0:30 %% 7)==0, "forestgreen", "red"), 
    lwd=4)

pacf(ts_weight_both, 
     main=paste0("Partial Autocorrelation Function For", "\n", "Seasonal And Regularly Differenced Weight"),
     lag.max=30,
     ci.col="blue", 
     col=ifelse((0:30 %% 7)==0, "forestgreen", "red"), 
     lwd=4)

par(mfrow=c(1,1))





#####4. Try A Bunch Of Models#####
###4a. For weight Data###
ARIMAs_model_weight=vector()
aic_weight=vector()              
bic_weight=vector()
LBtest_weight=vector()
s=7                    #From Analysis#
m=5                    #the number of MA and AR terms to try#
i=0                    #to keep track of iterations

for (p in 1:m) {
  for (d in 1:2) {
    for (q in 1:m) {
      for (P in 1:m) {
        for (D in 1:2) {
          for (Q in 1:m) {
            i=i+1
            print(paste0("i=", round(i/(m^4*2*2), 2) ))    #where we're at in the process#
            
            mymodel=paste0("ARIMA(", p-1, ",", d-1, ",", q-1, ")(", P-1, ",", D-1, ",", Q-1, ")",s)
            
            setTimeLimit(cpu=3, elapsed=3)                 #otherwise would take forever#
            model_result=tryCatch({                        #for convergence problems#
              model=arima(ts_weight, 
                          order=c(p-1,d-1,q-1),
                          seasonal=list(order=c(P-1,D-1,Q-1), period=s),
                          method="ML")                     #By Maximum Likelihood#
              
              ARIMAs_model_weight[i]=mymodel
              aic_weight[i]=round(AIC(model),2)
              bic_weight[i]=round(BIC(model),2)
              LBtest_weight[i]=round(Box.test(residuals(model), lag=21, type="Ljung-Box")$p.value,2)
              
            }, error=function(e) {                         #for convergence problems#
              ARIMAs_model_weight[i]=mymodel
              aic_weight[i]=999
              bic_weight[i]=999
              LBtest_weight[i]=999
            })
            setTimeLimit(cpu=Inf, elapsed=Inf)
            
          }
        }
      }
    }
  }
}

df_weight=data.frame(ARIMAs_model=ARIMAs_model_weight, 
                     aic=aic_weight, 
                     bic=bic_weight, 
                     LBtest=LBtest_weight) 
df_weight=df_weight[which(df_weight$bic<999),]
df_weight=df_weight[order(df_weight$bic),]
df_weight[1:15,]

save(df_weight, file="modelfit_weight.R")
load("modelfit_weight.R")                                 #so don't have to run this part of the code#



###4b. For HR Data###
ARIMAs_model_hr=vector()
aic_hr=vector()              
bic_hr=vector()
LBtest_hr=vector()
m=5                    #the number of MA and AR terms to try#
i=0

for (p in 0:m) {
  for (q in 0:m) {
    i=i+1
    mymodel=paste0("ARMA(", p, ",", q, ")")
    
    model=arima(ts_hr, 
                order=c(p,0,q),
                method="ML")               #by Maximum Likelihood#
    
    ARIMAs_model_hr[i]=mymodel
    aic_hr[i]=round(AIC(model),2)
    bic_hr[i]=round(BIC(model),2)
    LBtest_hr[i]=round(Box.test(residuals(model), lag=21, type="Ljung-Box")$p.value,2)
  }
}

df_hr=data.frame(
  ARMA_model=ARIMAs_model_hr, 
  aic=aic_hr, 
  bic=bic_hr, 
  LBTest=LBtest_hr)
df_hr=df_hr[order(df_hr$bic),]
df_hr[1:10,]

save(df_hr, file="modelfit_hr.R")
load("modelfit_hr.R")                               #so don't have to run this part of the code#



#####4c. Getting parameter weights###
hr_ar1=arima(ts_hr,                                #best in terms of BIC#
             order=c(1,0,0), 
             method="ML")

hr_ma1=arima(ts_hr,                                #our recommendation; 2nd best in AIC and BIC#
             order=c(0,0,1),
             method="ML")

weight_sarima211114=arima(ts_weight,               #best in terms of AIC#
                          order=c(2,1,1),
                          seasonal=list(order=c(1,1,4), period=7),
                          method="ML")

weight_sarima011011=arima(ts_weight,               #best in terms of BIC; our recommendation#
                          order=c(0,1,1),
                          seasonal=list(order=c(0,1,1), period=7),
                          method="ML")

hr_ar1
hr_ma1
weight_sarima211114
weight_sarima011011



#####5. Forecast#####
###5a. Weight Data###
forecast_weight_aic=predict(weight_sarima211114, 
                            n.ahead=nrow(test))

forecast_weight_aic=data.frame(
  date=seq(from=test$Date[1], to=test$Date[nrow(test)], by="day"),
  forecast=forecast_weight_aic$pred,
  lower=forecast_weight_aic$pred-1.96*forecast_weight_aic$se,
  upper=forecast_weight_aic$pred+1.96*forecast_weight_aic$se,
  observed=test$Weight,
  resid=test$Weight-forecast_weight_aic$pred
)

forecast_weight_bic=predict(weight_sarima011011, 
                            n.ahead=nrow(test))

forecast_weight_bic=data.frame(
  date=seq(from=test$Date[1], to=test$Date[nrow(test)], by="day"),
  forecast=forecast_weight_bic$pred,
  lower=forecast_weight_bic$pred-1.96*forecast_weight_bic$se,
  upper=forecast_weight_bic$pred+1.96*forecast_weight_bic$se,
  observed=test$Weight,
  resid=test$Weight-forecast_weight_bic$pred
)

rmse_weight_aic=(sum(forecast_weight_aic$resid^2)/nrow(forecast_weight_aic))^(0.5)
rmse_weight_bic=(sum(forecast_weight_bic$resid^2)/nrow(forecast_weight_aic))^(0.5)
rmse_weight_aic                                     #just curious#
rmse_weight_bic                                     #just curious#

forecast_weight_aic
forecast_weight_bic



###5b. Plot Weight Data###
ggplot() +
  geom_line(data=forecast_weight_bic, 
            aes(x=date, y=observed, color="Observed"),
            size=1.2) +
  geom_line(data=forecast_weight_bic, 
            aes(x=date, y=forecast, color="Forecast SARIMA(0,0,1)(0,1,1)7"),
            size=1.2) +
  geom_ribbon(data=forecast_weight_bic,
              aes(x=date, ymin=lower, ymax=upper),
              fill="blue",
              alpha=0.2) +
  geom_line(data=forecast_weight_aic, 
            aes(x=date, y=forecast, color="Forecast SARIMA(2,1,1)(1,1,4)7"),
            size=1.2) +
  geom_ribbon(data=forecast_weight_aic,
              aes(x=date, ymin=lower, ymax=upper),
              fill="red",
              alpha=0.2) +
  scale_color_manual(values=c("Observed"="green", 
                              "Forecast SARIMA(0,0,1)(0,1,1)7"="blue",
                              "Forecast SARIMA(2,1,1)(1,1,4)7"="red")) +
  labs(title="Weight Forecasts",
       x="Date", 
       y="Weight", 
       color="Series",
       fill="Interval") +
  theme_bw() +
  theme(plot.title=element_text(size=16, face="bold", hjust=0.5),      #center main title#
        axis.title.x=element_text(size=14),           
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position=c(0.15,0.85),                                   #x position (0,1), y position (0,1)#   
        legend.background=element_rect(fill="white",color="black"))     #put it in a black outlined box



###5c. HR Data###
forecast_hr_ar1=predict(hr_ar1, 
                        n.ahead=nrow(test))

forecast_hr_ar1=data.frame(
  date=seq(from=test$Date[1], to=test$Date[nrow(test)], by="day"),
  forecast=forecast_hr_ar1$pred,
  lower=forecast_hr_ar1$pred-1.96*forecast_hr_ar1$se,
  upper=forecast_hr_ar1$pred+1.96*forecast_hr_ar1$se,
  observed=test$HR,
  resid=test$HR-forecast_hr_ar1$pred
)

forecast_hr_ma1=predict(hr_ma1, 
                        n.ahead=nrow(test))

forecast_hr_ma1=data.frame(
  date=seq(from=test$Date[1], to=test$Date[nrow(test)], by="day"),
  forecast=forecast_hr_ma1$pred,
  lower=forecast_hr_ma1$pred-1.96*forecast_hr_ma1$se,
  upper=forecast_hr_ma1$pred+1.96*forecast_hr_ma1$se,
  observed=test$HR,
  resid=test$HR-forecast_hr_ma1$pred
)


rmse_hr_ar1=(sum(forecast_hr_ar1$resid^2)/nrow(forecast_hr_ar1))^(0.5)
rmse_hr_ma1=(sum(forecast_hr_ma1$resid^2)/nrow(forecast_hr_ma1))^(0.5)
rmse_hr_ar1                                          #just curious#
rmse_hr_ma1                                          #just curious#

forecast_hr_ar1
forecast_hr_ma1


###5d. Plot HR Data###
ggplot() +
  geom_line(data=forecast_hr_ar1, 
            aes(x=date, y=observed, color="Observed"),
            size=1.2) +
  geom_line(data=forecast_hr_ar1, 
            aes(x=date, y=forecast, color="Forecast AR(1)"),
            size=1.2) +
  geom_ribbon(data=forecast_hr_ar1,
              aes(x=date, ymin=lower, ymax=upper),
              fill="blue",
              alpha=0.2) +
  geom_line(data=forecast_hr_ma1, 
            aes(x=date, y=forecast, color="Forecast MA(1)"),
            size=1.2) +
  geom_ribbon(data=forecast_hr_ma1,
              aes(x=date, ymin=lower, ymax=upper),
              fill="red",
              alpha=0.2) +
  scale_color_manual(values=c("Observed"="green", 
                              "Forecast AR(1)"="blue",
                              "Forecast MA(1)"="red")) +
  labs(title="Heart Rate Forecasts",
       x="Date", 
       y="Weight", 
       color="Series",
       fill="Interval") +
  theme_bw() +
  theme(plot.title=element_text(size=16, face="bold", hjust=0.5),      #center main title#
        axis.title.x=element_text(size=14),           
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position=c(0.15,0.85),                                   #x position (0,1), y position (0,1)#   
        legend.background=element_rect(fill="white",color="black"))     #put it in a black outlined box
