---
title: "R Notebook on Sub-metering Data"
output: rmarkdown::github_document
---
Dataset:
Measurement of electric power consumption in one household with a one-minute sampling rate over a period of almost 4 years that can be found on:
http://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption

Reading/Background Article Sub-metering for Electricity located:
https://s3.amazonaws.com/gbstool/courses/793/docs/IV2p.pdf?AWSAccessKeyId=AKIAJBIZLMJQ2O6DKIAA&Expires=1500454800&Signature=pzpAYFsGeQGPZgsQ%2FvPr3BV3qVI%3D

Task: 
Given over 2 million minutes of continuous electric power comsumption, can we make a case for the cost with installing sub-metering in a household or business complex?
________________________________________________________________________________________________________________

Our focus will be using the dplyr package to make the dataset more easily understandable and analyzable in looking for trends.

Upon loading the data set, we take a summary to get an idea of the data. 

```r
summary(household_power_consumption)
```

```
##       Date                Time           Global_active_power
##  Min.   :2006-12-16   Length:2075259     Min.   : 0.076     
##  1st Qu.:2007-12-12   Class :character   1st Qu.: 0.308     
##  Median :2008-12-06   Mode  :character   Median : 0.602     
##  Mean   :2008-12-05                      Mean   : 1.092     
##  3rd Qu.:2009-12-01                      3rd Qu.: 1.528     
##  Max.   :2010-11-26                      Max.   :11.122     
##                                          NA's   :25979      
##  Global_reactive_power    Voltage      Global_intensity Sub_metering_1  
##  Min.   :0.000         Min.   :223.2   Min.   : 0.200   Min.   : 0.000  
##  1st Qu.:0.048         1st Qu.:239.0   1st Qu.: 1.400   1st Qu.: 0.000  
##  Median :0.100         Median :241.0   Median : 2.600   Median : 0.000  
##  Mean   :0.124         Mean   :240.8   Mean   : 4.628   Mean   : 1.122  
##  3rd Qu.:0.194         3rd Qu.:242.9   3rd Qu.: 6.400   3rd Qu.: 0.000  
##  Max.   :1.390         Max.   :254.2   Max.   :48.400   Max.   :88.000  
##  NA's   :25979         NA's   :25979   NA's   :25979    NA's   :25979   
##  Sub_metering_2   Sub_metering_3  
##  Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median : 1.000  
##  Mean   : 1.299   Mean   : 6.458  
##  3rd Qu.: 1.000   3rd Qu.:17.000  
##  Max.   :80.000   Max.   :31.000  
##  NA's   :25979    NA's   :25979
```

Note per my own personal norm, in order to preserve the integrity of the original dataset as well as the various modified versions, whenever drastic changes are made, the new dataframe is given a new name.

We first notice that the We observe that "Date" and time are identified as a character strings and not a value. Time by default needs to be connected to a date to be recognized as time. Thus, first we will change the "Date" column and then we will combine "Date" and "Time" into one column and move in front.


```r
household_power_consumption$Date<-as.Date(household_power_consumption$Date, "%d/%m/%Y" )
summary(household_power_consumption)
```

```
##       Date                Time           Global_active_power
##  Min.   :2006-12-16   Length:2075259     Min.   : 0.076     
##  1st Qu.:2007-12-12   Class :character   1st Qu.: 0.308     
##  Median :2008-12-06   Mode  :character   Median : 0.602     
##  Mean   :2008-12-05                      Mean   : 1.092     
##  3rd Qu.:2009-12-01                      3rd Qu.: 1.528     
##  Max.   :2010-11-26                      Max.   :11.122     
##                                          NA's   :25979      
##  Global_reactive_power    Voltage      Global_intensity Sub_metering_1  
##  Min.   :0.000         Min.   :223.2   Min.   : 0.200   Min.   : 0.000  
##  1st Qu.:0.048         1st Qu.:239.0   1st Qu.: 1.400   1st Qu.: 0.000  
##  Median :0.100         Median :241.0   Median : 2.600   Median : 0.000  
##  Mean   :0.124         Mean   :240.8   Mean   : 4.628   Mean   : 1.122  
##  3rd Qu.:0.194         3rd Qu.:242.9   3rd Qu.: 6.400   3rd Qu.: 0.000  
##  Max.   :1.390         Max.   :254.2   Max.   :48.400   Max.   :88.000  
##  NA's   :25979         NA's   :25979   NA's   :25979    NA's   :25979   
##  Sub_metering_2   Sub_metering_3  
##  Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median : 1.000  
##  Mean   : 1.299   Mean   : 6.458  
##  3rd Qu.: 1.000   3rd Qu.:17.000  
##  Max.   :80.000   Max.   :31.000  
##  NA's   :25979    NA's   :25979
```

```r
hpc_new<-cbind(household_power_consumption,paste(household_power_consumption$Date,household_power_consumption$Time), stringsAsFactors=FALSE)
colnames(hpc_new)[10] <-"DateTime"
hpc_new<- hpc_new[,c(ncol(hpc_new), 1:(ncol(hpc_new)-1))]
hpc_new$DateTime<-strptime(hpc_new$DateTime, format=" %Y-%m-%d %H:%M:%S")
summary(hpc_new)
```

```
##     DateTime                        Date                Time          
##  Min.   :2006-12-16 17:24:00   Min.   :2006-12-16   Length:2075259    
##  1st Qu.:2007-12-12 00:18:30   1st Qu.:2007-12-12   Class :character  
##  Median :2008-12-06 07:13:00   Median :2008-12-06   Mode  :character  
##  Mean   :2008-12-06 06:48:06   Mean   :2008-12-05                     
##  3rd Qu.:2009-12-01 14:07:30   3rd Qu.:2009-12-01                     
##  Max.   :2010-11-26 21:02:00   Max.   :2010-11-26                     
##  NA's   :240                                                          
##  Global_active_power Global_reactive_power    Voltage     
##  Min.   : 0.076      Min.   :0.000         Min.   :223.2  
##  1st Qu.: 0.308      1st Qu.:0.048         1st Qu.:239.0  
##  Median : 0.602      Median :0.100         Median :241.0  
##  Mean   : 1.092      Mean   :0.124         Mean   :240.8  
##  3rd Qu.: 1.528      3rd Qu.:0.194         3rd Qu.:242.9  
##  Max.   :11.122      Max.   :1.390         Max.   :254.2  
##  NA's   :25979       NA's   :25979         NA's   :25979  
##  Global_intensity Sub_metering_1   Sub_metering_2   Sub_metering_3  
##  Min.   : 0.200   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 1.400   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 2.600   Median : 0.000   Median : 0.000   Median : 1.000  
##  Mean   : 4.628   Mean   : 1.122   Mean   : 1.299   Mean   : 6.458  
##  3rd Qu.: 6.400   3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000  
##  Max.   :48.400   Max.   :88.000   Max.   :80.000   Max.   :31.000  
##  NA's   :25979    NA's   :25979    NA's   :25979    NA's   :25979
```

Finally, we notice there are NAs throughout the data. Given we will be doing some statistical analysis and do not want this to skew our results or cause errors in our functions, we will remove the NA's which will finish our initial preprocessing of the data.


```r
hpc_new_noNA<-na.omit(hpc_new)
summary(hpc_new_noNA)
```

```
##     DateTime                        Date                Time          
##  Min.   :2006-12-16 17:24:00   Min.   :2006-12-16   Length:2049280    
##  1st Qu.:2007-12-10 05:37:45   1st Qu.:2007-12-10   Class :character  
##  Median :2008-11-30 01:22:30   Median :2008-11-30   Mode  :character  
##  Mean   :2008-12-02 00:34:28   Mean   :2008-12-01                     
##  3rd Qu.:2009-11-23 20:31:15   3rd Qu.:2009-11-23                     
##  Max.   :2010-11-26 21:02:00   Max.   :2010-11-26                     
##  NA's   :240                                                          
##  Global_active_power Global_reactive_power    Voltage     
##  Min.   : 0.076      Min.   :0.0000        Min.   :223.2  
##  1st Qu.: 0.308      1st Qu.:0.0480        1st Qu.:239.0  
##  Median : 0.602      Median :0.1000        Median :241.0  
##  Mean   : 1.092      Mean   :0.1237        Mean   :240.8  
##  3rd Qu.: 1.528      3rd Qu.:0.1940        3rd Qu.:242.9  
##  Max.   :11.122      Max.   :1.3900        Max.   :254.2  
##                                                           
##  Global_intensity Sub_metering_1   Sub_metering_2   Sub_metering_3  
##  Min.   : 0.200   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 1.400   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 2.600   Median : 0.000   Median : 0.000   Median : 1.000  
##  Mean   : 4.628   Mean   : 1.122   Mean   : 1.299   Mean   : 6.458  
##  3rd Qu.: 6.400   3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000  
##  Max.   :48.400   Max.   :88.000   Max.   :80.000   Max.   :31.000  
## 
```

Now that our data is pretty clean and manageable, we will consider the task at hand.

Our goal is to see if we can give an argument to whether Sub-metering is would be cost efficient to install in an apartment complex based on the readings we have from the electricity usage of a home with sub-metering.

First off, we note there are three sub-meters sections which report electricity usage in watts per minute and that the global active power (GAP) reading in active power in kilowatts for minute averaged. Thus, if we would want to calculate the total miscellaneous electricity used not reported in by the sub-metering we would need to convert the GAP units to watt per minute. Then we would take the difference of GAP in watts per minute and the total of three sub-meters.


```r
hpc_rev<-hpc_new_noNA
hpc_rev$GAP_WtPerMin<-hpc_rev$Global_active_power*1000/60
hpc_rev$Sub_Met_Total<-hpc_rev$Sub_metering_1+hpc_rev$Sub_metering_2+hpc_rev$Sub_metering_3
hpc_rev$GAP_SubMet_Diff<-hpc_rev$GAP_WtPerMin-hpc_rev$Sub_Met_Total
summary(hpc_rev)
```

```
##     DateTime                        Date                Time          
##  Min.   :2006-12-16 17:24:00   Min.   :2006-12-16   Length:2049280    
##  1st Qu.:2007-12-10 05:37:45   1st Qu.:2007-12-10   Class :character  
##  Median :2008-11-30 01:22:30   Median :2008-11-30   Mode  :character  
##  Mean   :2008-12-02 00:34:28   Mean   :2008-12-01                     
##  3rd Qu.:2009-11-23 20:31:15   3rd Qu.:2009-11-23                     
##  Max.   :2010-11-26 21:02:00   Max.   :2010-11-26                     
##  NA's   :240                                                          
##  Global_active_power Global_reactive_power    Voltage     
##  Min.   : 0.076      Min.   :0.0000        Min.   :223.2  
##  1st Qu.: 0.308      1st Qu.:0.0480        1st Qu.:239.0  
##  Median : 0.602      Median :0.1000        Median :241.0  
##  Mean   : 1.092      Mean   :0.1237        Mean   :240.8  
##  3rd Qu.: 1.528      3rd Qu.:0.1940        3rd Qu.:242.9  
##  Max.   :11.122      Max.   :1.3900        Max.   :254.2  
##                                                           
##  Global_intensity Sub_metering_1   Sub_metering_2   Sub_metering_3  
##  Min.   : 0.200   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 1.400   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 2.600   Median : 0.000   Median : 0.000   Median : 1.000  
##  Mean   : 4.628   Mean   : 1.122   Mean   : 1.299   Mean   : 6.458  
##  3rd Qu.: 6.400   3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000  
##  Max.   :48.400   Max.   :88.000   Max.   :80.000   Max.   :31.000  
##                                                                     
##   GAP_WtPerMin     Sub_Met_Total     GAP_SubMet_Diff  
##  Min.   :  1.267   Min.   :  0.000   Min.   : -2.400  
##  1st Qu.:  5.133   1st Qu.:  0.000   1st Qu.:  3.800  
##  Median : 10.033   Median :  1.000   Median :  5.500  
##  Mean   : 18.194   Mean   :  8.879   Mean   :  9.315  
##  3rd Qu.: 25.467   3rd Qu.: 18.000   3rd Qu.: 10.367  
##  Max.   :185.367   Max.   :134.000   Max.   :124.833  
## 
```

Now that it looks like we have all the data needed to start our analysis, 

There are various differnt ways to analyze data and look for trends. Given the data is provided sequentially by minutes but electricity is usually billed monthly, it would make sense to group the minutes by months. In order to do this, we would first need to break down the date by month. 


```r
hpc_rev$Month <- as.numeric(format(as.Date(hpc_rev$Date), "%m"))
hpc_rev$Month_Yr <- as.character(format(as.Date(hpc_rev$Date), "%Y-%m"))
summary(hpc_rev)
```

```
##     DateTime                        Date                Time          
##  Min.   :2006-12-16 17:24:00   Min.   :2006-12-16   Length:2049280    
##  1st Qu.:2007-12-10 05:37:45   1st Qu.:2007-12-10   Class :character  
##  Median :2008-11-30 01:22:30   Median :2008-11-30   Mode  :character  
##  Mean   :2008-12-02 00:34:28   Mean   :2008-12-01                     
##  3rd Qu.:2009-11-23 20:31:15   3rd Qu.:2009-11-23                     
##  Max.   :2010-11-26 21:02:00   Max.   :2010-11-26                     
##  NA's   :240                                                          
##  Global_active_power Global_reactive_power    Voltage     
##  Min.   : 0.076      Min.   :0.0000        Min.   :223.2  
##  1st Qu.: 0.308      1st Qu.:0.0480        1st Qu.:239.0  
##  Median : 0.602      Median :0.1000        Median :241.0  
##  Mean   : 1.092      Mean   :0.1237        Mean   :240.8  
##  3rd Qu.: 1.528      3rd Qu.:0.1940        3rd Qu.:242.9  
##  Max.   :11.122      Max.   :1.3900        Max.   :254.2  
##                                                           
##  Global_intensity Sub_metering_1   Sub_metering_2   Sub_metering_3  
##  Min.   : 0.200   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 1.400   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 2.600   Median : 0.000   Median : 0.000   Median : 1.000  
##  Mean   : 4.628   Mean   : 1.122   Mean   : 1.299   Mean   : 6.458  
##  3rd Qu.: 6.400   3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000  
##  Max.   :48.400   Max.   :88.000   Max.   :80.000   Max.   :31.000  
##                                                                     
##   GAP_WtPerMin     Sub_Met_Total     GAP_SubMet_Diff       Month       
##  Min.   :  1.267   Min.   :  0.000   Min.   : -2.400   Min.   : 1.000  
##  1st Qu.:  5.133   1st Qu.:  0.000   1st Qu.:  3.800   1st Qu.: 3.000  
##  Median : 10.033   Median :  1.000   Median :  5.500   Median : 6.000  
##  Mean   : 18.194   Mean   :  8.879   Mean   :  9.315   Mean   : 6.454  
##  3rd Qu.: 25.467   3rd Qu.: 18.000   3rd Qu.: 10.367   3rd Qu.: 9.000  
##  Max.   :185.367   Max.   :134.000   Max.   :124.833   Max.   :12.000  
##                                                                        
##    Month_Yr        
##  Length:2049280    
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

Now that we have the data grouped by months, we want to compare the different electricity usage. Since months have different number of days, it is better to take the average than take sums.

Now using the dplyr package in R we can group the data while taking the average usages.


```r
hpc_rev_Mo_Yr_Mean_Summary<-hpc_rev %>% group_by(Month_Yr) %>% summarise(GAP_Mean=mean(Global_active_power), GRP_Mean=mean(Global_reactive_power), Voltage_Mean=mean(Voltage), GI_Mean=mean(Global_intensity), SubM1_Mean=mean(Sub_metering_1),  SubM2_Mean=mean(Sub_metering_2), SubM3_Mean=mean(Sub_metering_3), GAP_WtPerMin_Mean=mean(GAP_WtPerMin), GAP_SubMet_Diff_Mean=mean(GAP_SubMet_Diff))
```

```
## Error in grouped_df_impl(data, unname(vars), drop): Column `DateTime` is of unsupported class POSIXlt/POSIXt
```

```r
hpc_rev_Mo_Yr_Mean_Summary
```

```
## # A tibble: 48 x 10
##    Month_Yr  GAP_Mean  GRP_Mean Voltage_Mean  GI_Mean SubM1_Mean
##       <chr>     <dbl>     <dbl>        <dbl>    <dbl>      <dbl>
##  1  2006-12 1.9012951 0.1313858     241.4411 8.029956  1.2486359
##  2  2007-01 1.5460339 0.1326761     240.9051 6.546915  1.2642367
##  3  2007-02 1.4010835 0.1136368     240.5194 5.914569  1.1802173
##  4  2007-03 1.3186270 0.1147468     240.5135 5.572979  1.3613432
##  5  2007-04 0.8911889 0.1187779     239.4000 3.825676  1.0658865
##  6  2007-05 0.9858618 0.1153426     235.1784 4.297464  1.6966174
##  7  2007-06 0.8268144 0.1463953     238.8755 3.603550  1.3826726
##  8  2007-07 0.6673668 0.1274812     237.6712 2.944133  0.9672650
##  9  2007-08 0.7641862 0.1128164     237.9372 3.312668  0.8124748
## 10  2007-09 0.9693182 0.1260108     239.4241 4.174610  1.2232279
## # ... with 38 more rows, and 4 more variables: SubM2_Mean <dbl>,
## #   SubM3_Mean <dbl>, GAP_WtPerMin_Mean <dbl>, GAP_SubMet_Diff_Mean <dbl>
```

Now we have grouped by months chronologically, we will also group by only months in order to be able to see trends of each month across the years.


```r
hpc_rev_Mo_Mean_Summary<-hpc_rev %>% group_by(Month) %>% summarise(GAP_Mean=mean(Global_active_power), GRP_Mean=mean(Global_reactive_power), Voltage_Mean=mean(Voltage), GI_Mean=mean(Global_intensity), SubM1_Mean=mean(Sub_metering_1),  SubM2_Mean=mean(Sub_metering_2), SubM3_Mean=mean(Sub_metering_3), GAP_WtPerMin_Mean=mean(GAP_WtPerMin), GAP_SubMet_Diff_Mean=mean(GAP_SubMet_Diff))
```

```
## Error in grouped_df_impl(data, unname(vars), drop): Column `DateTime` is of unsupported class POSIXlt/POSIXt
```

```r
hpc_rev_Mo_Mean_Summary
```

```
## # A tibble: 12 x 10
##    Month  GAP_Mean   GRP_Mean Voltage_Mean  GI_Mean SubM1_Mean SubM2_Mean
##    <dbl>     <dbl>      <dbl>        <dbl>    <dbl>      <dbl>      <dbl>
##  1     1 1.4622255 0.11072100     242.1776 6.138727  1.4064814  1.5488385
##  2     2 1.3004307 0.09966128     241.4594 5.447099  1.1321435  1.3865686
##  3     3 1.2313426 0.11125653     241.5286 5.176797  1.2667210  1.6616986
##  4     4 1.0471457 0.12277922     241.0447 4.424004  1.0756237  1.1732437
##  5     5 1.0295708 0.12790282     239.0085 4.403161  1.2349162  1.3078134
##  6     6 0.9091479 0.14625425     239.9124 3.926679  1.3308370  1.2444098
##  7     7 0.7003589 0.15188916     240.0037 3.062319  0.7076370  0.9922319
##  8     8 0.5728124 0.13944571     240.1738 2.514592  0.5084292  0.8049279
##  9     9 0.9756525 0.13287515     240.4523 4.169791  1.1559221  1.1847778
## 10    10 1.1371413 0.11702725     240.7773 4.795653  1.0656815  1.4849955
## 11    11 1.2915322 0.10878691     241.0422 5.438691  1.2915535  1.3503545
## 12    12 1.4897289 0.11359724     242.7491 6.225546  1.3119907  1.4411889
## # ... with 3 more variables: SubM3_Mean <dbl>, GAP_WtPerMin_Mean <dbl>,
## #   GAP_SubMet_Diff_Mean <dbl>
```

Now using ggplot, we can use the R visualization package to try and get a better handle on the trends on the data.


```r
hpc_rev_Mo_Yr_Mean_Summary%>% gather(key, Watts, GAP_WtPerMin_Mean,SubM1_Mean, SubM2_Mean, SubM3_Mean,GAP_SubMet_Diff_Mean) %>% ggplot(aes(group =key, x=Month_Yr,y=Watts,colour=key))+geom_line()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![plot of chunk unnamed-chunk-38](figure/unnamed-chunk-38-1.png)

This gives a good idea of the average Watts/Minute electricity usage. One thing that is immediately apparent is that the majority of electricity used is not reported by the sub-metered sections but is miscellaneous, i.e. the GAP_SubMet_Diff_Mean, usage reported. This means based off of the current data reported it would be difficult to make any solid case for using sub-metering.

Nonetheless, let us also investigate the trends we can see by grouping the data by only months.


```r
hpc_rev_Mo_Mean_Summary %>% gather(key, Watts, GAP_WtPerMin_Mean,SubM1_Mean, SubM2_Mean, SubM3_Mean,GAP_SubMet_Diff_Mean) %>% ggplot(aes(group=key, x=Month,y=Watts,colour=key))+geom_line()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_x_discrete(name="Months Grouped By Year",limits=c(1,3,6,9,12))
```

![plot of chunk unnamed-chunk-39](figure/unnamed-chunk-39-1.png)

As we immediately can see, while the graph is useful and we can easily see that during the November to early February, the electricity usage is the highest. And from June to late August, it reaches the minimum usage, this 
