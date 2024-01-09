Consumer Usage Trends in Fitness Wearables
================

### The Business Question

How do consumers use smart fitness wearables?

### Analysis Objective:

Identify insights and trends in fitness smart device usage.

### Business Objective:

To use these insights to drive future marketing of its own line of
fitness devices.

### Stakeholders

Bellabeat - Urska Srsen: Co-founder and Chief Creative Officers - Sando
Mur: Co-founder and member of Bellabeat executive team. - Marketing
Analytics Team

## Step 1: Load Packages

``` r
library(tidyverse) # includes ggplot2 to plot/visualize
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr) # allows us to aggregate, manipulate, and analyze
library(readr) #allows us to import the .csv files
library(here) #allows for easy file references when importing and exporting
```

    ## here() starts at C:/Users/abdulkarim ussery/Documents/R_Scripts/Projects/Fitness_tracking_capstone

``` r
library(janitor) #useful tools and functions for cleaning
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(stringr) #allows us to check string length 
```

## Step 2: Import files

Based on the business question and the dataset, the key metrics we’ve
decided to use in our analysis are activity, steps, and sleep. We may
add/aggregate others if we see the need. We’re going to import the
dailyActivity file, dailySteps file, and sleepDay file”

## Step 3: Clean data

### 3a: Check blanks/nulls/truncated data. Address via “complete or delete”.

``` r
lapply(lapply(daily_activity,is.na),table)
```

    ## $Id
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $ActivityDate
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $TotalSteps
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $TotalDistance
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $TrackerDistance
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $LoggedActivitiesDistance
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $VeryActiveDistance
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $ModeratelyActiveDistance
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $LightActiveDistance
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $SedentaryActiveDistance
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $VeryActiveMinutes
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $FairlyActiveMinutes
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $LightlyActiveMinutes
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $SedentaryMinutes
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $Calories
    ## 
    ## FALSE 
    ##   940

``` r
lapply(lapply(daily_steps,is.na),table)
```

    ## $Id
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $ActivityDay
    ## 
    ## FALSE 
    ##   940 
    ## 
    ## $StepTotal
    ## 
    ## FALSE 
    ##   940

``` r
lapply(lapply(sleep_day,is.na),table) 
```

    ## $Id
    ## 
    ## FALSE 
    ##   413 
    ## 
    ## $SleepDay
    ## 
    ## FALSE 
    ##   413 
    ## 
    ## $TotalSleepRecords
    ## 
    ## FALSE 
    ##   413 
    ## 
    ## $TotalMinutesAsleep
    ## 
    ## FALSE 
    ##   413 
    ## 
    ## $TotalTimeInBed
    ## 
    ## FALSE 
    ##   413

### 3b Check for and remove duplicates

``` r
nrow(daily_activity)
```

    ## [1] 940

``` r
n_distinct(daily_activity)
```

    ## [1] 940

``` r
nrow(daily_steps)
```

    ## [1] 940

``` r
n_distinct(daily_steps)
```

    ## [1] 940

``` r
nrow(sleep_day)
```

    ## [1] 413

``` r
n_distinct(sleep_day)
```

    ## [1] 410

There are 3 duplicates in the sleep_day dataframe 413 vs 410

``` r
sum(duplicated(sleep_day))
```

    ## [1] 3

Indeed there are three. Let’s drop them and confirm.

``` r
sleep_day <- distinct(sleep_day)
nrow(sleep_day)
```

    ## [1] 410

``` r
n_distinct(sleep_day)
```

    ## [1] 410

### 3c: ensure consistent and clean columns

``` r
daily_activity <- clean_names(daily_activity)
daily_steps <- clean_names(daily_steps)
daily_sleep <- clean_names(sleep_day)

remove(sleep_day)
```

### 3d: ensure consistent typcast

``` r
str(daily_activity)
```

    ## 'data.frame':    940 obs. of  15 variables:
    ##  $ id                        : num  1.5e+09 1.5e+09 1.5e+09 1.5e+09 1.5e+09 ...
    ##  $ activity_date             : chr  "4/12/2016" "4/13/2016" "4/14/2016" "4/15/2016" ...
    ##  $ total_steps               : int  13162 10735 10460 9762 12669 9705 13019 15506 10544 9819 ...
    ##  $ total_distance            : num  8.5 6.97 6.74 6.28 8.16 ...
    ##  $ tracker_distance          : num  8.5 6.97 6.74 6.28 8.16 ...
    ##  $ logged_activities_distance: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ very_active_distance      : num  1.88 1.57 2.44 2.14 2.71 ...
    ##  $ moderately_active_distance: num  0.55 0.69 0.4 1.26 0.41 ...
    ##  $ light_active_distance     : num  6.06 4.71 3.91 2.83 5.04 ...
    ##  $ sedentary_active_distance : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ very_active_minutes       : int  25 21 30 29 36 38 42 50 28 19 ...
    ##  $ fairly_active_minutes     : int  13 19 11 34 10 20 16 31 12 8 ...
    ##  $ lightly_active_minutes    : int  328 217 181 209 221 164 233 264 205 211 ...
    ##  $ sedentary_minutes         : int  728 776 1218 726 773 539 1149 775 818 838 ...
    ##  $ calories                  : int  1985 1797 1776 1745 1863 1728 1921 2035 1786 1775 ...

``` r
str(daily_steps)
```

    ## 'data.frame':    940 obs. of  3 variables:
    ##  $ id          : num  1.5e+09 1.5e+09 1.5e+09 1.5e+09 1.5e+09 ...
    ##  $ activity_day: chr  "4/12/2016" "4/13/2016" "4/14/2016" "4/15/2016" ...
    ##  $ step_total  : int  13162 10735 10460 9762 12669 9705 13019 15506 10544 9819 ...

``` r
str(daily_sleep)
```

    ## 'data.frame':    410 obs. of  5 variables:
    ##  $ id                  : num  1.5e+09 1.5e+09 1.5e+09 1.5e+09 1.5e+09 ...
    ##  $ sleep_day           : chr  "4/12/2016 12:00:00 AM" "4/13/2016 12:00:00 AM" "4/15/2016 12:00:00 AM" "4/16/2016 12:00:00 AM" ...
    ##  $ total_sleep_records : int  1 2 1 2 1 1 1 1 1 1 ...
    ##  $ total_minutes_asleep: int  327 384 412 340 700 304 360 325 361 430 ...
    ##  $ total_time_in_bed   : int  346 407 442 367 712 320 377 364 384 449 ...

Everything looks fine except the dates. All of them are formatted as
text. Let’s convert all dates to a standard date format. While we’re at
it, let’s rename the date columns in all three to be consistent.

``` r
daily_activity <- daily_activity %>% 
  rename(date = activity_date) %>% 
  mutate(date = as_date(date, format = '%m/%d/%Y')) # since what we're converting is a string, we need to identify the month, day, year, hour, etc as well as that which they are delimited by.

daily_steps <- daily_steps %>% 
  rename(date = activity_day) %>% 
  mutate(date = as_date(date, format = '%m/%d/%Y'))

daily_sleep <- daily_sleep %>% 
  rename(date = sleep_day) %>% 
  mutate(date = as_date(date, format = '%m/%d/%Y %I:%M:%S %p',tz=Sys.timezone())) #we had to let it know that "12:00:00 AM" is 12-hour decimal hours (%I)
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p", tz =
    ##   Sys.timezone())`.
    ## Caused by warning:
    ## ! `tz` argument is ignored by `as_date()`

``` r
# and not 24-hour (%H), that the "AM" is the locale/specific AM/PM.
```

### 3e: Ensure consistent strings

``` r
str(daily_activity)
```

    ## 'data.frame':    940 obs. of  15 variables:
    ##  $ id                        : num  1.5e+09 1.5e+09 1.5e+09 1.5e+09 1.5e+09 ...
    ##  $ date                      : Date, format: "2016-04-12" "2016-04-13" ...
    ##  $ total_steps               : int  13162 10735 10460 9762 12669 9705 13019 15506 10544 9819 ...
    ##  $ total_distance            : num  8.5 6.97 6.74 6.28 8.16 ...
    ##  $ tracker_distance          : num  8.5 6.97 6.74 6.28 8.16 ...
    ##  $ logged_activities_distance: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ very_active_distance      : num  1.88 1.57 2.44 2.14 2.71 ...
    ##  $ moderately_active_distance: num  0.55 0.69 0.4 1.26 0.41 ...
    ##  $ light_active_distance     : num  6.06 4.71 3.91 2.83 5.04 ...
    ##  $ sedentary_active_distance : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ very_active_minutes       : int  25 21 30 29 36 38 42 50 28 19 ...
    ##  $ fairly_active_minutes     : int  13 19 11 34 10 20 16 31 12 8 ...
    ##  $ lightly_active_minutes    : int  328 217 181 209 221 164 233 264 205 211 ...
    ##  $ sedentary_minutes         : int  728 776 1218 726 773 539 1149 775 818 838 ...
    ##  $ calories                  : int  1985 1797 1776 1745 1863 1728 1921 2035 1786 1775 ...

``` r
str(daily_sleep)
```

    ## 'data.frame':    410 obs. of  5 variables:
    ##  $ id                  : num  1.5e+09 1.5e+09 1.5e+09 1.5e+09 1.5e+09 ...
    ##  $ date                : Date, format: "2016-04-12" "2016-04-13" ...
    ##  $ total_sleep_records : int  1 2 1 2 1 1 1 1 1 1 ...
    ##  $ total_minutes_asleep: int  327 384 412 340 700 304 360 325 361 430 ...
    ##  $ total_time_in_bed   : int  346 407 442 367 712 320 377 364 384 449 ...

``` r
str(daily_steps)
```

    ## 'data.frame':    940 obs. of  3 variables:
    ##  $ id        : num  1.5e+09 1.5e+09 1.5e+09 1.5e+09 1.5e+09 ...
    ##  $ date      : Date, format: "2016-04-12" "2016-04-13" ...
    ##  $ step_total: int  13162 10735 10460 9762 12669 9705 13019 15506 10544 9819 ...

It seems the id number should be 10 digits. Let’s check.

``` r
str_length(unique(daily_activity$id))
```

    ##  [1] 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
    ## [26] 10 10 10 10 10 10 10 10

``` r
str_length(unique(daily_sleep$id))
```

    ##  [1] 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10

``` r
str_length(unique(daily_steps$id))
```

    ##  [1] 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
    ## [26] 10 10 10 10 10 10 10 10

## Step 4: Exploratory data analysis

\###Q1: How many users do we have?

``` r
n_distinct(daily_activity$id) # (All) 33 users provided daily activity data.
```

    ## [1] 33

``` r
n_distinct(daily_steps$id) # ditto for step data. No surprise as step data is one of the variables in daily activity.
```

    ## [1] 33

``` r
n_distinct(daily_sleep$id) # 24/33 provided sleep data. 
```

    ## [1] 24

\###Q2: Are the observations in `daily_steps` redundant with regards to
those in `daily_activity`?

``` r
daily_activity %>% 
  select(total_steps) %>% 
  summary()
```

    ##   total_steps   
    ##  Min.   :    0  
    ##  1st Qu.: 3790  
    ##  Median : 7406  
    ##  Mean   : 7638  
    ##  3rd Qu.:10727  
    ##  Max.   :36019

``` r
daily_steps %>% 
  select(step_total) %>% 
  summary() 
```

    ##    step_total   
    ##  Min.   :    0  
    ##  1st Qu.: 3790  
    ##  Median : 7406  
    ##  Mean   : 7638  
    ##  3rd Qu.:10727  
    ##  Max.   :36019

They are the exact same. So, we will exclude daily_steps from our
analysis as the data is redundant.

``` r
remove(daily_steps)
```

\###Q3: How many days were the users tracking data?

``` r
n_distinct(daily_activity$date) # 31 days
```

    ## [1] 31

``` r
n_distinct(daily_sleep$date) # 31 days
```

    ## [1] 31

\###Q4: Do the activity and sedentary minutes for each day sum to a
specific amount?

``` r
total_activity_min <- daily_activity %>% 
  group_by(id) %>% 
  filter(date == '2016-04-12') %>% 
  summarize(sum(very_active_minutes,fairly_active_minutes,lightly_active_minutes,sedentary_minutes))

view(total_activity_min)

total_activity_min %>% 
  select(`sum(...)`) %>% 
  summary() # min = 789, mean = 1255, 3rd qu = 1440, and max =1440
```

    ##     sum(...)   
    ##  Min.   : 789  
    ##  1st Qu.: 983  
    ##  Median :1440  
    ##  Mean   :1255  
    ##  3rd Qu.:1440  
    ##  Max.   :1440

``` r
n_distinct(total_activity_min$`sum(...)`) # returns a count of 15
```

    ## [1] 15

``` r
n_distinct(total_activity_min$id) # returns a count of 33
```

    ## [1] 33

``` r
nrow(total_activity_min)
```

    ## [1] 33

``` r
nrow(total_activity_min[total_activity_min$`sum(...)` == '1440',]) # count is 19. 
```

    ## [1] 19

Now we know that out of the 33 users, 19 had 1440 total activity minutes
measured for 2016-04-12. What’s the significance of this number?

``` r
1440/60
```

    ## [1] 24

Equals 24 hours. So, that means that if a user’s activity minutes on any
given day totals 1440, they’ve tracked an entire 24hr period.
Interesting. 19/33 have done so on the first day. Later on in our
analysis, we can aggregate these “full-time” users if we choose to.

\###Q5: Do sleep minutes and sedentary minutes overlap?

Answer: Logically speaking, they do. Full-time use of the device means
that all activity minutes in a day = all minutes in a day (see previous
question). A portion of the day’s minutes are minutes spent sleeping and
a portion of all activity minutes are sedentary. Sleeping is a sedentary
activity, Therefore, sleeping minutes should be considered sedentary
minutes. The expectation is that a) sedentary minutes should exceed
sleeping minutes and b) sleeping minutes should never exceed sedentary
minutes. Let’s test this by first combining the two dataframes, then
getting a statistical summary of it:

``` r
daily_activity_sleep <- merge(daily_activity,daily_sleep, by = c('id','date'))
glimpse(daily_activity_sleep) 
```

    ## Rows: 410
    ## Columns: 18
    ## $ id                         <dbl> 1503960366, 1503960366, 1503960366, 1503960…
    ## $ date                       <date> 2016-04-12, 2016-04-13, 2016-04-15, 2016-0…
    ## $ total_steps                <int> 13162, 10735, 9762, 12669, 9705, 15506, 105…
    ## $ total_distance             <dbl> 8.50, 6.97, 6.28, 8.16, 6.48, 9.88, 6.68, 6…
    ## $ tracker_distance           <dbl> 8.50, 6.97, 6.28, 8.16, 6.48, 9.88, 6.68, 6…
    ## $ logged_activities_distance <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ very_active_distance       <dbl> 1.88, 1.57, 2.14, 2.71, 3.19, 3.53, 1.96, 1…
    ## $ moderately_active_distance <dbl> 0.55, 0.69, 1.26, 0.41, 0.78, 1.32, 0.48, 0…
    ## $ light_active_distance      <dbl> 6.06, 4.71, 2.83, 5.04, 2.51, 5.03, 4.24, 4…
    ## $ sedentary_active_distance  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ very_active_minutes        <int> 25, 21, 29, 36, 38, 50, 28, 19, 41, 39, 73,…
    ## $ fairly_active_minutes      <int> 13, 19, 34, 10, 20, 31, 12, 8, 21, 5, 14, 2…
    ## $ lightly_active_minutes     <int> 328, 217, 209, 221, 164, 264, 205, 211, 262…
    ## $ sedentary_minutes          <int> 728, 776, 726, 773, 539, 775, 818, 838, 732…
    ## $ calories                   <int> 1985, 1797, 1745, 1863, 1728, 2035, 1786, 1…
    ## $ total_sleep_records        <int> 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ total_minutes_asleep       <int> 327, 384, 412, 340, 700, 304, 360, 325, 361…
    ## $ total_time_in_bed          <int> 346, 407, 442, 367, 712, 320, 377, 364, 384…

``` r
daily_activity_sleep %>% 
  select(sedentary_minutes,total_minutes_asleep) %>% 
  summary()
```

    ##  sedentary_minutes total_minutes_asleep
    ##  Min.   :   0.0    Min.   : 58.0       
    ##  1st Qu.: 631.2    1st Qu.:361.0       
    ##  Median : 717.0    Median :432.5       
    ##  Mean   : 712.1    Mean   :419.2       
    ##  3rd Qu.: 782.8    3rd Qu.:490.0       
    ##  Max.   :1265.0    Max.   :796.0

A statistical summary analysis reveals that sedentary minutes exceed
sleeping minutes by a fair amount. However, the minimums reveal that the
minimum of the sedentary_minutes column is 0 while the minimum of the
total_minutes_asleep is 58. There could be any number of reasons for
this anomaly. Let’s just take the average of each user. This will also
double as useful info we want to move forward with in our analysis:

``` r
test_calc_track <-cbind(daily_activity_sleep,
                        total_tracking_min = rowSums(daily_activity_sleep[11:14]))  
  
test_calc_act <- cbind(test_calc_track,total_min_active = rowSums(test_calc_track[11:13]))

head(test_calc_act)
```

    ##           id       date total_steps total_distance tracker_distance
    ## 1 1503960366 2016-04-12       13162           8.50             8.50
    ## 2 1503960366 2016-04-13       10735           6.97             6.97
    ## 3 1503960366 2016-04-15        9762           6.28             6.28
    ## 4 1503960366 2016-04-16       12669           8.16             8.16
    ## 5 1503960366 2016-04-17        9705           6.48             6.48
    ## 6 1503960366 2016-04-19       15506           9.88             9.88
    ##   logged_activities_distance very_active_distance moderately_active_distance
    ## 1                          0                 1.88                       0.55
    ## 2                          0                 1.57                       0.69
    ## 3                          0                 2.14                       1.26
    ## 4                          0                 2.71                       0.41
    ## 5                          0                 3.19                       0.78
    ## 6                          0                 3.53                       1.32
    ##   light_active_distance sedentary_active_distance very_active_minutes
    ## 1                  6.06                         0                  25
    ## 2                  4.71                         0                  21
    ## 3                  2.83                         0                  29
    ## 4                  5.04                         0                  36
    ## 5                  2.51                         0                  38
    ## 6                  5.03                         0                  50
    ##   fairly_active_minutes lightly_active_minutes sedentary_minutes calories
    ## 1                    13                    328               728     1985
    ## 2                    19                    217               776     1797
    ## 3                    34                    209               726     1745
    ## 4                    10                    221               773     1863
    ## 5                    20                    164               539     1728
    ## 6                    31                    264               775     2035
    ##   total_sleep_records total_minutes_asleep total_time_in_bed total_tracking_min
    ## 1                   1                  327               346               1094
    ## 2                   2                  384               407               1033
    ## 3                   1                  412               442                998
    ## 4                   2                  340               367               1040
    ## 5                   1                  700               712                761
    ## 6                   1                  304               320               1120
    ##   total_min_active
    ## 1              366
    ## 2              257
    ## 3              272
    ## 4              267
    ## 5              222
    ## 6              345

``` r
test_calc_ut <- cbind(test_calc_act,uptime_percent = round((rowSums(daily_activity_sleep[11:14])/1440)*100))

daily_activity_sleep <- test_calc_ut


daily_avg <- daily_activity_sleep %>% 
  group_by(id) %>% 
  summarise(avg_steps=round(mean(total_steps)),avg_tracking_min=round(mean(total_tracking_min)),
            avg_uptime_percent = round(mean(uptime_percent)),
            avg_active_min=round(mean(total_min_active)),
            avg_sedentary_min=round(mean(sedentary_minutes)),
            avg_sleep_min=round(mean(total_minutes_asleep)))

view(daily_avg) 

daily_avg %>% 
  filter(avg_sleep_min > avg_sedentary_min)
```

    ## # A tibble: 1 × 7
    ##           id avg_steps avg_tracking_min avg_uptime_percent avg_active_min
    ##        <dbl>     <dbl>            <dbl>              <dbl>          <dbl>
    ## 1 1844505072      3477              591                 41            147
    ## # ℹ 2 more variables: avg_sedentary_min <dbl>, avg_sleep_min <dbl>

In 1 out of 24 cases, the sleep exceeds the sedentary. It appears to be
an anomaly.

## Step 4: Analysis: Observation of trends and relationships

### Observation 1: The more steps users took, the more active they became.

![steps_v_activity](https://github.com/carlussery/Fitness_device_trends/assets/153660397/141dda2f-4e68-4bb7-9326-c8d4f654f4b7)


### IMPORTANT:

Surprisingly, the inverse isn’t as true. There is only a very slight
negative correlation between sedentary minutes and steps taken
(i.e. taking more steps doesn’t necessarily equate to less sedentary
minutes). Perhaps the reason is that being sedentary for long periods of
time is not a trait exclusive to those who don’t walk. For instance,
career choice may play a role.
![steps_v_sedentary](https://github.com/carlussery/Fitness_device_trends/assets/153660397/415f573a-5123-468e-a50c-1c993561855d)


### Observation 2: The more sedentary users became, the LESS they tended to sleep.
![sedentary_v_sleep](https://github.com/carlussery/Fitness_device_trends/assets/153660397/cd97df00-1214-4492-95aa-d066ad5a71eb)


### Observation 3: Users who wore their devices longer (tracking more activity) tended to sleep less.
![uptime_v_sleep](https://github.com/carlussery/Fitness_device_trends/assets/153660397/2a2cbb9f-caf5-417e-a183-ed8079835d96)


### Observation 4: Users who wore their devices longer (tracking more activity) tended to be more sedentary.
![uptime_v_sedentary](https://github.com/carlussery/Fitness_device_trends/assets/153660397/ea4d7dc8-6c54-4685-9a47-70f180cfc931)


### Observation 5: Users who tracked and logged more activity tended to sleeep less.
![sleep_v_activity](https://github.com/carlussery/Fitness_device_trends/assets/153660397/f32344a2-c60f-4013-b21a-f2f8d42c2ffb)

### Observation 6: The more active users were, the more calories they tended to burn.
![activity_v_calories](https://github.com/carlussery/Fitness_device_trends/assets/153660397/e9bc4217-e27c-49d9-a64d-cd69ab8db3f9)



### Observation 7: This is no strong negative relationship between activte minutes and sedentary minutes.

What this means in plain English is that based on this data, we can’t
definitively say that high sedentary time equates to low active time and
vice versa. Too many variable come into play such as lifestyle, training
style, etc.  
![](Bellabeat_cleaning_and_analysis_files/figure-gfm/observation%207-1.png)<!-- -->

There were no more identifiable trends based on the key metrics chosen
for this analysis.

## Recommendations

Based upon the results of the analysis we recommend the following:

1.  Segment smart device users based upon their goals and motivations.
2.  Those looking to become more active should be encouraged to track
    steps.
3.  Those who are already very active should be encouraged to track
    sleep and rest to achieve higher quality recovery.
4.  Further analysis is required due to the small sample size of the
    dataset. With this in mind, the recommendations are merely a starting
    point.

## Limitations

The sample size of the dataset is very small both in terms of the number
of users (33) and the time span of the sample (31 days). No qualitative
data was presented in this dataset and thus we do not know for which
purposes the users are using the device let alone the types of activies
that are being undertaken.
