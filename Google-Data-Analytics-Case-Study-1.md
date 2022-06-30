How Does a Bike-Share Navigate Speedy Success?
================
Romeu Fonseca
2022-06-30

## The Business Task

Cyclistic is a bike-share company from Chicago.

Cyclistic offers three pricing plans: single-ride passes, full-day
passes and an annual membership. Customers who purchase the annual
membership are considered Cyclistic members.

Since annual memberships are more profitable, the marketing department
wants an strategy to convert casual riders into members.

To achieve this goal, the marketing department wants an answer to the
following question: How do annual members and casual riders use
Cyclistic bikes differently?

## Using Cyclistic’s historical trip data to analyze and identify trends

### Preparing the data

For the analysis, it will be used 12 CSV files with the historical trip
data from the last 12 months, in this case from April-2022 to May-2021.
For convenience, they will be merged in a single dataframe.

``` r
month1 <- read.csv("202105-divvy-tripdata.csv")
month2 <- read.csv("202106-divvy-tripdata.csv")
month3 <- read.csv("202107-divvy-tripdata.csv")
month4 <- read.csv("202108-divvy-tripdata.csv")
month5 <- read.csv("202109-divvy-tripdata.csv")
month6 <- read.csv("202110-divvy-tripdata.csv")
month7 <- read.csv("202111-divvy-tripdata.csv")
month8 <- read.csv("202112-divvy-tripdata.csv")
month9 <- read.csv("202201-divvy-tripdata.csv")
month10 <- read.csv("202202-divvy-tripdata.csv")
month11 <- read.csv("202203-divvy-tripdata.csv")
month12 <- read.csv("202204-divvy-tripdata.csv")
```

Loading the tidyverse, lubridate, ggplot2 and hms packages:

Each CSV file has the following attributes:

``` r
colnames(month1)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"

Combining all the dataframes in a single DF:

``` r
df_list <- list(month1, month2, month3, month4,
                month5, month6, month7, month8,
                month9, month10, month11, month12)

df <- do.call(rbind, df_list)

str(df)
```

    ## 'data.frame':    5757551 obs. of  13 variables:
    ##  $ ride_id           : chr  "C809ED75D6160B2A" "DD59FDCE0ACACAF3" "0AB83CB88C43EFC2" "7881AC6D39110C60" ...
    ##  $ rideable_type     : chr  "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
    ##  $ started_at        : chr  "2021-05-30 11:58:15" "2021-05-30 11:29:14" "2021-05-30 14:24:01" "2021-05-30 14:25:51" ...
    ##  $ ended_at          : chr  "2021-05-30 12:10:39" "2021-05-30 12:14:09" "2021-05-30 14:25:13" "2021-05-30 14:41:04" ...
    ##  $ start_station_name: chr  "" "" "" "" ...
    ##  $ start_station_id  : chr  "" "" "" "" ...
    ##  $ end_station_name  : chr  "" "" "" "" ...
    ##  $ end_station_id    : chr  "" "" "" "" ...
    ##  $ start_lat         : num  41.9 41.9 41.9 41.9 41.9 ...
    ##  $ start_lng         : num  -87.6 -87.6 -87.7 -87.7 -87.7 ...
    ##  $ end_lat           : num  41.9 41.8 41.9 41.9 41.9 ...
    ##  $ end_lng           : num  -87.6 -87.6 -87.7 -87.7 -87.7 ...
    ##  $ member_casual     : chr  "casual" "casual" "casual" "casual" ...

There are almost *6 million observations* in the dataframe, with 13
variables.

The “started_at” and “ended_at” variables are strings (chr type). They
must be converted to the date format.

Converting the above variables to the date format:

``` r
df <- df %>%
        mutate(start = ymd_hms(started_at), end = ymd_hms(ended_at))
```

Selecting and renaming some variables that are going to be analyzed:

``` r
df <- df %>% 
  select('ride_id','rideable_type', 'start', 'end',
         'member_casual','start_lat', 'start_lng') %>% 
  rename('member_type'= 'member_casual')
```

Sorting the dataframe by date:

``` r
df <- df %>% arrange(ymd_hms(start))
```

### Processing the data

Calculating and creating the “ride_length” attribute:

``` r
df <- df %>%
        mutate(ride_length = as_hms(difftime(end,start, unit="secs")))
```

Adding a “day_of_week” column to the dataframe:

``` r
df <- df %>%
        mutate(day_of_week = wday(start, label=TRUE, abbr=FALSE, 
                                  locale="English_United States"))

head(df)
```

    ##            ride_id rideable_type               start                 end
    ## 1 015AD83E58838178  classic_bike 2021-05-01 00:00:11 2021-05-01 00:12:28
    ## 2 850563C43E02CBAC  classic_bike 2021-05-01 00:00:22 2021-05-01 00:04:00
    ## 3 20252EF674FE87AA   docked_bike 2021-05-01 00:00:31 2021-05-01 00:20:50
    ## 4 70F741E61F1342B5  classic_bike 2021-05-01 00:00:41 2021-05-01 00:10:32
    ## 5 5AF343214878AC95   docked_bike 2021-05-01 00:00:48 2021-05-01 02:39:53
    ## 6 209A72FF91EA4543  classic_bike 2021-05-01 00:00:49 2021-05-01 00:03:26
    ##   member_type start_lat start_lng ride_length day_of_week
    ## 1      member  41.89076 -87.63170    00:12:17    Saturday
    ## 2      member  41.90967 -87.64813    00:03:38    Saturday
    ## 3      casual  41.92556 -87.65840    00:20:19    Saturday
    ## 4      casual  41.91468 -87.64332    00:09:51    Saturday
    ## 5      casual  41.90096 -87.62378    02:39:05    Saturday
    ## 6      member  41.98974 -87.66014    00:02:37    Saturday

### Analyzing the data

Defining a function to calculate the mode of an attribute: (source:
<https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode>)

``` r
getMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
```

Calculating the mean of the attribute ride_length, the maximum ride
length and the statistical mode of day of the week for casual and
members alike:

``` r
df %>% group_by(member_type) %>% 
  drop_na() %>% 
  summarize(mean_ride_length = as_hms(mean(ride_length)), 
            max_ride_length = as_hms(max(ride_length)), 
            mode_day_of_week = getMode(day_of_week)
            )
```

    ## # A tibble: 2 × 4
    ##   member_type mean_ride_length max_ride_length mode_day_of_week
    ##   <chr>       <time>           <time>          <ord>           
    ## 1 casual      31'17.446360"    932:24:09       Saturday        
    ## 2 member      13'08.656849"     25:59:54       Wednesday

As expected, weekend days are the chosen days to ride bikes for casual
users. For members, Wednesday is the most common day of the week.

The mean ride length is shorter for members. Members probably use a bike
to get from the nearest bike station to their job/school location, while
casual users ride bikes for leisure.

Now, let’s create a column in the dataframe with the month of the ride
so it will be possible to group the data by *meteorological season*
(spring, summer, autumn, winter):

``` r
df <- df %>% 
  mutate(month = month(start))

df <- df %>% 
  mutate(season = case_when(
    month %in% 10:12 ~ "Fall",
    month %in% 1:3   ~ "Winter",
    month %in% 4:6   ~ "Spring",
    TRUE             ~ "Summer"
  ))
  
str(df)
```

    ## 'data.frame':    5757551 obs. of  11 variables:
    ##  $ ride_id      : chr  "015AD83E58838178" "850563C43E02CBAC" "20252EF674FE87AA" "70F741E61F1342B5" ...
    ##  $ rideable_type: chr  "classic_bike" "classic_bike" "docked_bike" "classic_bike" ...
    ##  $ start        : POSIXct, format: "2021-05-01 00:00:11" "2021-05-01 00:00:22" ...
    ##  $ end          : POSIXct, format: "2021-05-01 00:12:28" "2021-05-01 00:04:00" ...
    ##  $ member_type  : chr  "member" "member" "casual" "casual" ...
    ##  $ start_lat    : num  41.9 41.9 41.9 41.9 41.9 ...
    ##  $ start_lng    : num  -87.6 -87.6 -87.7 -87.6 -87.6 ...
    ##  $ ride_length  : 'hms' num  00:12:17 00:03:38 00:20:19 00:09:51 ...
    ##   ..- attr(*, "units")= chr "secs"
    ##  $ day_of_week  : Ord.factor w/ 7 levels "Sunday"<"Monday"<..: 7 7 7 7 7 7 7 7 7 7 ...
    ##  $ month        : num  5 5 5 5 5 5 5 5 5 5 ...
    ##  $ season       : chr  "Spring" "Spring" "Spring" "Spring" ...

Summarizing the data by *member_type* and *season*:

Checking the *mean_ride_length*, *max_ride_length* and
*mode_day_of_week* by each month for each user type:

``` r
df %>% group_by(month, member_type) %>% 
  drop_na() %>% 
  summarize(mean_ride_length = as_hms(mean(ride_length)), 
            max_ride_length = as_hms(max(ride_length)), 
            mode_day_of_week = getMode(day_of_week)
            )
```

    ## # A tibble: 24 × 5
    ## # Groups:   month [12]
    ##    month member_type mean_ride_length max_ride_length mode_day_of_week
    ##    <dbl> <chr>       <time>           <time>          <ord>           
    ##  1     1 casual      30'22.647300"    487:51:06       Saturday        
    ##  2     1 member      11'58.890592"     24:59:57       Thursday        
    ##  3     2 casual      26'42.507051"    181:45:58       Monday          
    ##  4     2 member      11'24.331766"     24:59:56       Monday          
    ##  5     3 casual      32'37.343428"    572:34:04       Sunday          
    ##  6     3 member      11'57.499732"     25:59:54       Wednesday       
    ##  7     4 casual      29'31.945624"    352:02:12       Saturday        
    ##  8     4 member      11'29.544247"     24:59:58       Tuesday         
    ##  9     5 casual      38'13.857973"    898:41:36       Saturday        
    ## 10     5 member      14'38.334552"     24:59:56       Saturday        
    ## # … with 14 more rows

Checking the *mean_ride_length* and *max_ride_length* by each day of the
week for each user type:

``` r
df %>% group_by(day_of_week, member_type) %>% 
  drop_na() %>% 
  summarize(mean_ride_length = as_hms(mean(ride_length)), 
            max_ride_length = as_hms(max(ride_length))
            )
```

    ## # A tibble: 14 × 4
    ## # Groups:   day_of_week [7]
    ##    day_of_week member_type mean_ride_length max_ride_length
    ##    <ord>       <chr>       <time>           <time>         
    ##  1 Sunday      casual      36'57.803458"    898:41:36      
    ##  2 Sunday      member      15'03.503518"     24:59:56      
    ##  3 Monday      casual      31'03.944459"    517:11:13      
    ##  4 Monday      member      12'42.735194"     24:59:57      
    ##  5 Tuesday     casual      26'27.821425"    648:42:55      
    ##  6 Tuesday     member      12'15.576002"     24:59:57      
    ##  7 Wednesday   casual      27'05.554039"    640:18:36      
    ##  8 Wednesday   member      12'24.902195"     24:59:58      
    ##  9 Thursday    casual      27'53.260809"    818:27:09      
    ## 10 Thursday    member      12'26.283306"     24:59:57      
    ## 11 Friday      casual      29'12.511182"    928:11:41      
    ## 12 Friday      member      12'52.673037"     24:59:58      
    ## 13 Saturday    casual      34'11.566782"    932:24:09      
    ## 14 Saturday    member      14'46.842989"     25:59:54

The mean_ride_length reaches its maximum value in spring, for both
casual riders and members, specially on May and June. The average ride
length is also larger on the weekend.

Calculating the number of rides by *day_of_the_week* per user:

``` r
df %>% group_by(day_of_week, member_type) %>% 
  drop_na() %>%
  summarize(number_of_rides = n())
```

    ## # A tibble: 14 × 3
    ## # Groups:   day_of_week [7]
    ##    day_of_week member_type number_of_rides
    ##    <ord>       <chr>                 <int>
    ##  1 Sunday      casual               477032
    ##  2 Sunday      member               388042
    ##  3 Monday      casual               289029
    ##  4 Monday      member               445635
    ##  5 Tuesday     casual               270548
    ##  6 Tuesday     member               498682
    ##  7 Wednesday   casual               284868
    ##  8 Wednesday   member               506969
    ##  9 Thursday    casual               298061
    ## 10 Thursday    member               485843
    ## 11 Friday      casual               358203
    ## 12 Friday      member               453281
    ## 13 Saturday    casual               558617
    ## 14 Saturday    member               442741

Splitting the dataframe in two: one with only members and the other with
casual riders

``` r
df_member <- df %>% 
  filter(member_type == 'member')

df_casual <- df %>% 
  filter(member_type == 'casual')
```

### Visualizing and sharing the data

First, let’s evaluate the average ride length for each user by season:

``` r
color_range <- c(1:8)

df %>%
  drop_na() %>%
  #filter(member_type =='casual') %>% 
  group_by(season, member_type) %>% 
  summarize(mean_ride_length = as_hms(mean(ride_length))) %>% 
  ggplot(mapping = aes(x = season, y = mean_ride_length, 
                       fill = color_range, width = 0.7)) +
  geom_col() +
  scale_fill_gradient2(guide = "none") + 
  labs(title = "Average Length Ride for casual users per Season", 
       x = "Season", y = "Average Ride Length (mins)") +
  theme_bw() +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    plot.title = element_text(colour = "black", face = "bold", 
                              margin = margin(t=0, r=0, b=10, l=0)), 
    axis.title.y = element_text(margin = margin(t=0, r=20, b=0, l=0)),
    axis.title.x = element_text(margin = margin(t=10, r=0, b=0, l=0))
    ) +
  facet_wrap(~member_type)
```

![](Google-Data-Analytics-Case-Study-1_files/figure-gfm/Average%20ride%20length%20by%20season-1.png)<!-- -->

The overall average ride length is a around 30 min. Rides in Spring are
longer than other seasons of the year.

For Cyclistic members, the situation is different. Members average ride
length are shorter by half in comparison with casual riders.

Verifying the distribution of the attribute rideable_type.

``` r
df %>%
  drop_na() %>%
  ggplot() +
  geom_bar(mapping = aes(y = rideable_type, fill = rideable_type)) +
  scale_fill_manual(guide = "none", values = c("docked_bike" = "#E7E4F2", 
                                               "electric_bike" = "#B8B0D9",
                                               "classic_bike" = "#5750A5")) +
  labs(title = "Distribution of rides type for casual users", x="Count", 
       y="Ride Type") +
  theme_minimal() +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    plot.title = element_text(colour = "black", face = "bold",
                              margin = margin(t=0, r=0, b=10, l=0)),
    axis.title.y = element_text(margin = margin(t=0, r=30, b=0, l=0)), 
    axis.title.x = element_text(margin = margin(t=10, r=0, b=0, l=0))
    ) +
  facet_wrap(~member_type, ncol=1)
```

![](Google-Data-Analytics-Case-Study-1_files/figure-gfm/distribution%20of%20the%20attribute%20rideable_type-1.png)<!-- -->

The most common ride type is the classic bike, followed by the electric
bike.

Analyzing the growth of each ride type in the last 12 months, for each
user.

``` r
df %>%
  drop_na() %>%
  filter(rideable_type != 'docked_bike') %>% 
  group_by(month, member_type, rideable_type) %>% 
  summarize(rides_count = n()) %>% 
  ggplot(mapping = aes(x = factor(month), y = rides_count/1000, color = rideable_type)) +
  geom_point() +
  scale_colour_manual(values = c("electric_bike" = "#B8B0D9",
                               "classic_bike" = "#5750A5")) +
  labs(title = "Count of rides by rideable type in each month", x = "Month", 
       y = "Rides Count (x1000)") +
  theme_bw() +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    plot.title = element_text(colour = "black", face = "bold", 
                              margin = margin(t=0, r=0, b=10, l=0)),
    axis.title.y = element_text(margin = margin(t=0, r=30, b=0, l=0)), 
    axis.title.x = element_text(margin = margin(t=10, r=0, b=0, l=0))
    ) +
  xlab('month') +
  facet_wrap(~member_type, ncol=1)
```

![](Google-Data-Analytics-Case-Study-1_files/figure-gfm/ride%20type%20growth%20in%20the%20last%2012%20months-1.png)<!-- -->
And the respective distribution of each ride type:

``` r
df %>%
  drop_na() %>%
  filter(rideable_type != 'docked_bike') %>% 
  group_by(month, member_type, rideable_type) %>% 
  summarize(ride_count = n())  %>% 
  mutate(ride_proportion = ride_count*100/sum(ride_count)) %>% 
  ggplot(mapping=aes(x=factor(month), y = ride_proportion)) +
  geom_col(aes(fill = rideable_type)) +
  scale_fill_manual(values = c("electric_bike" = "#B8B0D9",
                               "classic_bike" = "#5750A5")) +
  labs(title = "Proportion of each rideable type per month", x = "Month", 
       y = "Rideable Type Proportion (%)") +
  theme_bw() +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    plot.title = element_text(colour = "black", face = "bold",
                              margin = margin(t=0, r=0, b=10, l=0)),
    axis.title.y = element_text(margin = margin(t=0, r=30, b=0, l=0)), 
    axis.title.x = element_text(margin = margin(t=10, r=0, b=0, l=0))) +
  xlab('month') +
  facet_wrap(~member_type, ncol=1)
```

![](Google-Data-Analytics-Case-Study-1_files/figure-gfm/distribution%20of%20ride%20types%20per%20month-1.png)<!-- -->

Electric bikes are the preferable choice in all seasons for casual
users, except in the summer months. Casual riders, in particular, choose
electric bike the most (as shown in the visualizations above) when
comparing with Cyclistic members.

Now let’s consider the geospatial data considering the number of rides
of casual users and Cyclistic members.

Calculating the mode of latitudes and longitudes per user type.

``` r
df %>% 
  drop_na() %>% 
  select(member_type, rideable_type, start_lat, start_lng) %>% 
  mutate(Latitude = start_lat, Longitude = start_lng)  %>%
  group_by(member_type) %>% 
  summarize(mode_lat = getMode(Latitude), mode_lng = getMode(Longitude))
```

    ## # A tibble: 2 × 3
    ##   member_type mode_lat mode_lng
    ##   <chr>          <dbl>    <dbl>
    ## 1 casual          41.9    -87.6
    ## 2 member          41.8    -87.6

Using those coordinates as a reference, let’s extract a map from the
city of Chicago around the above latitude and longitude.

For these visualizations, the ggmap package will be loaded.

To create the bbox parameter below, the site *<https://bboxfinder.com>*
was used.

Visualizing the data for Cyclistic members:

``` r
df_member_map <- df_member %>% 
  drop_na() %>% 
  select(ride_id, rideable_type, member_type, start_lat, start_lng) %>% 
  mutate(Latitude = start_lat, Longitude = start_lng) %>% 
  group_by(Latitude, Longitude, member_type) %>% 
  summarize(ride_count = n()) 

theme_set(theme_void(10))
  
chicago_map_member + 
  ggtitle("Heat map - Number of rides by Cyclistic members") +
  theme(
      plot.margin = margin(.5, 0, .5, 0, "cm"),
      plot.title = element_text(colour = "black", face = "bold", 
                                margin = margin(t=0, r=0, b=10, l=0)), 
      panel.border = element_rect(colour = "grey", fill=NA, size=2)
    ) +
  stat_density2d(
    data = df_member_map,
    aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
    size = 2, bins = 6, geom = "polygon"
    ) + 
  scale_fill_gradient(low = "green", high = "red", name = "Rides count") +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = .5, 
                                label.position = "left")) +
  scale_alpha(guide = 'none')
```

![](Google-Data-Analytics-Case-Study-1_files/figure-gfm/number%20of%20rides%20by%20members-1.png)<!-- -->

Now, let’s visualize the data for casual users:

``` r
df_casual_map <- df_casual %>% 
  drop_na() %>% 
  select(ride_id, rideable_type, member_type, start_lat, start_lng) %>% 
  mutate(Latitude = start_lat, Longitude = start_lng) %>% 
  group_by(Latitude, Longitude, member_type) %>% 
  summarize(ride_count = n()) 


theme_set(theme_void(10))
  
chicago_map_casual + 
  ggtitle("Heat map - Number of rides by casual users") +
  theme(
    plot.margin = margin(.5, 0, .5, 0, "cm"),
    plot.title = element_text(colour = "black", face = "bold", 
                              margin = margin(t=0, r=0, b=10, l=0)), 
    panel.border = element_rect(colour = "grey", fill=NA, size=2)
    ) +
  stat_density2d(
    data = df_casual_map,
    aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, geom = "polygon"
    ) +
  scale_fill_gradient(low = "green", high = "red", name = "Rides count", 
                      n.breaks=5) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = .5, 
                                label.position = "left")) +
  scale_alpha(guide = 'none')
```

![](Google-Data-Analytics-Case-Study-1_files/figure-gfm/number%20of%20rides%20by%20casual%20users-1.png)<!-- -->

As shown above, the casual user group is concentrated in Chicago Navy
Pier, around Addams (Jane) Memorial Park (a touristic area).

In the other hand, Cyclistic members are found around the University of
Chicago (Hyde Park).

## Conclusion

After analyzing Cyclistic historical data from the last 12 months, those
are some of the recommendations to help the marketing department to plan
a reliable strategy to convert casual users into members:

1.  Concentrate efforts around Chicago Navy Pier;
2.  Focus on electric bikes in all seasons, except summer, when classic
    bikes are the top choice;
3.  Spring is a good season to make conversion campaigns. Weekends are
    the best days to approach casual users.
