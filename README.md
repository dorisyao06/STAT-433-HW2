STAT 433 HW2
================
2022-10-11

# The following work are completed by Fenghang Yao(<fyao4@wisc.edu>).

[Fenghang Yao HW2 Github link]()

## Introduction

In this assignment, I want to find out what time of day should people
fly if people want to avoid delays as much as possible. To make this
topic more interesting, I also digged into the question that does this
choice depend on other factors like Season, Weather, Airport, and
Airline. The datasets I used is in nycflights13 package.

### Findings

1.  When consider season as a factor, I found out that in Spring,
    Summer,and Winter, 7 am is the time people want to fly if people
    want to avoid delays as much as possible.
2.  

## Codes for Findings

### Season

In this assignment, month 1,2,11,12 are seen as winter, month 3 to 5 are
seen as Spring,month 6 to 8 are seen as Summer, month 9 to 10 are seen
as Fall.

``` r
flights_season = flights %>% mutate(season = case_when(month %in% c(6:8) ~ "Summer",
                                             month %in% c(9:10) ~ "Fall",
                                             month %in% c(1:2,11:12) ~ "Winter",
                                             month %in% c(3:5) ~  "Spring"))
```

After adding the season column, I used group_by function to first group
hour and season factor, and using summarise function to get the mean
arrival delay time for each hour-season combination group, and also used
arrange function to list the least arrival delay time at first.

``` r
flights_season_hour = flights_season %>%group_by(hour,season) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)
```

In the following step, I want to see, for each season, which hour has
the least arrival delay time.

``` r
flights_season_hour= flights_season_hour %>% group_by(season) %>% filter(arr_delay == min(arr_delay,na.rm = T))
flights_season_hour
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["hour"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["season"],"name":[2],"type":["chr"],"align":["left"]},{"label":["arr_delay"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"5","2":"Fall","3":"-11.047904"},{"1":"7","2":"Spring","3":"-6.664645"},{"1":"7","2":"Summer","3":"-5.131790"},{"1":"7","2":"Winter","3":"-1.853815"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

According to the above table, we could see that in Fall season, 5am is
the time of the day people should choose to fly if they want to avoid
arrival delay as much as possible.

## Weather

## Airport

``` r
flights %>% View
```
