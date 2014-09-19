When were the "best" popular movies made?
========================================================
Week 4 Assignment
--------------------------------------------------------



```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
```

```
## Warning: package 'dplyr' was built under R version 3.1.1
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data(movies)
movies_df <- tbl_df(movies)
```


For this assignment we need to determine the year when the "best popular" movies were made. To determine which movies fit in this category we decided to define "best" and "popular" in the following ways.


__Best Movies__ - We chose to use the IMDB rating to determine which movies were the "best". We decided that we would include any movie greater than one standard deviation from the mean rating.


```r
best <- mean(movies$rating) + sd(movies$rating)
best
```

```
## [1] 7.486
```


__Popular Movies__ - We chose to us the number of times that a movie was rated to determine it's popularity. This does lead to a possible issue that we can see from the following plot.

```r
ggplot(data = movies, aes(x = year, y = votes)) + 
    geom_point() +
    ggtitle("Number of Votes per Year")
```

![plot of chunk Plot of votes by year](figure/Plot of votes by year.png) 


It appears that movies were more likely to be rated if they were released more recently then movies that were released before 1935. However with this data set this was the best measure that we could come up with. We choose to also pick movies that were greater than one standard deviation from the mean number of votes.


```r
popular <- mean(movies$votes) + sd(movies$votes)
popular
```

```
## [1] 4462
```


As an interesting aside, we initially used two standard deviations for both selectors but ended up with a small data set of two excelent movies that I enjoyed watching, however these are not enough to make the problem interesting!

```r
pop <- mean(movies$votes) + 2*sd(movies$rating)
bst <- mean(movies$rating) + 2*sd(movies$rating)
movies_df %>%
    filter(votes > pop, rating > bst) %>%
    select(title, year, rating, votes)
```

```
## Source: local data frame [2 x 4]
## 
##                       title year rating  votes
## 1            Godfather, The 1972    9.1 122755
## 2 Shawshank Redemption, The 1994    9.1 149494
```


Next we decided to graph the 'best popular' movies to see what we could find.

```r
best.years <- movies_df %>%
    group_by(year) %>%
    filter(votes > popular, rating > best) %>%
    summarise(avgrating = mean(rating))

ggplot(best.years, aes(x = year, y = avgrating))+
    geom_point()+
    geom_smooth(method = loess)+
    ggtitle("Average of the 'best popular' Movie Ratings by Year")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 



We then used the dplyr() package to filter the movies data down to the year or years that had the highest average rated movies based on popularity.

```r
movies_df %>%
    group_by(year) %>%
    filter(votes > popular, rating > best) %>%
    summarise(avgrating = mean(rating)) %>%
    filter(avgrating == max(avgrating))
```

```
## Source: local data frame [1 x 2]
## 
##   year avgrating
## 1 1941      8.55
```


This leads us to conclude that the year when the best popular movies were made was 1941 with an average rating of the popular movies of 8.55. Looking at the movies that made up this year we get the following:


```r
movies_df %>%
    group_by(title) %>%
    filter(votes > popular, rating > best, year == 1941)
```

```
## Source: local data frame [2 x 24]
## Groups: title
## 
##                 title year length budget rating votes  r1  r2  r3  r4  r5
## 1        Citizen Kane 1941    119 686033    8.7 61083 4.5 4.5 4.5 4.5 4.5
## 2 Maltese Falcon, The 1941    101 300000    8.4 19444 4.5 4.5 4.5 4.5 4.5
## Variables not shown: r6 (dbl), r7 (dbl), r8 (dbl), r9 (dbl), r10 (dbl),
##   mpaa (fctr), Action (int), Animation (int), Comedy (int), Drama (int),
##   Documentary (int), Romance (int), Short (int)
```
