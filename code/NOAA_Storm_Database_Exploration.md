# Coursera Reproducible Research Peer Assessment #2
Bill Cary  
Sunday, December 21, 2014  

# Title - NOAA Storm Database Exploration and Analysis
## Synopsis
Based on the following analysis, it is clear that in terms of hazards to human
health, tornados are far and away the leading weather-related cause of both 
injuries and fatalities to human beings.  In terms of economic damage, flooding
is the leading cause of both total economic damage as well as property damage.
However, drought is the leading cause of crop damage.

## Data Analysis
### Set defaults
Set knitr to echo code by default

```r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set echo=TRUE by default
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(knitr)
opts_chunk$set(echo = TRUE)
```

### Prepare the analysis environment

```r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import required libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
suppressMessages(library(ggplot2))     # General plotting functions
suppressMessages(library(plyr))        # Data manipulation
suppressMessages(library(dplyr))       # Data manipulation
suppressMessages(library(gridExtra))   # Grid layout for ggplot2 graphs
```

```
## Warning: package 'gridExtra' was built under R version 3.1.2
```

```r
suppressMessages(library(scales))      # Axis scaling for ggplot2 graphs
```

```
## Warning: package 'scales' was built under R version 3.1.2
```

```r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set paths for files and directory structure
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
path_data <- '../data/repdata-data-StormData.csv.bz2'
```

### Loading the data
Read the raw dataset into R.  Because the dataset is large and the initial load
is time consuming, the cache=TRUE option is used to decrease the runtime after
the initial execution of the analysis.

```r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read the data into R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data <- read.csv(path_data)
```

### Explore the Data
For this exercise, the fields of interest are FATALITIES, INJURIES, PROPDMG,
PROPDMGEXP, CROPDMG and CROPDMGEXP.  The PROPDMGEXP and CROPDMGEXP fields
provide an "order of magnitude" describing the size of the monetary damages
(thousands, millions, billions, etc...)

```r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate summaries of the fields of interest
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(data$FATALITIES)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0       0       0       0       0     583
```

```r
summary(data$INJURIES)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0     0.0     0.0     0.2     0.0  1700.0
```

```r
summary(data$PROPDMG)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0       0       0      12       0    5000
```

```r
summary(data$PROPDMGEXP)
```

```
##             -      ?      +      0      1      2      3      4      5 
## 465934      1      8      5    216     25     13      4      4     28 
##      6      7      8      B      h      H      K      m      M 
##      4      5      1     40      1      6 424665      7  11330
```

```r
summary(data$CROPDMG)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0     0.0     0.0     1.5     0.0   990.0
```

```r
summary(data$CROPDMGEXP)
```

```
##             ?      0      2      B      k      K      m      M 
## 618413      7     19      1      9     21 281832      1   1994
```

From the summaries above, it does not appear that the FATALITIES, INJURIES,
PROPDMG and CROPDMG fields require cleansing/standardization.  However, the
PROPDMGEXP and CROPDMGEXP field contain significant duplication of values ("m"
and "M") that needs to be corrected before the data is useful for analysis.
They also contain a number of values that are not defined, including +, -, ?
as well as numeric values.  In addition, the exponents provided in those fields
need to be applied to the PROPDMG and CROPDMG values to put the values of each
event on the same numeric basis.


```r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean the 'EXP fields (PROPDMGEXP, CROPDMGEXP) according to the following
# rules:
#        1) If field contains a numeric value, then drop the row
#        2) If field contains a +, - or ? then drop the row
#        3) If field contains a lower case h, k, b or m, then convert to upper
#        4) If field is blank, then take no action
#
#        Following the above rules will eliminate only a very small fraction of
#        the available data and should not impact the results of the analysis.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# First, determine the initial length of the dataset (number of rows)
initial.length <- dim(data)[1]

# Now convert both columns to upper case
data$PROPDMGEXP <- toupper(data$PROPDMGEXP)
data$CROPDMGEXP <- toupper(data$CROPDMGEXP)

# Now drop any rows that do not contain an alphabetic or blank value in either
# of the two columns
data <- filter(data, PROPDMGEXP %in% c('', 'H', 'K', 'M', 'B'))
data <- filter(data, CROPDMGEXP %in% c('', 'H', 'K', 'M', 'B'))

# Now, determine the final length of the dataset (number of rows)
final.length <- dim(data)[1]
```

By removing the ambiguously labeled rows, we have only removed
0.0378% of the original
data.


```r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standardize the PROPDMG and CROPDMG values according to the following rules, 
# based on the values in the PROPDMGEXP and CROPDMGEXP fields:
#
#        1) If PROPDMGEXP or CROPDMGEXP are blank, then leave the value as-is
#        2) If they equal 'H', then multiply by 100
#        3) If they equal 'K', then multiply by 1,000
#        4) If they equal 'M', then multiply by 1,000,000
#        5) If they equal 'B', then multiply by 1,000,000,000
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create a function to handle the standardization
standardize <- function(x,y) {
        
        if (y == 'H'){
                x <- x * 100
                }
        else if (y == 'K'){
                x <- x * 1000
                }
        else if (y == 'M'){
                x <- x * 1000000
                }
        else if (y == 'B'){
                x <- x * 1000000000
                }
        else{
                x <- x
                }

return(x)

}

# Standardize the damage estimates
data$PROPDMG <- mapply(standardize,x=data$PROPDMG,y=data$PROPDMGEXP)
data$CROPDMG <- mapply(standardize,x=data$CROPDMG,y=data$CROPDMGEXP)
```



```r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add a column that summarizes injuries and fatalities
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data$TOTALHAZARDS <- data$FATALITIES + data$INJURIES

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summarize the data by Event Type using dplyr
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sumdata <- group_by(data, EVTYPE) %>%
        summarise(sumfatalities = sum(FATALITIES, na.rm=TRUE),
                  suminjuries = sum(INJURIES, na.rm=TRUE),
                  sumhazards = sum(TOTALHAZARDS, na.rm=TRUE)) %>% 
        arrange(desc(sumhazards))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Select the top 10 event types by total hazards
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
top10health <- head(sumdata, 10)

top10health
```

```
## Source: local data frame [10 x 4]
## 
##               EVTYPE sumfatalities suminjuries sumhazards
## 1            TORNADO          5630       91321      96951
## 2     EXCESSIVE HEAT          1903        6525       8428
## 3          TSTM WIND           504        6957       7461
## 4              FLOOD           470        6789       7259
## 5          LIGHTNING           816        5230       6046
## 6               HEAT           937        2100       3037
## 7        FLASH FLOOD           978        1777       2755
## 8          ICE STORM            89        1975       2064
## 9  THUNDERSTORM WIND           133        1488       1621
## 10      WINTER STORM           206        1321       1527
```


```r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add a column that summarizes total economic damage caused by the event
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data$TOTALDAMAGE <- data$PROPDMG + data$CROPDMG

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summarize the data by Event Type using dplyr
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sumdata <- group_by(data, EVTYPE) %>%
        summarise(sumpropdmg = sum(PROPDMG, na.rm=TRUE),
                  sumcropdmg = sum(CROPDMG, na.rm=TRUE),
                  sumtotaldmg = sum(TOTALDAMAGE, na.rm=TRUE)) %>% 
        arrange(desc(sumtotaldmg))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Select the top 10 event types by total economic damage
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
top10econ <- head(sumdata, 10)

top10econ
```

```
## Source: local data frame [10 x 4]
## 
##               EVTYPE sumpropdmg sumcropdmg sumtotaldmg
## 1              FLOOD  1.447e+11  5.662e+09   1.503e+11
## 2  HURRICANE/TYPHOON  6.931e+10  2.608e+09   7.191e+10
## 3            TORNADO  5.694e+10  3.650e+08   5.730e+10
## 4        STORM SURGE  4.332e+10  5.000e+03   4.332e+10
## 5               HAIL  1.573e+10  3.001e+09   1.873e+10
## 6        FLASH FLOOD  1.614e+10  1.421e+09   1.756e+10
## 7            DROUGHT  1.046e+09  1.397e+10   1.502e+10
## 8          HURRICANE  1.187e+10  2.742e+09   1.461e+10
## 9        RIVER FLOOD  5.119e+09  5.029e+09   1.015e+10
## 10         ICE STORM  3.945e+09  5.022e+09   8.967e+09
```

## Results

This analysis is being conducted for the purpose of answering two questions:

1. Across the United States, which types of events (as indicated in the EVTYPE
variable) are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic
consequences?

For the purposes of answering these questions, I interpret the degree of harm of
an event with respect to population health as being the aggregate total of all
deaths and injuries (whether direct or indirect) caused by the event.  However,
I also make special note of the events which lead to the highest number of
direct and indirect deaths, as one can subjectively consider any injury, no
matter how severe, to be less severe than death.

I interpret economic damage for a given event as the sum of the property and
crop damage.  The primary objective of this part of the exercise is to identify
the greatest cause of economic damage, it is also useful to understand the
largest causes of each component of damage - crop and property damage.

*NOTE FOR GRADERS: The plot below is a single figure with multiple panels*
*constructed using the gridExtra package.*



```r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot the top 10 event types in descending order of magnitude
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot1 <- ggplot(data=top10health, aes(x=reorder(EVTYPE, -sumhazards),
                                      y=sumhazards)) +
        geom_bar(stat="identity") +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        xlab('') +
        ylab('No of Occurences') +
        theme(text = element_text(size=8)) +
        ggtitle('Fatalities + Injuries') +
        scale_y_continuous(labels = comma)

plot2 <- ggplot(data=top10health, aes(x=reorder(EVTYPE, -sumfatalities),
                                      y=sumfatalities)) +
        geom_bar(stat="identity") +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        xlab('') +
        ylab('No of Occurences') +
        theme(text = element_text(size=8)) +
        ggtitle('Fatalities') +
        scale_y_continuous(labels = comma)

plot3 <- ggplot(data=top10health, aes(x=reorder(EVTYPE, -suminjuries),
                                      y=suminjuries)) +
        geom_bar(stat="identity") +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        xlab('') +
        ylab('No of Occurences') +
        theme(text = element_text(size=8)) +
        ggtitle('Injuries') +
        scale_y_continuous(labels = comma)

plot4 <- ggplot(data=top10econ, aes(x=reorder(EVTYPE, -sumtotaldmg),
                                    y=sumtotaldmg)) +
        geom_bar(stat="identity") +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        xlab('') +
        ylab('$ Damages') +
        theme(text = element_text(size=8)) +
        ggtitle('Property + Crop') +
        scale_y_continuous(labels = comma)

plot5 <- ggplot(data=top10econ, aes(x=reorder(EVTYPE, -sumpropdmg),
                                    y=sumpropdmg)) +
        geom_bar(stat="identity") +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        xlab('') +
        ylab('$ Damages') +
        theme(text = element_text(size=8)) +
        ggtitle('Property') +
        scale_y_continuous(labels = comma)

plot6 <- ggplot(data=top10econ, aes(x=reorder(EVTYPE, -sumcropdmg),
                                    y=sumcropdmg)) +
        geom_bar(stat="identity") +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        xlab('') +
        ylab('$ Damages') +
        theme(text = element_text(size=8)) +
        ggtitle('Crop') +
        scale_y_continuous(labels = comma)

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=2, ncol=3)
```

![plot of chunk plots](./NOAA_Storm_Database_Exploration_files/figure-html/plots.png) 

## Appendix

### Miscellaneous Support Information
Print out system and session information to support future readers in
replicating the environment used to produce the output, and in troubleshooting
any potential issues that arise when running the code.

```r
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Print System and Session for reproducibility & troubleshooting.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print(Sys.time())
```

```
## [1] "2014-12-23 23:11:21 EST"
```

```r
print(sessionInfo())
```

```
## R version 3.1.1 (2014-07-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
## [1] scales_0.2.4    gridExtra_0.9.1 dplyr_0.2       plyr_1.8.1     
## [5] ggplot2_1.0.0   knitr_1.6      
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1   codetools_0.2-9  colorspace_1.2-4 digest_0.6.4    
##  [5] evaluate_0.5.5   formatR_1.0      gtable_0.1.2     htmltools_0.2.6 
##  [9] labeling_0.3     magrittr_1.0.1   MASS_7.3-35      munsell_0.4.2   
## [13] parallel_3.1.1   proto_0.3-10     Rcpp_0.11.3      reshape2_1.4    
## [17] rmarkdown_0.3.3  stringr_0.6.2    tools_3.1.1      yaml_2.1.13
```

```r
# print(Sys.info())   omit Sys.info() for privacy reasons
```
