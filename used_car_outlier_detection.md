---
title: 'Outlier Detection: Finding the Good Used Car Deals!'
output:
  html_document:
    theme: cerulean
    code_folding: hide
    keep_md: TRUE
editor_options:
  chunk_output_type: inline
---

# The Cheap Car Finder
Imagine this- a friend of yours just reached out to you and asked you for help with a project he's working on.  He has scraped over 2 million rows of data from various used car websites and needs help finding the deals that are worth his money.  You agree to help and decide to do the project in R (if you haven't figured it out yet, this is my situation :)

After thinking and studying about it for a while, you discover that outlier detection may be the simplest and easiest way to work through the problem!

We will discuss later what constitutes a "good deal" later (this is where we'll dig into outlier detection).
  
  
![](C:/Users/zacha/Desktop/car_project_outlier_detection_files/car_lot.jpg)  

##Data Cleanup
Our client is mostly interested in looking at Ford Deisel trucks with the folowing criteria:  

-the truck was made between the years 1999-2016  
-the truck has a clean title  
-it is selling for less than $25k  
-It has less than 100,000 miles on it (makes sense)

We'll create our outlier detection model with all of these in mind.  For now, we only need to filter a few things.  We'll complete all of our initial cleanup and transformations on the data read to save time!


```r
#Bringing in our libraries
library(tidyverse)
library(ggpubr) # For qqplots
library(data.table) #faster data reading
library(DT) # For nicer looking tables
library(kableExtra) #For other nice looking tables

#Bringing in our data
ford_diesel_trucks <- fread("C:/Users/zacha/Downloads/Cars (1).csv", header=F, showProgress = TRUE) %>%
  as_data_frame() %>% 
  
#Renaming our columns (only including the ones we want)
  select(car_description = V1
         , year = V2
         , make = V3
         , model = V4
         , description = V5
         , mileage = V6
         , price = V7
         , transmission = V8
         , wheel_drive = V10
         , fuel_type = V11
         , body_type = V12
         , color = V13
         , location = V14
         , url = V15
         , website_name = V21
         , title_condition = V20
         ) %>%
  
#Changing mileage, price, and year columns to integer data type
    mutate_at(vars(mileage, price, year),
              funs(as.integer)) %>% 
  
#Filtering down the Ford Deisel Trucks with a clean title and some other constraints
  filter(make == "Ford" 
        & fuel_type == "Diesel" 
        & body_type == "Truck"
        & year <= 2015
        & year >= 1999
        & mileage <= 200000
        & price < 115000
        & title_condition == "Clean"
        )
```
  
  Now let's take a peak at our data!

```r
#Writing a simple function for looking at our data in a kable styled table
kable_table <- function(dataset, number_of_rows){
  dataset %>%
  head(number_of_rows) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
}

#Show a pretty table of our data
ford_diesel_trucks %>% kable_table(number_of_rows = 5)
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> car_description </th>
   <th style="text-align:right;"> year </th>
   <th style="text-align:left;"> make </th>
   <th style="text-align:left;"> model </th>
   <th style="text-align:left;"> description </th>
   <th style="text-align:right;"> mileage </th>
   <th style="text-align:right;"> price </th>
   <th style="text-align:left;"> transmission </th>
   <th style="text-align:left;"> wheel_drive </th>
   <th style="text-align:left;"> fuel_type </th>
   <th style="text-align:left;"> body_type </th>
   <th style="text-align:left;"> color </th>
   <th style="text-align:left;"> location </th>
   <th style="text-align:left;"> url </th>
   <th style="text-align:left;"> website_name </th>
   <th style="text-align:left;"> title_condition </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2011 F250 LARIAT 6.7L DIESEL 4X4 LIFTED CREW SHORT DELETE 154K </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Ford </td>
   <td style="text-align:left;"> F-250 </td>
   <td style="text-align:left;"> Lariat </td>
   <td style="text-align:right;"> 154715 </td>
   <td style="text-align:right;"> 26995 </td>
   <td style="text-align:left;"> Automatic </td>
   <td style="text-align:left;"> 4WD </td>
   <td style="text-align:left;"> Diesel </td>
   <td style="text-align:left;"> Truck </td>
   <td style="text-align:left;"> Red </td>
   <td style="text-align:left;"> Houston Texas </td>
   <td style="text-align:left;"> ebay.com/itm/192165172137 </td>
   <td style="text-align:left;"> eBay </td>
   <td style="text-align:left;"> Clean </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ""2011 Ford F-350 Crew Cab Lariat FX4 Diesel 20"""" Wheels Navigation Sunroof DVDs"" </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Ford </td>
   <td style="text-align:left;"> F-350 </td>
   <td style="text-align:left;"> Lariat </td>
   <td style="text-align:right;"> 151000 </td>
   <td style="text-align:right;"> 29990 </td>
   <td style="text-align:left;"> Automatic </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Diesel </td>
   <td style="text-align:left;"> Truck </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Walker Louisiana </td>
   <td style="text-align:left;"> ebay.com/itm/322415726947 </td>
   <td style="text-align:left;"> eBay </td>
   <td style="text-align:left;"> Clean </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2011 F250 FX4 4WD CrewCab LongBed Powerstroke AllPower 4DR 1TXowner </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Ford </td>
   <td style="text-align:left;"> F-250 </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:right;"> 159256 </td>
   <td style="text-align:right;"> 21995 </td>
   <td style="text-align:left;"> Automatic </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Diesel </td>
   <td style="text-align:left;"> Truck </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Cedar Hill Texas </td>
   <td style="text-align:left;"> ebay.com/itm/152546661980 </td>
   <td style="text-align:left;"> eBay </td>
   <td style="text-align:left;"> Clean </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2012 F450 Crew Cab 4x4 Diesel Flatbed Low Miles 1 Owner F-450 Truck </td>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> Ford </td>
   <td style="text-align:left;"> F-450 </td>
   <td style="text-align:left;"> Crew Cab 4x4 </td>
   <td style="text-align:right;"> 52227 </td>
   <td style="text-align:right;"> 32950 </td>
   <td style="text-align:left;"> Automatic </td>
   <td style="text-align:left;"> 4WD </td>
   <td style="text-align:left;"> Diesel </td>
   <td style="text-align:left;"> Truck </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Arlington Texas </td>
   <td style="text-align:left;"> ebay.com/itm/282364325254 </td>
   <td style="text-align:left;"> eBay </td>
   <td style="text-align:left;"> Clean </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2011 White 4x4! </td>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:left;"> Ford </td>
   <td style="text-align:left;"> F-250 </td>
   <td style="text-align:left;"> 4x4 </td>
   <td style="text-align:right;"> 91451 </td>
   <td style="text-align:right;"> 29988 </td>
   <td style="text-align:left;"> Automatic </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Diesel </td>
   <td style="text-align:left;"> Truck </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Addison Texas </td>
   <td style="text-align:left;"> ebay.com/itm/302313290892 </td>
   <td style="text-align:left;"> eBay </td>
   <td style="text-align:left;"> Clean </td>
  </tr>
</tbody>
</table>

  
  Looks good!  

# Analysis 
Now it's time to analyze this data!  To detect outliers, or good deals, in this data, we first need to see if our distribution of prices is normal. A "normal" distribution looks like a bellcurve, just like this one:
<div style="text-align:center" markdown="1">
![](C:/Users/zacha/Desktop/car_project_outlier_detection_files/normal_distribution.jpg) 
</div>
</n>  

## Checking for Normal Distributions {.tabset .tabset-fade}
Keep in mind that we'll working with two variables: price, and mileage.  These are very good indicators of what a car's price will be, so we'll check these two variables for outliers.

### Price Distribution


```r
# Plotting the histogram
ford_diesel_trucks %>% 
  ggplot(aes(x=price)) +
      geom_histogram(binwidth = 1000, fill="dodgerblue1", color="black") + 
      theme_minimal() +
      labs(x= "Price ($)",
           y= "Number of Cars") +
      ggtitle("Ford Diesel Price Distribution")+
      theme(plot.title = element_text(hjust = 0.5)) #Centers the title
```

![](used_car_outlier_detection_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Plotting the QQ plot
ggqqplot(ford_diesel_trucks$price, 
         title = "QQ Plot (If normal, the points will mostly be on the Line")
```

![](used_car_outlier_detection_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

We can see that our distribution of prices isn't really normally distributed.  

### Mileage Distribution


```r
# Plotting the histogram
ggplot(data = ford_diesel_trucks, aes(x=mileage)) +
         geom_histogram(binwidth = 5000, fill="#E69F00", color="black") +
         labs(x="Mileage",
              y="# of Cars")+
         ggtitle("Mileage Distribution with Ford Diesels")+
         theme(plot.title = element_text(hjust = 0.5))+
         theme_minimal()
```

![](used_car_outlier_detection_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Plotting the QQ plot
ggqqplot(ford_diesel_trucks$mileage, title = "QQ Plot (If Normal, the points will mostly be on the line")
```

![](used_car_outlier_detection_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

The mileage distribution looks much better!  

## Looking for Outliers {.tabset .tabset-fade}
We're on a hunt for the best deals, but it turns out that there are several methods for finding them.  Click one of the tabs below to find out about it!

<div style="text-align:center" markdown="1">
![](C:/Users/zacha/Desktop/car_project_outlier_detection_files/binoculars-to-sunglasses.gif)
</div>
</n>  

### The Tukey Method

Since we're looking for good deals, we'd want to find the cars with REALLY low prices and REALLY low mileage, right?  One way we can visualize these outliers is by using boxplots.  The dots that show up outside of the box plots are considered as outliers, as defined by the Tukey Method.

The Tukey Method is defined as follows:

"The lower and upper hinges correspond to the first and third quartiles (the 25th and 75th percentiles)...

The upper whisker extends from the hinge to the largest value no further than 1.5 \* IQR from the hinge (where IQR is the inter-quartile range, or distance between the first and third quartiles). The lower whisker extends from the hinge to the smallest value at most 1.5 \* IQR of the hinge. *Data beyond the end of the whiskers are called "outlying" points and are plotted individually.They are points that are greater than or less than 1.5 \* the Inter Quartile Range*"

This illustration may help:

<div style="text-align:center" markdown="1">
![](C:/Users/zacha/Desktop/car_project_outlier_detection_files/boxplot_with_outliers.jpg)
</div>
</n>  


In other words, when we visualize the mileage and price, do any dots show up on our boxplots?


```r
# Plotting the price boxplot
ford_diesel_trucks %>% 
  ggplot(aes(x=1,y=price)) +
  geom_boxplot(width=.05, fill="dodgerblue1") +
  theme_minimal() +
  labs(x= "Density",
       y= "Price",
       title = "Ford Diesel Price Distribution") +
  theme(axis.text.x=element_blank(), #Gets rid of x-axis text
        axis.ticks.x=element_blank(), #Gets rid of x-axis ticks
        plot.title = element_text(hjust = 0.5) #Centers the title
        ) 
```

![](used_car_outlier_detection_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# Plotting the mileage boxplot
ford_diesel_trucks %>% 
  ggplot(aes(x=1,y=mileage)) +
    geom_boxplot(width=.05, fill="#E69F00") +
    theme_minimal() +
    labs(x= "Density",
         y= "Mileage",
         title = "Ford Diesel Mileage Distribution") +
  theme(axis.text.x=element_blank(), #Gets rid of x-axis text
        axis.ticks.x=element_blank(), #Gets rid of x-axis ticks
        plot.title = element_text(hjust = 0.5) #Centers the title
        ) 
```

![](used_car_outlier_detection_files/figure-html/unnamed-chunk-5-2.png)<!-- -->
  
As you can see in each of these boxplots, we don't have any outliers (at least as defined by the Tukey method).  It is likely that this is because the IQR's (InterQuartile Ranges) for price and mileage is so large!  

Scroll by up to look at a different method!

### Using Standard Deviation
Instead, let's try using standard deviation to tell us which cars are the best deals.

The formula for standard deviation looks like this:
$$
\sigma = \sqrt{\frac{\sum\limits_{i=1}^{n} \left(x_{i} - \bar{x}\right)^{2}} {n-1}}
$$
  
I know, I know- this equation is big and scary!  But basically what it's doing is creating an "average distance from the mean" and putting that into a number, which is called the standard deviation.  

Here's a graphic that might help you understand this concept:
![](C:/Users/zacha/Desktop/car_project_outlier_detection_files/standard_deviation_curve.jpg)

So we could say that 1 standard deviation is a typical distance from the mean, we could say that 2 standard deviations is pretty dang far from the mean, and 3 is SUPER far from the mean (highly scientific, I know).

By taking the cars that have a high, negative standard deviation, we'll catch the cars that have very low prices and very low mileage.

To do this, we'll need to change our data to include an extra column that will tell us the standard deviation (also known as a z-score in this context) for each car.


```r
ford_diesels_with_outliers <- ford_diesel_trucks %>%

#Creating columns that tell us the standard deviation for price and mileage
  mutate(sd_price = (price-mean(price))/sd(price),  
         sd_mileage = (mileage-mean(mileage))/sd(mileage))

#Let's take a peak at these two new columns
ford_diesels_with_outliers %>% select(sd_price, sd_mileage) %>% kable_table(5)
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> sd_price </th>
   <th style="text-align:right;"> sd_mileage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.0097153 </td>
   <td style="text-align:right;"> 0.9054206 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.2688827 </td>
   <td style="text-align:right;"> 0.8262457 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> -0.4229514 </td>
   <td style="text-align:right;"> 1.0021994 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.5250214 </td>
   <td style="text-align:right;"> -1.2788265 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.2687096 </td>
   <td style="text-align:right;"> -0.4428759 </td>
  </tr>
</tbody>
</table>
Looks good!  Now let's filter our data accordingly.  We'll consider a car as a *good deal* when it is...  
  
1. More than .4 standard deviations below the mean price and
2. More than .8 standard deviations below the mean mileage

(These values were determined by experimentation with different numbers.  They capture a good sample of the best deals.)

```r
ford_diesel_sd_outliers <- ford_diesels_with_outliers %>% 
  filter(price < mean(price) 
         & mileage < mean(mileage)) %>%
  filter(sd_price < -.4 #Here's our standard deviation boundary for price
         & sd_mileage < -.8 #Here's our standard deviation boundary for mileage
         )
```

Alright- let's find out just how many trucks we got.

```r
print(paste("We captured", nrow(ford_diesel_sd_outliers), "outliers")
      , quote = FALSE)
```

```
## [1] We captured 47 outliers
```

Great!  Much better than the original histogram method.  Now let's take a look at a couple of histograms of our new prices and mileage!


```r
par(mfrow=c(1,2)) # For plotting two plots next to each other
hist(ford_diesel_sd_outliers$price,
     main = "Outlier Prices Distribution",
     col="dodgerblue1",
     xlab = "Price")
hist(ford_diesel_sd_outliers$mileage, 
     main = "Outlier Mileage Distribution", 
     col="#E69F00",
     xlab = "Mileage")
```

![](used_car_outlier_detection_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Great!  So we got 47 cars that are...  

1. between approximately 70k and 110k in mileage  
2. between approximately \$18k and \$26k

Let's visualize this data in context of all the cars and see what we find out!


```r
mileage_boundary_sd <- max(ford_diesel_sd_outliers$mileage)
price_boundary_sd <- max(ford_diesel_sd_outliers$price)

#Plotting the mileage and price outliers in the context of all our data

ggplot() +
  
  #First plotting the red outliers
  geom_point(data=ford_diesel_sd_outliers, aes(x=mileage, y=price), 
             size=3,
             color="#55C667FF") +
  
  #Plotting the rest of the data
  geom_point(data=ford_diesel_trucks, aes(x=mileage, y=price)) + 
  
  #Plot the price boundary we set as a line
  geom_segment(data=ford_diesel_trucks, 
               aes(x=0, xend=mileage_boundary_sd, y=price_boundary_sd, yend=price_boundary_sd), 
               color="black", size=1.25) +

  #Plot the mileage boundary we set as a line
  geom_segment(data=ford_diesel_trucks, 
               aes(x=mileage_boundary_sd, xend=mileage_boundary_sd, y=0, yend=price_boundary_sd), 
               color="black", size=1.25) +
  
  theme_classic() +
  
  labs(x="Mileage", 
       y="Price",
       title="Ford Diesel Trucks: Outliers Are Circled in Green")
```

![](used_car_outlier_detection_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
  
This looks like it's doing a pretty good job!  As you can see, the boundaries look very hard set.  I'd say that this method worked quite well!

If you want to peruse these vehicles and see if there are any common themes, check out the table below.  I've noticed that they are usually very large vehicles like F-250's and F-350's.  Perhaps these are priced lower because larger vehicles depreciate more quickly?


```r
datatable(ford_diesel_sd_outliers)
```

<!--html_preserve--><div id="htmlwidget-0835fafe4b3a9caed3f3" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0835fafe4b3a9caed3f3">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47"],["2004 FORD F-350 LARIAT SUPER DUTY CREW PICK UP DIESEL RWD","2006 FORD F650 XL DIESEL ALLISON AUTO 16FT BOX LIFT GATE ONLY 70K MILES","This is a Great Truck with very Low Mileage!!!","2006 Ford F250 Diesel 2WD XLT Supercab Long Bed Extended Cab","F250 6.0 4x4 66K miles CLEAN CLEAN BONE STOCK 6.0 Lariat","59294 Miles 2005 Ford F-550 RWD Crew Cab Truck Towing Package Automatic Bedliner","2004 Ford King Ranch","2005 Ford F-350 Lariat","2006 Ford F-250","2005 Ford F-350 XLT","2007 Ford F350 Service Utility Body 6.0L Diesel Low Miles Dual Tanks 4:10 Gears","2004 Ford F-250 Lariat","2006 Ford F-250 XL","2004 Ford F-350 XL 6.0 Diesel Only 47K Miles 1-Owner Excellent Condition","2001 FORD F-250 XLT 7.3L TURBO DIESEL * 59000 ACTUAL MILES NO RUST 7.3 F-350 F2","2006 Ford F-550 XL","2005 Ford F350 SRW Service Utility Body 6.0L Diesel 1 Owner FL City Fleet Truck","2006 Ford F-550 XL","2006 Ford F-250 XLT","2005 Ford F-350 King Ranch","2005 Ford F-350 King Ranch","2005 Ford F-350 King Ranch","2008 Ford F-350 XLT","2004 Ford F-350 XLT","2006 Ford F-250 Lariat","2000 Ford F-250 FX4","2007 Ford F350 SRW Service Utility Body 6.0L Diesel 1 Owner FL City Fleet Truck","2005 Ford F-250 Super Duty XLT 2dr Standard Cab Rwd LB Automatic 5-Speed RWD V8","2001 Ford F-250 XLT","2004 Ford F-250 XLT","2004 Ford F-250 XLT","2000 Ford F-250 Lariat","2004 Ford F-350","2006 Ford F-350 XLT","2004 Ford F350 DRW Crew Cab 6.0L Diesel Super Duty 1 Owner FL Clean Carfax 4.10","SUPERDUTY DUMPTRUCK DRW 4WD HEATED MIRRORS AC TOOLBOXES AND MORE!","2001 Ford F-550 XL","LOW MILE DIESAL 2006 Ford XLT! ALL REPAIRED! Financing!","2007 Ford F250 Diesel 4x4 XLT Supercab Extended Cab","3 OWNER CLEAN CARFAX 6.0L POWERSTROKE DIESEL XLT 4X4 POWER WINDOWS LOW MILES","2002 Ford F-350 Super Duty XL 2dr Standard Cab 4WD LB Automatic 4-Speed 4WD V8","2004 LARIAT KING RANCH 4X4 BULLETPROOFED 4DR SHORT NO RUST NO RESERVE","2003 Ford F-450 Regular Cab XL Diesel 11ft. Flatbed Low Miles Great Shape !!","2005 Ford F-350 King Ranch","2009 Ford F-450 Regular Cab XL Diesel 11ft. Flatbed Low Miles Clean!!","08 Ford F-350 F350 XL Regular Cab Dump Truck 4x4 6.4L Power Stroke Diesel 4WD","2003 Ford F350 Service Utility Body DRW 6.0L Turbo Diesel Super Duty 1 Owner FL"],[2004,2006,2006,2006,2003,2005,2004,2005,2006,2005,2007,2004,2006,2004,2001,2006,2005,2006,2006,2005,2005,2005,2008,2004,2006,2000,2007,2005,2001,2004,2004,2000,2004,2006,2004,2000,2001,2006,2007,2003,2002,2004,2003,2005,2009,2008,2003],["Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford"],["F-350","XL DRW","F-350","F-250","F-250","Other","F-350","F-350","F-250","F-350","F-350","F-250","F-250","F-350","F-250","F-550","F-350","F-550","F-250","F-350","F-350","F-350","F-350","F-350","F-250","F-250","F-350","F-250","F-250","F-250","F-250","F-250","F-350","F-350","F-350","F-450","F-550","F-250","F-250","F-350","F-350","F-250","F-450","F-350","F-450","F-350","F-350"],["NULL","16 ft Box with Lift Gate","Lariat","NULL","NULL","Lariat Dually RWD V8 Crew Cab Truck Leather Seats","NULL","Lariat","NULL","XLT","Service Utility Body","Lariat","XL","XL","NULL","XL","Service Utility Body SRW FL Truck","XL","XLT","King Ranch","King Ranch","King Ranch","XLT","XLT","Lariat","FX4","Service Utility Body SRW FL Truck","XLT 2dr Standard Cab Rwd LB","XLT","XLT","XLT","Lariat","NULL","XLT","Crew Cab 1 Owner FL Truck","SUPERDUTY DUMPTRUCK","XL","XLT","NULL","\"\"Supercab 158\"\"\"\" XLT 4WD\"\"","XL 2dr Standard Cab 4WD LB","Lariat","XL","King Ranch","XL","XL Dump Truck","Service Utility Body FL Truck"],[59434,70833,70341,66047,66344,59294,66842,55000,64000,74500,32853,73443,58927,47121,59139,71500,66324,60600,62700,17000,65000,16233,72000,72460,57600,13000,58029,72201,48500,52250,74445,18000,48500,57338,73162,67000,61000,67904,66116,74684,55900,59000,61199,17000,65314,70958,71508],[19900,18900,21500,16980,18900,20985,19999,21000,21500,11499,16995,20900,12900,12000,20900,13000,12995,13000,18500,15500,14500,16900,10000,17500,18500,9700,13995,12995,21500,18900,16995,10000,20500,18995,11995,12000,11250,15906,18980,18981,19500,10995,11990,15250,18990,21900,9995],["Automatic","Automatic","Automatic","Automatic","Manual","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","NULL","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","NULL","NULL","NULL","Automatic","Automatic","NULL","Automatic","Automatic","Automatic","Automatic","Automatic","Manual","NULL","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","NULL","Automatic","Automatic","Automatic"],["NULL","4WD","RWD","NULL","NULL","RWD","NULL","4WD","4WD","4WD","NULL","4WD","RWD","RWD","NULL","RWD","NULL","RWD","4WD","4WD","4WD","4WD","4WD","4WD","4WD","4WD","NULL","RWD","4WD","4WD","4WD","4WD","4WD","4WD","NULL","4WD","RWD","NULL","NULL","NULL","4WD","4WD","NULL","4WD","NULL","4WD","NULL"],["Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel"],["Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck"],["Burgundy","Blue","Burgundy","Tan","Gold","White","Burgundy","Brown","Gray","Brown","White","Blue","White","Red","White","NULL","White","NULL","Brown","NULL","Brown","NULL","Red","Silver","NULL","Green","White","White","White","White","Silver","NULL","Gray","White","White","Red and Black","White","White","White","Gold","White","White","White","NULL","White","Green","White"],["Pompano Beach Florida","Neenah Wisconsin","Cairo Georgia","Mansfield Texas","Wylie Texas","Kernersville North Carolina","Arlington Texas","Layton UT","Hurricane UT","Burley ID","Pompano Beach Florida","Ogden UT","Logan UT","Willoughby Ohio","Miami Florida","West Jordan UT","Pompano Beach Florida","West Jordan UT","Bluffdale UT","North logan UT","Cedar City UT","North logan UT","West jordan UT","Riverton UT","Roy UT","Provo UT","Pompano Beach Florida","Irving Texas","St. George UT","Logan UT","OREM UT","Portland OR","Pocatello ID","Layton UT","Pompano Beach Florida","Wilmington Delaware","Salt Lake City UT","Encinitas California","Mansfield Texas","Lilburn Georgia","Fredericksburg Virginia","Oak Hill Florida","Walker Louisiana","Logan UT","Walker Louisiana","South Weymouth Massachusetts","Pompano Beach Florida"],["ebay.com/itm/201736698283","ebay.com/itm/272572592886","ebay.com/itm/112330648325","ebay.com/itm/172568529762","ebay.com/itm/282450119407","ebay.com/itm/302193674278","ebay.com/itm/182419933235","ksl.com/auto/listing/3570870","ksl.com/auto/listing/3444308","ksl.com/auto/listing/3451510","ebay.com/itm/382033580905","ksl.com/auto/listing/3420366","ksl.com/auto/listing/3373456","ebay.com/itm/322373198700","ebay.com/itm/132019860707","ksl.com/auto/listing/3384940","ebay.com/itm/322431170229","ksl.com/auto/listing/3384736","ksl.com/auto/listing/3383878","ksl.com/auto/listing/3363726","ksl.com/auto/listing/3356424","ksl.com/auto/listing/3358152","ksl.com/auto/listing/3427212","ksl.com/auto/listing/3421848","ksl.com/auto/listing/3471902","ksl.com/auto/listing/3421057","ebay.com/itm/382006683780","ebay.com/itm/282339785398","ksl.com/auto/listing/3348225","ksl.com/auto/listing/3330739","ksl.com/auto/listing/3362162","ksl.com/auto/listing/3307325","ksl.com/auto/listing/3348643","ksl.com/auto/listing/3228227","ebay.com/itm/381867934285","ebay.com/itm/172581874013","ksl.com/auto/listing/3361891","ebay.com/itm/262756598546","ebay.com/itm/201742671671","ebay.com/itm/192095842706","ebay.com/itm/262841730209","ebay.com/itm/302218638439","ebay.com/itm/232189848821","ksl.com/auto/listing/3379716","ebay.com/itm/322383279356","ebay.com/itm/222395028896","ebay.com/itm/381950225079"],["eBay","eBay","eBay ","eBay ","eBay ","eBay","eBay","KSL ","KSL","KSL","eBay ","KSL","KSL ","eBay","eBay","KSL","eBay","KSL","KSL","KSL","KSL","KSL","KSL","KSL","KSL","KSL","eBay ","eBay","KSL","KSL","KSL","KSL","KSL","KSL","eBay","eBay ","KSL","eBay","eBay","eBay","eBay","eBay","eBay","KSL","eBay","eBay","eBay"],["Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean"],[-0.604238763103258,-0.690772105162418,-0.465785415808602,-0.856916121916006,-0.690772105162418,-0.510350086969069,-0.595671962239401,-0.509052086838182,-0.465785415808602,-1.33120536974226,-0.855618121785118,-0.517705421044098,-1.20997215751738,-1.28785216537062,-0.517705421044098,-1.20131882331146,-1.20175149002176,-1.20131882331146,-0.725385441986082,-0.984985468163562,-1.07151881022272,-0.863838789280738,-1.46091884948894,-0.811918784045242,-0.725385441986082,-1.48687885210669,-1.1152181479626,-1.20175149002176,-0.465785415808602,-0.690772105162418,-0.855618121785118,-1.46091884948894,-0.552318757867762,-0.682551437666798,-1.28828483208092,-1.28785216537062,-1.35275217191499,-0.949852931287543,-0.683849437797685,-0.683762904455626,-0.638852099926922,-1.37481817414008,-1.28871749879121,-1.00661880367835,-0.682984104377094,-0.431172078984938,-1.46135151619924],[-1.12522931106386,-0.882291284226285,-0.892776897806782,-0.984291582186161,-0.977961852036959,-1.1282130222453,-0.967348365120114,-1.21972770662468,-1.02791770210339,-0.804139363495222,-1.69172950330612,-0.826666382915111,-1.13603460798523,-1.38764670947171,-1.13151641676761,-0.868076031668985,-0.978388096491451,-1.10037925936699,-1.05562359164536,-2.02959217015901,-1.0066054793788,-2.04593864498877,-0.857419920306691,-0.847616297853381,-1.16431592754075,-2.11484106105736,-1.1551729839919,-0.853136163539049,-1.3582571543345,-1.2783363191173,-0.805311535745075,-2.00827994743442,-1.3582571543345,-1.1698997298946,-0.83265511750072,-0.963981033929629,-1.09185437027716,-0.944714784586602,-0.982821038818165,-0.800217914513898,-1.20054670617255,-1.13447881572633,-1.08761323795496,-2.02959217015901,-0.999913441443284,-0.879627256385712,-0.867905533887188]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>car_description<\/th>\n      <th>year<\/th>\n      <th>make<\/th>\n      <th>model<\/th>\n      <th>description<\/th>\n      <th>mileage<\/th>\n      <th>price<\/th>\n      <th>transmission<\/th>\n      <th>wheel_drive<\/th>\n      <th>fuel_type<\/th>\n      <th>body_type<\/th>\n      <th>color<\/th>\n      <th>location<\/th>\n      <th>url<\/th>\n      <th>website_name<\/th>\n      <th>title_condition<\/th>\n      <th>sd_price<\/th>\n      <th>sd_mileage<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,6,7,17,18]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

### Manually Catching Outliers

<div style="text-align:center" markdown="1">
![](C:/Users/zacha/Desktop/car_project_outlier_detection_files/custom_outlier_detection.png)
</div>
</n> 
Let's say you want to take outlier detection into your own hands.  Okay! We can do that!  We'll use percentiles.  

Below is a quick explanation of what percentiles are:

<div style="text-align:center" markdown="1">
![](C:/Users/zacha/Desktop/car_project_outlier_detection_files/percentiles.jpg)
</div>
</n> 


After some experimentation, I found that the following rule worked quite well:  

*Only keep cars within the bottom 40th percentile for price AND within the bottom 35th percentile for mileage.*

By doing this, we will get cars that have both low mileage AND a low price!  What's also nifty is that this method can be applied to any car type, and not matter what the distribution looks like, we'll still capture the best deals.


```r
#Filtering the data to include the bottom 40% of price AND bottom 35% of mileage
good_deals <- ford_diesel_trucks %>%
  filter(price < quantile(price, .4) &
         mileage < quantile(mileage, .35)
         ) %>%
        arrange(price)
```


```r
#Setting boundaries for our price and mileage again
price_boundary <- quantile(ford_diesel_trucks$price, .4)[[1]]
mileage_boundary <- quantile(ford_diesel_trucks$mileage, .35)[[1]]

ggplot() +
  #Plotting the outliers as green dots
  geom_point(data=good_deals, aes(x=mileage, y=price), size=3, color="#55C667FF") +
  
  #Plotting the rest of the trucks
  geom_point(data=ford_diesel_trucks, aes(x=mileage, y=price)) +
  
  #Plotting the price boundary line we set
  geom_segment(data=ford_diesel_trucks, 
               aes(x=0, xend=mileage_boundary, y=price_boundary, yend=price_boundary), 
               color="black", size=1.25, linestyle = "dashed") +

  #Plotting the price boundary line we set
  geom_segment(data=ford_diesel_trucks, 
               aes(x=mileage_boundary, xend=mileage_boundary, y=0, yend=price_boundary), 
               color="black", size=1.25, linestyle = "dashed") +
  theme_classic() +
  ggtitle("Outlier Detection Using Manually Set Boundaries") +
  labs(x="Mileage", y="Price") 
```

```
## Warning: Ignoring unknown parameters: linestyle

## Warning: Ignoring unknown parameters: linestyle
```

![](used_car_outlier_detection_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
As you can see, this method was a bit more forgiving with the mileage.  Once again, we customized the fit of detecting these outliers, so you can make it work for whatever data you want.


```r
#Interactive table for looking at the good deals
datatable(good_deals, options=list(lengthMenu = c(3,10,30)))
```

<!--html_preserve--><div id="htmlwidget-1381705513f3f4c1e8a8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1381705513f3f4c1e8a8">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94"],["2000 Ford F-250 FX4","2003 Ford F350 Service Utility Body DRW 6.0L Turbo Diesel Super Duty 1 Owner FL","2003 Ford F350 Service Utility Body DRW 6.0L Turbo Diesel Super Duty 1 Owner FL","2008 Ford F-350 XLT","2000 Ford F-250 Lariat","2004 LARIAT KING RANCH 4X4 BULLETPROOFED 4DR SHORT NO RUST NO RESERVE","2001 Ford F-550 XL","2005 Ford F-350 XLT","2003 Ford F-450 Regular Cab XL Diesel 11ft. Flatbed Low Miles Great Shape !!","2004 Ford F350 DRW Crew Cab 6.0L Diesel Super Duty 1 Owner FL Clean Carfax 4.10","2004 Ford F-350 XL 6.0 Diesel Only 47K Miles 1-Owner Excellent Condition","SUPERDUTY DUMPTRUCK DRW 4WD HEATED MIRRORS AC TOOLBOXES AND MORE!","2006 Ford F-250 XL","2005 Ford F350 4X4 11ft Flatbed 6.0L Diesel FL Truck 4.10 Gears Ltd Slip Axle","2005 Ford F350 SRW Service Utility Body 6.0L Diesel 1 Owner FL City Fleet Truck","2005 Ford F-250 Super Duty XLT 2dr Standard Cab Rwd LB Automatic 5-Speed RWD V8","2006 Ford F550 Crew Cab Super Duty Cab Chassis 6.0L Turbo Diesel FL Truck 4:88","2006 Ford F-550 XL","2006 Ford F-550 XL","2003 Ford F-250 FX4","2007 Ford F350 SRW Service Utility Body 6.0L Diesel 1 Owner FL City Fleet Truck","2005 Ford F-350 King Ranch","2007 Ford F-250 XL","2005 Ford F-250 XLT 6.0L Powerstroke Diesel 4x4","2008 Ford F250 Lariat SRW 2WD Turbo Diesel 6.4L FL Truck Clean Carfax","2007 Gold XLT!","2005 Ford F-350 King Ranch","2005 Ford F-350 King Ranch","2004 Ford F-350 XLT","2006 Ford F-250 XL","2007 Ford F-250","LOW MILE DIESAL 2006 Ford XLT! ALL REPAIRED! Financing!","2003 FORD F550 4X4 UTILITY BODY DUMP TRUCK 7.3 DIESEL PLOW 75K MILES FINANCING","2007 Ford F450 Service Utility Body 6.0L Turbo Diesel FL 1 Owner Clean Carfax","2006 Ford F-350 Super Duty SRW SUPER DUTY Automatic 5-Speed 6.0L Turbocharger","2003 FORD F350 7.3L DIESEL V PLOW ONLY 80K!!","2006 Ford F-250 XL","2008 Ford F-250","93564 Miles 2003 Ford F-250 RWD Towing Package Bedliner Long Bed Cruise Control","2005 Ford F-350 King Ranch","ARKANSAS 1-OWNER NONSMOKER 4X4 HEIL DUMP BED POWERSTROKE PERFECT CARFAX!","2004 Ford F-250","2006 Ford F250 Diesel 2WD XLT Supercab Long Bed Extended Cab","2007 Silver XLT!","2007 Ford F350 Service Utility Body 6.0L Diesel Low Miles Dual Tanks 4:10 Gears","2004 Ford F-250 XLT","2002 Ford F-350 Lariat","2004 Ford Econoline Cutaway","2004 Ford F-450SD DRW","2004 Ford F-350 XLT","2005 Ford F-250 XLT","2008 White XL Reading Bed 4x4 Crew Cab!","ARKANSAS-OWNED NONSMOKER 6.7L DIESEL DUALLY KNAPHEIDE UTILITY BED!  NICE!","2008 Ford Super Duty F350 XLT Crew Cab 6.4L V8 Power Stroke Turbo Diesel Engine","2008 Ford F-350 Crew Cab XL Diesel Flatbed Low Miles !!","2008 Ford F-550 Regular Cab XL Diesel Flat Bed Low Miles!!","2005 Ford F550 KUV Service Body 1 Owner Clean Carfax 6.0L Turbo Diesel 4:88 Gear","2004 Ford F-350 FX4","2006 Ford F-350 Lariat","2006 Ford F-250 XLT","2006 Ford F-250 Lariat","2006 Ford F-250 Lariat","SHARP 4WD EZ LIFE 4 DOOR TURBO DIESEL 4X4 SUPER EXT CAB LEATHER CAMPER TOP WAGON","2006 FORD F650 XL DIESEL ALLISON AUTO 16FT BOX LIFT GATE ONLY 70K MILES","F250 6.0 4x4 66K miles CLEAN CLEAN BONE STOCK 6.0 Lariat","2004 Ford F-250 XLT","2005 Ford F-350 Lariat","2009 FORD F-350 SUPER DUTY 4X4 FLATBED WITH UTILITY BOX REG CAB LOW MILES 88K","2007 Ford F250 Diesel 4x4 XLT Supercab Extended Cab","2009 Ford F250 Diesel 2WD XL SuperCab Utility Bed 1 TEXAS OWNER","3 OWNER CLEAN CARFAX 6.0L POWERSTROKE DIESEL XLT 4X4 POWER WINDOWS LOW MILES","2009 Ford F-450 Regular Cab XL Diesel 11ft. Flatbed Low Miles Clean!!","2006 Ford F-350 XLT","2001 Ford F-350 XLT","2006 Ford F-250 XLT","2004 Ford F-350 Lariat","2007 Ford F-250 Lariat","2002 Ford F-350 Super Duty XL 2dr Standard Cab 4WD LB Automatic 4-Speed 4WD V8","2004 FORD F-350 LARIAT SUPER DUTY CREW PICK UP DIESEL RWD","2006 Ford F-350 XL","09 Ford F-350 XLT Regular Cab Utility Truck 4x4 6.4L Powerstroke Diesel F350 AC","2004 White King Ranch!","05 F250 4x4 Lariat Leather 92k Miles Crew Power Stroke Diesel We Finance Texas","2004 Ford King Ranch","2004 Ford F-350","2008 FORD F250 DIESEL 4X4 LEATHER 74700 MILES POWERSTROKE EXTENDED-CAB BEDLINER","2004 Ford F-250 Lariat","2001 FORD F-250 XLT 7.3L TURBO DIESEL * 59000 ACTUAL MILES NO RUST 7.3 F-350 F2","06 Ford F-350 F350 Crew Cab 4 Door King Ranch 6.0L Powerstroke Diesel 4x4 Loaded","59294 Miles 2005 Ford F-550 RWD Crew Cab Truck Towing Package Automatic Bedliner","2005 Ford F-350 Lariat","This is a Great Truck with very Low Mileage!!!","2006 Ford F-250","2001 Ford F-250 XLT"],[2000,2003,2003,2008,2000,2004,2001,2005,2003,2004,2004,2000,2006,2005,2005,2005,2006,2006,2006,2003,2007,2005,2007,2005,2008,2007,2005,2005,2004,2006,2007,2006,2003,2007,2006,2003,2006,2008,2003,2005,2003,2004,2006,2007,2007,2004,2002,2004,2004,2004,2005,2008,2011,2008,2008,2008,2005,2004,2006,2006,2006,2006,2003,2006,2003,2004,2005,2009,2007,2009,2003,2009,2006,2001,2006,2004,2007,2002,2004,2006,2009,2004,2005,2004,2004,2008,2004,2001,2006,2005,2005,2006,2006,2001],["Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford","Ford"],["F-250","F-350","F-350","F-350","F-250","F-250","F-550","F-350","F-450","F-350","F-350","F-450","F-250","F-350","F-350","F-250","F-550","F-550","F-550","F-250","F-350","F-350","F-250","F-250","F-250","F-250","F-350","F-350","F-350","F-250","F-250","F-250","F-550","F-450","F-350","F-350","F-250","F-250","F-250","F-350","F-550","F-250","F-250","F-250","F-350","F-250","F-350","Econoline Cutaway","F-450","F-350","F-250","F-550","F-350","F-350","F-350","F-550","F-550","F-350","F-350","F-250","F-250","F-250","F-350","XL DRW","F-250","F-250","F-350","F-350","F-250","F-250","F-350","F-450","F-350","F-350","F-250","F-350","F-250","F-350","F-350","F-350","F-350","F-250","F-250","F-350","F-350","F-250","F-250","F-250","F-350","Other","F-350","F-350","F-250","F-250"],["FX4","Service Utility Body","Service Utility Body FL Truck","XLT","Lariat","Lariat","XL","XLT","XL","Crew Cab 1 Owner FL Truck","XL","SUPERDUTY DUMPTRUCK","XL","4X4 Flatbed FL Truck","Service Utility Body SRW FL Truck","XLT 2dr Standard Cab Rwd LB","Cab Chassis Crew Cab","XL","XL","FX4","Service Utility Body SRW FL Truck","King Ranch","XL","NULL","\"\"2WD Crew Cab 156\"\"\"\" Lariat\"\"","NULL","King Ranch","King Ranch","XLT","XL","NULL","XLT","XL","Service Utility Body","SRW SUPER DUTY","XLT","XL","NULL","Lariat RWD V8 Diesel Extended Cab Truck Cloth","King Ranch","XLT w/Heil Dump Body","NULL","NULL","XLT","Service Utility Body","XLT","Lariat","NULL","NULL","XLT","XLT","XL Reading Bed 4x4 Crew Cab","XL w/Knapheide KUV Service Body","XLT","XL","XL","KUV Service Body","FX4","Lariat","XLT","Lariat","Lariat","1-OWNER 94K F350 1 TON LARIAT 6.0L POWERSTROKE V8","16 ft Box with Lift Gate","NULL","XLT","Lariat","NULL","NULL","NULL","\"\"Supercab 158\"\"\"\" XLT 4WD\"\"","XL","XLT","XLT","XLT","Lariat","Lariat","XL 2dr Standard Cab 4WD LB","NULL","XL","XLT Utility Truck","King Ranch","NULL","NULL","NULL","NULL","Lariat","NULL","Crew Cab King Ranch","Lariat Dually RWD V8 Crew Cab Truck Leather Seats","Lariat","Lariat","NULL","XLT"],[13000,80796,71508,72000,18000,59000,61000,74500,61199,73162,47121,67000,58927,87040,66324,72201,89321,71500,60600,93499,58029,65000,92230,84818,91603,87150,17000,17000,91000,87320,81295,67904,75210,93086,90582,80160,89902,75104,93564,16233,90030,88975,66047,75921,32853,74445,85000,87000,81190,72460,89000,90300,78303,89780,89632,88700,75299,83000,79500,62700,57600,84000,93960,70833,66344,52250,90280,88869,66116,85867,74684,65314,57338,83500,94000,80000,80200,55900,59434,83000,85645,76890,92125,66842,48500,74700,73443,59139,88434,59294,55000,70341,64000,48500],[9700,9995,9995,10000,10000,10995,11250,11499,11990,11995,12000,12000,12900,12995,12995,12995,12995,13000,13000,13995,13995,14500,14900,14995,14995,14999,15250,15500,15500,15900,15900,15906,15995,15995,15995,15995,16000,16500,16785,16900,16900,16900,16980,16988,16995,16995,17000,17000,17500,17500,17600,17900,17900,17980,17990,17990,17995,18000,18000,18500,18500,18500,18890,18900,18900,18900,18900,18900,18980,18980,18981,18990,18995,19000,19000,19450,19500,19500,19900,19900,19900,19981,19995,19999,20500,20598,20900,20900,20900,20985,21000,21500,21500,21500],["Automatic","Automatic","Automatic","Automatic","NULL","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Manual","Automatic","Automatic","Automatic","Automatic","NULL","Automatic","Automatic","Automatic","Automatic","NULL","NULL","Manual","Automatic","NULL","Automatic","Automatic","Automatic","Automatic","Automatic","NULL","Automatic","Automatic","NULL","Manual","Automanual","Automatic","Automatic","Automatic","Manual","Automatic","NULL","Automatic","Automatic","Automanual","Automatic","Automatic","Automatic","Automatic","Manual","Automatic","Automatic","Automatic","Automatic","NULL","Automatic","Automatic","Automatic","Manual","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","NULL","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic","Automatic"],["4WD","NULL","NULL","4WD","4WD","4WD","RWD","4WD","NULL","NULL","RWD","4WD","RWD","NULL","NULL","RWD","NULL","RWD","RWD","4WD","NULL","4WD","4WD","NULL","NULL","NULL","4WD","4WD","4WD","4WD","RWD","NULL","4WD","NULL","4WD","4WD","4WD","4WD","RWD","4WD","NULL","4WD","NULL","NULL","NULL","4WD","4WD","NULL","NULL","4WD","4WD","NULL","NULL","NULL","NULL","NULL","NULL","4WD","4WD","4WD","4WD","4WD","4WD","4WD","NULL","4WD","4WD","NULL","NULL","NULL","NULL","NULL","4WD","4WD","4WD","4WD","4WD","4WD","NULL","4WD","4WD","NULL","NULL","NULL","4WD","NULL","4WD","NULL","4WD","RWD","4WD","RWD","4WD","4WD"],["Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel","Diesel"],["Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck","Truck"],["Green","White","White","Red","NULL","White","White","Brown","White","White","Red","Red and Black","White","White","White","White","White","NULL","NULL","Red","White","Brown","White","Green","Gold","Gold","NULL","NULL","White","White","Green","White","Yellow","White","Gold","White","Tan","Yellow","Gray","NULL","Black","Red","Tan","Silver","White","Silver","Red","White","Tan","Silver","Gray","White","White","Red","White","White","White","Blue","White","Brown","NULL","NULL","White","Blue","Gold","White","Gray","White","White","White","Gold","White","White","Blue","White","White","White","White","Burgundy","Red","White","White","Silver","Burgundy","Gray","White","Blue","White","Black","White","Brown","Burgundy","Gray","White"],["Provo UT","Pompano Beach Florida","Pompano Beach Florida","West jordan UT","Portland OR","Oak Hill Florida","Salt Lake City UT","Burley ID","Walker Louisiana","Pompano Beach Florida","Willoughby Ohio","Wilmington Delaware","Logan UT","Pompano Beach Florida","Pompano Beach Florida","Irving Texas","Pompano Beach Florida","West Jordan UT","West Jordan UT","Layton UT","Pompano Beach Florida","Cedar City UT","CLEARFIELD UT","Decatur Illinois","Pompano Beach Florida","Addison Texas","Logan UT","North logan UT","Taylorsville UT","Logan UT","St. George UT","Encinitas California","Rye New Hampshire","Pompano Beach Florida","Dallas Texas","Dover New Jersey","Salt Lake City UT","River Heights UT","Kernersville North Carolina","North logan UT","Marion Arkansas","Lehi UT","Mansfield Texas","Addison Texas","Pompano Beach Florida","OREM UT","Eden UT","Taylorsville UT","Conway South Carolina","Riverton UT","Saratoga springs UT","Plano Texas","Marion Arkansas","Corpus Christi Texas","Walker Louisiana","Walker Louisiana","Pompano Beach Florida","DRAPER  UT","North Ogden UT","Bluffdale UT","Roy UT","MIDVALE UT","Canton Georgia","Neenah Wisconsin","Wylie Texas","Logan UT","Elk Ridge UT","Houston Texas","Mansfield Texas","Mansfield Texas","Lilburn Georgia","Walker Louisiana","Layton UT","CODY WY","Cedar city UT","Peoa UT","Page AZ","Fredericksburg Virginia","Pompano Beach Florida","Brigham City UT","South Weymouth Massachusetts","Addison Texas","Mansfield Texas","Arlington Texas","Pocatello ID","Houston Texas","Ogden UT","Miami Florida","South Weymouth Massachusetts","Kernersville North Carolina","Layton UT","Cairo Georgia","Hurricane UT","St. George UT"],["ksl.com/auto/listing/3421057","ebay.com/itm/322401535880","ebay.com/itm/381950225079","ksl.com/auto/listing/3427212","ksl.com/auto/listing/3307325","ebay.com/itm/302218638439","ksl.com/auto/listing/3361891","ksl.com/auto/listing/3451510","ebay.com/itm/232189848821","ebay.com/itm/381867934285","ebay.com/itm/322373198700","ebay.com/itm/172581874013","ksl.com/auto/listing/3373456","ebay.com/itm/232155578539","ebay.com/itm/322431170229","ebay.com/itm/282339785398","ebay.com/itm/381996711096","ksl.com/auto/listing/3384940","ksl.com/auto/listing/3384736","ksl.com/auto/listing/3479593","ebay.com/itm/382006683780","ksl.com/auto/listing/3356424","ksl.com/auto/listing/3390918","ebay.com/itm/232166495616","ebay.com/itm/322398517621","ebay.com/itm/302155338346","ksl.com/auto/listing/3379716","ksl.com/auto/listing/3363726","ksl.com/auto/listing/3354805","ksl.com/auto/listing/3453773","ksl.com/auto/listing/3370335","ebay.com/itm/262756598546","ebay.com/itm/322421765145","ebay.com/itm/382015894533","ebay.com/itm/172541304585","ebay.com/itm/122245366670","ksl.com/auto/listing/3357910","ksl.com/auto/listing/3448661","ebay.com/itm/302297837686","ksl.com/auto/listing/3358152","ebay.com/itm/192154677188","ksl.com/auto/listing/3340570","ebay.com/itm/172568529762","ebay.com/itm/302290457642","ebay.com/itm/382033580905","ksl.com/auto/listing/3362162","ksl.com/auto/listing/3571139","ksl.com/auto/listing/3310750","ebay.com/itm/172542692931","ksl.com/auto/listing/3421848","ksl.com/auto/listing/3399117","ebay.com/itm/262869929144","ebay.com/itm/201890457696","ebay.com/itm/182479784701","ebay.com/itm/232279676731","ebay.com/itm/222482629117","ebay.com/itm/232155570042","ksl.com/auto/listing/3473666","ksl.com/auto/listing/3364461","ksl.com/auto/listing/3383878","ksl.com/auto/listing/3471902","ksl.com/auto/listing/3371910","ebay.com/itm/262949234581","ebay.com/itm/272572592886","ebay.com/itm/282450119407","ksl.com/auto/listing/3330739","ksl.com/auto/listing/3335680","ebay.com/itm/262745247227","ebay.com/itm/201742671671","ebay.com/itm/172514115841","ebay.com/itm/192095842706","ebay.com/itm/322383279356","ksl.com/auto/listing/3228227","ksl.com/auto/listing/3454247","ksl.com/auto/listing/3326428","ksl.com/auto/listing/3344573","ksl.com/auto/listing/3383108","ebay.com/itm/262841730209","ebay.com/itm/201736698283","ksl.com/auto/listing/3345184","ebay.com/itm/352017272007","ebay.com/itm/302155340226","ebay.com/itm/252680948670","ebay.com/itm/182419933235","ksl.com/auto/listing/3348643","ebay.com/itm/122243449430","ksl.com/auto/listing/3420366","ebay.com/itm/132019860707","ebay.com/itm/351962292853","ebay.com/itm/302193674278","ksl.com/auto/listing/3570870","ebay.com/itm/112330648325","ksl.com/auto/listing/3444308","ksl.com/auto/listing/3348225"],["KSL","eBay","eBay","KSL","KSL","eBay","KSL","KSL","eBay","eBay","eBay","eBay ","KSL ","eBay","eBay","eBay","eBay ","KSL","KSL","KSL","eBay ","KSL","KSL","eBay","eBay","eBay","KSL","KSL","KSL","KSL","KSL","eBay","eBay","eBay ","eBay","eBay","KSL","KSL","eBay ","KSL","eBay ","KSL","eBay ","eBay ","eBay ","KSL","KSL ","KSL","eBay","KSL","KSL","eBay","eBay","eBay ","eBay ","eBay ","eBay","KSL","KSL","KSL","KSL","KSL","eBay ","eBay","eBay ","KSL","KSL","eBay","eBay","eBay","eBay","eBay","KSL","KSL","KSL","KSL","KSL","eBay","eBay","KSL","eBay ","eBay","eBay","eBay","KSL","eBay","KSL","eBay","eBay","eBay","KSL ","eBay ","KSL","KSL"],["Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean","Clean"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>car_description<\/th>\n      <th>year<\/th>\n      <th>make<\/th>\n      <th>model<\/th>\n      <th>description<\/th>\n      <th>mileage<\/th>\n      <th>price<\/th>\n      <th>transmission<\/th>\n      <th>wheel_drive<\/th>\n      <th>fuel_type<\/th>\n      <th>body_type<\/th>\n      <th>color<\/th>\n      <th>location<\/th>\n      <th>url<\/th>\n      <th>website_name<\/th>\n      <th>title_condition<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"lengthMenu":[3,10,30],"columnDefs":[{"className":"dt-right","targets":[2,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

It looks like there are very few cars that are selling for this low a price under 50,000 miles, but we've captured some pretty low-mileage low-price cars! 

## In Conclusion

In the end, Jared didn't need any of these solutions.  He was already implimenting something very similar to it!

But I then learned about how much time he wastes sifting through fake car postings his model sends him.  

I now have a new project to work on :)

But what's the lesson here?  

Put a lot of effort into communicating with your clients to find out what their real concerns are.  Then build off of what has already been accomplished! By doing this, you will avoid wasting time reinventing what's already been done and truly deliver on what your clients need :)

Also, we learned about outlier detection- yay!



## In Conclusion

In the end, Jared didn't need any of these solutions.  He was already implimenting something very similar to it!

But I then learned about how much time he wastes sifting through fake car postings his model sends him.  

I now have a new project to work on :)

But what's the lesson here?  

Put a lot of effort into communicating with your clients to find out what their real concerns are.  Then build off of what has already been accomplished! By doing this, you will avoid wasting time reinventing what's already been done and truly deliver on what your clients need :)
