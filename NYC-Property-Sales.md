NYC Property Sales
================
Fredrick Boshe
24/05/2021

New York city is known to be one of the most expensive cities in the
world when it comes to real estate. The city has 5 different
**boroughs**, with each having its unique socioeconomic profile that
helps determine the real estate prices.

<center>
<img src="NYC-Property-Sales_files/figure-gfm/S0nLu-new-york-city-boroughs.png" style="width:65.0%" />
</center>

Using data shared by the [New York City department of
Finance](https://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page),
this project looks to analyze housing costs for the last 12 months and
use regression models to predict prices based on indicators.

The data is found in 4 distinct excel files. Read them to the
environment and merge them in a single dataframe, making it easier to
handle.

``` r
#The boroughs are coded (manhattan=1, bronx=2, brooklyn=3, queens=4, staten=5)
nyc<-bind_rows(manhattan,bronx,brooklyn,queens,staten)

#Recode the boroughs
nyc<-nyc%>%
  mutate(
    BOROUGH=case_when(
      BOROUGH=="1" ~ "Manhattan",
      BOROUGH=="2" ~ "Bronx",
      BOROUGH=="3" ~ "Brooklyn",
      BOROUGH=="4" ~ "Queens",
      BOROUGH=="5" ~ "Staten Island"
      )
    )

#remove the individual dataframes
rm(bronx, brooklyn, manhattan, queens, staten)

#Normalize the column names (lower column names and remove space)
colnames(nyc)<-str_to_lower(colnames(nyc))%>%
  str_replace_all("\\s", "_")%>%
  str_to_title(colnames(nyc))
```

    ## Warning in opts["locale"] <- locale: number of items to replace is not a
    ## multiple of replacement length

``` r
#some of the properties were exchanged between family members (i.e. filter with a threshold of 10,000$)
summary(nyc$Sale_price)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##         0         0    470000   1184058    880000 809912583

``` r
nyc<-nyc%>%
  filter(Sale_price>10000)

#Some properties have square footage of 0, which is unlikely in reality. Remove
summary(nyc$Gross_square_feet)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##       0    1300    1791    7190    2544 2400000   22601

``` r
nyc<-nyc%>%
  filter(Gross_square_feet>0)

#Initial removal of variables that are not of interest
nyc<-nyc%>%
  select(-5,-6,-7,-8,-10)
```

The initial data cleaning and manipulation helps set the data up ready
for exploration and followed by analysis.

``` r
plot_intro(nyc, ggtheme = theme_bw())
```

<img src="NYC-Property-Sales_files/figure-gfm/explore-1.png" style="display: block; margin: auto;" />

``` r
sum(is.na(nyc$Year_built))
```

    ## [1] 14

``` r
nyc<-nyc%>%
  drop_na(Year_built)

table(nyc$Borough)
```

    ## 
    ##         Bronx      Brooklyn     Manhattan        Queens Staten Island 
    ##          2491          5960           616          8066          4067

``` r
#Plot continuous and discrete values
plot_histogram(nyc, ggtheme = theme_bw(), ncol = 2)
```

<img src="NYC-Property-Sales_files/figure-gfm/explore-2.png" style="display: block; margin: auto;" />

``` r
plot_bar(nyc, maxcat = 5, 
         ggtheme = theme_bw())
```

<img src="NYC-Property-Sales_files/figure-gfm/explore-3.png" style="display: block; margin: auto;" />

``` r
#Keep only family dwellings only
nyc<-nyc%>%
  filter(Building_class_category=="01 ONE FAMILY DWELLINGS" | Building_class_category=="02 TWO FAMILY DWELLINGS" | Building_class_category=="03 THREE FAMILY DWELLINGS")%>%
  mutate(Building_class_category=
           case_when(
             Building_class_category=="01 ONE FAMILY DWELLINGS" ~ "One Family Dwellings",
             Building_class_category=="02 TWO FAMILY DWELLINGS" ~ "Two Family Dwellings",
             Building_class_category=="03 THREE FAMILY DWELLINGS" ~ "Three Family Dwellings"
             )
         )

table(nyc$Borough)
```

    ## 
    ##         Bronx      Brooklyn     Manhattan        Queens Staten Island 
    ##          2123          4961           148          7499          3965

``` r
plot_bar(nyc, maxcat = 5,
         ncol = 2,
         ggtheme = theme_bw())
```

<img src="NYC-Property-Sales_files/figure-gfm/explore-4.png" style="display: block; margin: auto;" />
Very few missing observations (14), and they seem to be from the *Year
Built* column. We can go ahead and remove them since they make up just
0.04% of the data.

You can also observe that most variables have very huge outliers that
skew their charts positively. Going forward, we might need to deal with
the outliers. One potential way is by keeping just residential dwellings
(building codes can be found
[here](https://www1.nyc.gov/assets/finance/jump/hlpbldgcode.html)).
Manhattan will see a considerable drop in observations (<span
style="color: red;">76%</span>) while the least drop in observations was
for Staten Island (<span style="color: red;">2%</span>). coincidentally
lower the number of observations from Manhattan.

``` r
#Residential Units
quartiles <- quantile(nyc$Residential_units)
# 75% minus 25% = interquartile range 
iqr <- quartiles[[4]] - quartiles[[2]]
# Outlier boundaries
lower_bound <- quartiles[[2]] - (1.5 * iqr)
upper_bound <- quartiles[[4]] + (1.5 * iqr)

# Isolate outlier(s)
res.outliers <- nyc%>% 
  filter(Residential_units > upper_bound | Residential_units< lower_bound)

#Land
quartiles <- quantile(nyc$Land_square_feet)
iqr <- quartiles[[4]] - quartiles[[2]]
lower_bound <- quartiles[[2]] - (1.5 * iqr)
upper_bound <- quartiles[[4]] + (1.5 * iqr)

land.outliers <- nyc%>% 
  filter(Land_square_feet > upper_bound | Land_square_feet< lower_bound)

#Price
quartiles <- quantile(nyc$Sale_price)
iqr <- quartiles[[4]] - quartiles[[2]]
lower_bound <- quartiles[[2]] - (1.5 * iqr)
upper_bound <- quartiles[[4]] + (1.5 * iqr)

price.outliers <- nyc%>% 
  filter(Borough=="Bronx")%>%
  filter(Sale_price > upper_bound | Sale_price< lower_bound)
```

``` r
#Recode columns to proper data types
nyc<-nyc%>%
  mutate(Borough=as.factor(Borough),
         Neighborhood=as.factor(Neighborhood),
         Tax_class_at_time_of_sale=as.factor(Tax_class_at_time_of_sale),
         Zip_code=as.factor(Zip_code),
         Building_class_at_time_of_sale=as.factor(Building_class_at_time_of_sale),
         Building_class_category=as.factor(Building_class_category))
nyc$Address<-NULL
nyc$Neighborhood<-NULL
nyc$Zip_code<-NULL
nyc$Tax_class_at_present<-NULL


#Check for multicorrliniality
numnyc <- names(which(sapply(nyc, is.numeric)))
corr <- cor(nyc[,numnyc], use = 'pairwise.complete.obs')
p3<-ggcorrplot(corr, lab = TRUE)
p3
```

<img src="NYC-Property-Sales_files/figure-gfm/cleaning2-1.png" style="display: block; margin: auto;" />

``` r
#Total units has strong relationship with Residential units, so i shall remove it
nyc$Total_units<-NULL

#Visualize relationship between slae price and gross size
nyc%>%ggplot(aes(x=Gross_square_feet, y=Sale_price, color=Borough))+
  geom_point()+
  theme_bw()+
  geom_smooth(method = "lm", se = FALSE)+
  theme(legend.position = "none")+
  facet_wrap(~Borough, ncol = 2, scales = "free")
```

    ## `geom_smooth()` using formula 'y ~ x'

<img src="NYC-Property-Sales_files/figure-gfm/cleaning2-2.png" style="display: block; margin: auto;" />

``` r
#Bronx has two duplicate outliers, remove them
nyc<-nyc%>%
  filter(Sale_price!=87400000)

#remove duplicates
nyc<-nyc%>%
  distinct()

#Visualize relationship again
nyc%>%ggplot(aes(x=Gross_square_feet, y=Sale_price, color=Borough))+
  geom_point()+
  theme_bw()+
  geom_smooth(method = "lm", se = FALSE)+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)+
  facet_wrap(~Borough, ncol = 2, scales = "free")
```

    ## `geom_smooth()` using formula 'y ~ x'

<img src="NYC-Property-Sales_files/figure-gfm/cleaning2-3.png" style="display: block; margin: auto;" />

``` r
#Regression
nyc_fit<-lm(Sale_price~Borough+Building_class_category+
              Residential_units+Commercial_units+Land_square_feet+
              Gross_square_feet+Year_built+Building_class_at_time_of_sale, data=nyc)
summary(nyc_fit)
```

    ## 
    ## Call:
    ## lm(formula = Sale_price ~ Borough + Building_class_category + 
    ##     Residential_units + Commercial_units + Land_square_feet + 
    ##     Gross_square_feet + Year_built + Building_class_at_time_of_sale, 
    ##     data = nyc)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -6413391  -214732     3194   190642 43065519 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                                                 Estimate Std. Error t value
    ## (Intercept)                                    3.352e+06  4.742e+05   7.069
    ## BoroughBrooklyn                                4.760e+05  2.189e+04  21.745
    ## BoroughManhattan                               4.682e+06  7.748e+04  60.428
    ## BoroughQueens                                  3.138e+05  2.077e+04  15.109
    ## BoroughStaten Island                           1.637e+05  2.438e+04   6.716
    ## Building_class_categoryThree Family Dwellings  2.515e+05  1.620e+05   1.553
    ## Building_class_categoryTwo Family Dwellings    7.528e+05  4.307e+05   1.748
    ## Residential_units                             -4.468e+05  7.634e+04  -5.853
    ## Commercial_units                              -9.398e+05  4.128e+05  -2.277
    ## Land_square_feet                              -2.248e+01  3.802e+00  -5.911
    ## Gross_square_feet                              6.109e+02  1.012e+01  60.373
    ## Year_built                                    -1.614e+03  2.388e+02  -6.759
    ## Building_class_at_time_of_saleA1              -2.237e+05  5.328e+04  -4.198
    ## Building_class_at_time_of_saleA2              -4.642e+04  5.529e+04  -0.840
    ## Building_class_at_time_of_saleA3              -2.991e+05  7.909e+04  -3.782
    ## Building_class_at_time_of_saleA4               1.633e+06  9.214e+04  17.727
    ## Building_class_at_time_of_saleA5              -1.850e+05  5.435e+04  -3.403
    ## Building_class_at_time_of_saleA6              -5.088e+04  1.542e+05  -0.330
    ## Building_class_at_time_of_saleA7               2.633e+06  2.821e+05   9.334
    ## Building_class_at_time_of_saleA9              -2.130e+05  6.158e+04  -3.459
    ## Building_class_at_time_of_saleB1              -7.822e+05  4.174e+05  -1.874
    ## Building_class_at_time_of_saleB2              -7.540e+05  4.176e+05  -1.805
    ## Building_class_at_time_of_saleB3              -5.940e+05  4.176e+05  -1.422
    ## Building_class_at_time_of_saleB9              -6.468e+05  4.184e+05  -1.546
    ## Building_class_at_time_of_saleC0                      NA         NA      NA
    ## Building_class_at_time_of_saleS0               1.819e+06  9.527e+05   1.909
    ## Building_class_at_time_of_saleS1               4.466e+05  4.157e+05   1.074
    ## Building_class_at_time_of_saleS2                      NA         NA      NA
    ##                                               Pr(>|t|)    
    ## (Intercept)                                   1.62e-12 ***
    ## BoroughBrooklyn                                < 2e-16 ***
    ## BoroughManhattan                               < 2e-16 ***
    ## BoroughQueens                                  < 2e-16 ***
    ## BoroughStaten Island                          1.93e-11 ***
    ## Building_class_categoryThree Family Dwellings 0.120482    
    ## Building_class_categoryTwo Family Dwellings   0.080481 .  
    ## Residential_units                             4.90e-09 ***
    ## Commercial_units                              0.022820 *  
    ## Land_square_feet                              3.45e-09 ***
    ## Gross_square_feet                              < 2e-16 ***
    ## Year_built                                    1.43e-11 ***
    ## Building_class_at_time_of_saleA1              2.71e-05 ***
    ## Building_class_at_time_of_saleA2              0.401126    
    ## Building_class_at_time_of_saleA3              0.000156 ***
    ## Building_class_at_time_of_saleA4               < 2e-16 ***
    ## Building_class_at_time_of_saleA5              0.000667 ***
    ## Building_class_at_time_of_saleA6              0.741477    
    ## Building_class_at_time_of_saleA7               < 2e-16 ***
    ## Building_class_at_time_of_saleA9              0.000544 ***
    ## Building_class_at_time_of_saleB1              0.060966 .  
    ## Building_class_at_time_of_saleB2              0.071020 .  
    ## Building_class_at_time_of_saleB3              0.154944    
    ## Building_class_at_time_of_saleB9              0.122118    
    ## Building_class_at_time_of_saleC0                    NA    
    ## Building_class_at_time_of_saleS0              0.056257 .  
    ## Building_class_at_time_of_saleS1              0.282687    
    ## Building_class_at_time_of_saleS2                    NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 819900 on 18646 degrees of freedom
    ## Multiple R-squared:  0.4752, Adjusted R-squared:  0.4745 
    ## F-statistic: 675.3 on 25 and 18646 DF,  p-value: < 2.2e-16

``` r
#Generate multiple linear models
##First nest data by the category
nyc_nest<-nyc%>%
  group_by(Borough)%>%
  nest()

##Second run a liner regression across all categories, using a mapping function
nyc_nest<-nyc_nest%>%
  mutate(linear_model=map(.x= data, 
                          .f= ~lm(Sale_price~Building_class_category+Residential_units+
                                    Commercial_units+Land_square_feet+Gross_square_feet+
                                    Year_built+Building_class_at_time_of_sale, data=.)
                          ))

##Third select the broom function suitable
#a) Tidy the data
nyc_nest<-nyc_nest%>%
  mutate(tidy_coef=map(.x=linear_model,
                       .f=tidy, conf.int=TRUE))

#Fourth we Unnest
nyc_nest%>%
  select(Borough, tidy_coef)%>%
  unnest(cols = tidy_coef)%>%
  filter(term=="Gross_square_feet")%>%
  arrange(estimate)
```

    ## # A tibble: 5 x 8
    ## # Groups:   Borough [5]
    ##   Borough    term      estimate std.error statistic   p.value conf.low conf.high
    ##   <fct>      <chr>        <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
    ## 1 Bronx      Gross_sq~     77.4      8.35      9.27 4.43e- 20     61.0      93.7
    ## 2 Staten Is~ Gross_sq~    131.       4.70     27.7  7.28e-155    121.      140. 
    ## 3 Queens     Gross_sq~    219.       7.30     30.0  2.41e-186    204.      233. 
    ## 4 Brooklyn   Gross_sq~    589.      19.8      29.7  1.44e-178    550.      628. 
    ## 5 Manhattan  Gross_sq~   1463.     278.        5.27 5.37e-  7    914.     2011.

``` r
#This shows that Manhattan has the highest increase in sale price for every increase in gross square feet. Bronx has the lowest


#a) glance the data (this has r-squared)
nyc_nest<-nyc_nest%>%
  mutate(glance_coef=map(.x=linear_model,
                       .f=glance))

#we Unnest
nyc_nest%>%
  select(Borough, glance_coef)%>%
  unnest(cols = glance_coef)%>%
  select(r.squared)%>%
  arrange(r.squared)
```

    ## Adding missing grouping variables: `Borough`

    ## # A tibble: 5 x 2
    ## # Groups:   Borough [5]
    ##   Borough       r.squared
    ##   <fct>             <dbl>
    ## 1 Brooklyn          0.273
    ## 2 Queens            0.353
    ## 3 Bronx             0.425
    ## 4 Staten Island     0.643
    ## 5 Manhattan         0.663

``` r
#Brooklyn has the lowest R squared while Manhattan has the highest R Squared
```
