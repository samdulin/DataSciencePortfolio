## Overview

This repository contains code related to an upcoming paper, “Quantifying
the Compounding Effects of Natural Hazard Events”. A limited, simplified
version of the code is provided here as a demo to showcase the core
functionality and potential applications. This demo is intended to give
an overview of the project while protecting the full implementation
until the paper’s publication.

Compounding effects from sequences of natural hazards often amplify the
magnitude and complexity of disruptions, posing profound challenges to
infrastructure. Despite ongoing theoretical discussions and literature
exploring the ramifications of such events, there is a pressing need to
comprehensively quantify the associated risks. The process to quantify a
natural hazard pairs has three steps: basic data cleaning, multiple
imputation, and then a Bayesian regression model.

There are two key coefficients in our model: 1) the first is the
compounding effect coefficient, which tells us the multiplicative
increase in property damage of the primary hazard (e.g. flood) given a
single standard deviation increase in the preceding hazard
(e.g. wildfire) and 2) The coefficient of the the interaction term,
which measures the multiplicative increase in the compounding effect for
each additional year. Together, these tell us the compounding effect, as
well as how it is changing over time. Granted, there is a lot of
uncertainty in these estimates, especially in the latter, and the
compounding effect is not necessarily causal.

Below, we show an example applied to the southern region of California.

## Preprocessing

``` r
# load libraries
library(caret)
library(dplyr)
library(ggplot2)
library(VIM)
library(maps)
library(stringr)
library(mice)
library(miceadds)
library(viridis)
library(MASS)
library(ggfortify)
library(brms)
library(kableExtra)
library(lubridate)
library(bayesplot)
library(tidybayes)
library(mice)

# This code automates the regression analysis so that any event pairs can be plugged in and run.

set.seed(123)

## County groups
nc <- c('Del Norte','Humboldt', 'Mendocino', 'Sonoma')
sthrn <- c('Los Angeles', 'Orange', 'Riverside', 'San Bernardino', 'San Diego', 'Imperial')
nint <- c('Lassen', 'Modoc', 'Shasta', 'Siskiyou', 'Trinity')
central <- c('Alameda', 'Contra Costa', 'Marin', 'Monterey', 'San Benito', 'San Luis Obispo', 'San Mateo', 'Santa Barbara', 'Santa Clara', 'Santa Cruz', 'Ventura')
sacramento <- c('Sutter', 'Butte', 'Colusa', 'El Dorado', 'Glenn', 'Lake', 'Nevada', 'Napa', 'Placer', 'Plumas', 'Sacramento', 'Sierra', 'Solano', 'Tehama', 'Yolo', 'Yuba')
sanjoaq <- c('Mono', 'Alpine', 'Amador', 'Calaveras', 'Fresno', 'Inyo', 'Kern', 'Kings', 'Madera', 'Mariposa', 'Merced', 'San Joaquin', 'Stanislaus', 'Tulare', 'Tuolumne')


# Add the regions defined above into a list
counties <- list(nc, sthrn, nint, central, sacramento, sanjoaq)

#### Below we single out a specific region and hazard pair
county <- sthrn
events <- c('Wildfire', 'Flood')
min <- 30
area_index <- 2
monthtf <- TRUE
day <- FALSE

areas <- read.csv("/Users/samdulin/Downloads/df_areas.csv", header = TRUE)
colnames(areas)[2] <- 'Area'
cent_area <- mean(areas[areas$County %in% central, 'Area'])

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

# Compute the mean area for each of the six regions, then scale them to be between 0 and 1
area_x <- data.frame(matrix(ncol = 2, nrow = 6))
colnames(area_x) <- c('Region', 'Area')
area_x$Region  <- c('North Coast', 'Southern', 'Northern Interior', 'Central Coast', 'Sacramento', 'San Joaquin')
j <- 1
for (i in counties){
  area_x[j, 'Area'] <- mean(areas[areas$County %in% i, 'Area'])
  j <- j + 1
}
area_x$Area <- normalize(area_x$Area)

# In the regression function, we do Box-Cox to the damage data to normalize it a bit to make the imputation better. But, we untransform it afterwards because we need count data
boxinvTransform <- function(y, lambda) {
  if (lambda == 0L) { (exp(y) - .0001) }
  else { ((y * lambda + 1)^(1/lambda) - .0001) }
}

# Define the regression function. It does imputation, runs the multiple models, and combines their results.
df1 <- read.csv("/Users/samdulin/Downloads/data_with_economic_weather3.csv", header = TRUE)
yr_list <- unique(df1$Year)
yr_list <- yr_list[order(yr_list)]

  df1$proportion_property <- df1$damage_property_adj2022/(df1$damage_crop_adj2022 + df1$damage_property_adj2022)
df1 <- df1[df1$County %in% county,]
data <- df1
types <- unique(data$EVENT_TYPE)
df1 <- df1[df1$EVENT_TYPE %in% events,]

# Check if there is enough data
if (nrow(df1) <= 10){return(c(1, NA, NA, NA))
  print('Not enough data.')}
# data is just a copy of df1. We use it to find when there are compound hazards and then scrap it
data <- df1

if (monthtf == TRUE){
  # Label months for clustering
  
  for (yr in yr_list){
    index <- which(yr_list == yr)
    data[data$Year == yr, 'MONTH'] <- data[data$Year == yr, 'MONTH']+(12*(index - 1))
  }
} else if ((monthtf == FALSE)&(day == FALSE)){
  data$week <- week(data$start_date)
  
  # Label weeks for clustering
  yr_list <- unique(data$Year)
  for (yr in yr_list){
    index <- which(yr_list == yr)
    data[data$Year == yr, 'week'] <- data[data$Year == yr, 'week']+(12*(index - 1))
  }
} else if ((monthtf == FALSE)&(day == TRUE)){
  data$days <- 0
  data$start_date <- as.Date(data$start_date)
  data <- data[order(data$start_date),]
  days <- seq(from = data$start_date[1], to = data$start_date[nrow(data)], by = 'day')
  
  # Label days for clustering
  for (dai in days){
    index <- which(days == dai)
    data[data$start_date == dai, 'days'] <- index
  }
}

if (monthtf == TRUE){
  # We just cluster based on day/week/ or month
  j = 0
  clst <- 1
  cluster <- c()
  for (i in 1:nrow(data)){
    month <- data$MONTH[i]
    j <- ceiling((month)/clst)
    cluster <- c(cluster, j)
  }
} else if ((monthtf == FALSE)&(day == FALSE)) {
  j = 0
  clst <- 1
  cluster <- c()
  for (i in 1:nrow(data)){
    wek <- data$week[i]
    cluster <- c(cluster, wek)
  }
} else if ((monthtf == FALSE)&(day == TRUE)) {
  j = 0
  clst <- 1
  cluster <- c()
  for (i in 1:nrow(data)){
    dai <- data$days[i]
    cluster <- c(cluster, dai)
  }
}

data$cluster <- cluster

# Define one-hot encoding function. We make dummy vars for the year.
data$EVENT_TYPE <- as.factor(data$EVENT_TYPE)
if (length(unique(data$EVENT_TYPE)) <= 1){
  print('Not enough data.')
  return(c(1, NA, NA, NA))
}
dummy <- dummyVars(" ~ EVENT_TYPE", data = data)

# Perform one-hot encoding
dummyvar_df <- data.frame(predict(dummy, newdata=data))

# Rename columns
colnames(dummyvar_df) <- gsub(".", " ", str_remove(colnames(dummyvar_df), 'EVENT_TYPE.'), fixed=TRUE)

## We select the features we want
bn_df <- cbind(dummyvar_df, dplyr::select(data, c('cluster','Year', 'County', 'population_density', 'total_home_value', 'duration', 'damage_property_adj2022', 'proportion_property', 'prcp')))

# Determine optimal lag time
lag_df <- bn_df[-5] %>% group_by(cluster)  %>%
  summarise_each(list(sum))

## Add any missing time periods where there was not a hazard
if (monthtf == TRUE){
  for (i in seq(1, 27*(12/clst), 1)){
    if (!(i %in% lag_df$cluster)){
      lag_df[(nrow(lag_df) + 1), ] <- list(i,0, 0, 0, 0, 0, 0, 0, .5, 0)
    }
  }
} else if ((monthtf == FALSE)&(day == FALSE)){
  for (i in seq(1, 27*(52/clst), 1)){
    if (!(i %in% lag_df$cluster)){
      lag_df[(nrow(lag_df) + 1), ] <- list(i,0, 0, 0, 0,0,0,0, .5, 0)
    }
  }
} else if ((monthtf == FALSE)&(day == TRUE)){
  for (i in seq(1, 27*(365/clst), 1)){
    if (!(i %in% lag_df$cluster)){
      lag_df[(nrow(lag_df) + 1), ] <- list(i,0, 0,0, 0,0,0,0, .5, 0)
    }
  }
}

lag_df <- lag_df[order(lag_df$cluster),]

# Find optimal lag using cross-correlation function
if (monthtf == TRUE){ # Use if interested in monthly time scale
  cc <- ccf(unlist(lag_df[,c(events[1])]), unlist(lag_df[,c(events[2])]), lag = 36, pl = FALSE)
  lag_vals <- cc$acf[1:31]
  lags <- cc$lag[1:31]
  opt_lag <- abs(lags[which.max(lag_vals)])
} else if ((monthtf == FALSE)&(day == FALSE)){ # Use if interested in weekly time scale
  cc <- ccf(unlist(lag_df[,c(events[1])]), unlist(lag_df[,c(events[2])]), pl = FALSE)
  lag_vals <- cc$acf[21:29]
  lags <- cc$lag[21:29]
  opt_lag <- abs(lags[which.max(lag_vals)])
} else if ((monthtf == FALSE)&(day == TRUE)){ # Use if interested in daily time scale
  cc <- ccf(unlist(lag_df[,c(events[1])]), unlist(lag_df[,c(events[2])]), pl = FALSE)
  lag_vals <- cc$acf[23:37]
  lags <- cc$lag[23:37]
  opt_lag <- abs(lags[which.max(lag_vals)])
}

bn_df <- cbind(dummyvar_df, dplyr::select(data, c('cluster','Year', 'County', 'population_density', 'total_home_value', 'duration', 'damage_property_adj2022', 'proportion_property', 'prcp')))
bn_df$compounding <- 0

# This is where we check how many secondary hazards happened within X-month/weeks/days of the primary hazard
for (i in 1:nrow(bn_df)){
  if (bn_df[i, c(events[2])] == 1){
    month <- bn_df$cluster[i]
    interval <- seq(month - (opt_lag), month - 3, 1)
    for (j in 1:nrow(bn_df)){
      if ((bn_df[j, c(events[1])] == 1) & (bn_df$cluster[j] %in% interval) & (bn_df$County[i] == bn_df$County[j])){
        bn_df$compounding[i] <- bn_df$compounding[i] + 1
      }
    }
  }
}

df_copy <- bn_df
bn_df <- df_copy

bn_df <- bn_df[bn_df$cluster >= (opt_lag/2),]

if (all(bn_df$compounding == bn_df$compounding[1])){
  return(c(1, NA, NA, NA))
}

# We only look at when there is a flood, and we delete the columns with event counts
wfire_index <- which((colnames(bn_df) == c(events[1])))
flood_index <-  which((colnames(bn_df) == c(events[2])))
bn_df <- bn_df[(bn_df[, c(events[2])] == 1), -c(wfire_index, flood_index)]
bn_df <- bn_df[-3]
colnames(bn_df)[6] <- c('Damage')
colnames(bn_df)[9] <- 'Precursor'
names <- colnames(bn_df)

bn_df[(bn_df$prcp == 0) & !(is.na(bn_df$prcp)), 'prcp'] <- NA

# We standardize our data so it has mean = 0 and sd = 1. This stabilizes the MCMC algorithm, and makes the results more interpretable.
for (col in colnames(bn_df)[-c(6,9)]){
  bn_df[,col] <- as.numeric(scale(bn_df[,col]))
}

# We Box-Cox transform the damages. Makes the imputations better, since our data is approximately normal after this
x <- bn_df$Damage + .01
bn_df$DamagePositive <- x
model <- lm(as.formula('DamagePositive ~ Precursor + Year + population_density + total_home_value + duration'),
            data = bn_df, y=TRUE, qr=TRUE)
b <- boxcox(model,    
            lambda = seq(-2, 2, 1/10), 
            plotit = TRUE,  
            eps = 1/50,     
            xlab = expression(lambda), 
            ylab = "log-Likelihood")
```

![](https://github.com/samdulin/DataSciencePortfolio/blob/main/RProjects/CompoundHazards/cars-1.png)

``` r
lambda <- b$x[which.max(b$y)]
lambda
```

    ## [1] -0.06060606

``` r
bn_df$Damage <- ((bn_df$Damage + .01)^(lambda) - 1)/lambda
```

    ## [1] 0.03443619

## We perform multiple imputation

``` r
# The data has a lot of missing values, so we use multiple imputation, the most statistically robust method, in order to impute damage data. It outputs 20 datasets which we will combine into one later
  imp <- mice(bn_df[-c(7,10)], maxit = 30, meth = 'cart', m = 20)


  # I commented this out, but it checks to make sure the new imputed datasets make sense
  #densityplot(imp)
  
  # Now that we've imputed, we undo the Box-Cox transformation, since we need counts with no decimals
  bn_df$Damage <- boxinvTransform(bn_df$Damage, lambda = lambda)
  for (i in 1:20){
    imp$imp$Damage[,i] <- boxinvTransform(imp$imp$Damage[,i], lambda = lambda)
    imp$imp$Damage[,i] <- round(imp$imp$Damage[,i])
  }
  imp$data$Damage <- round(boxinvTransform(imp$data$Damage, lambda = lambda))
  
  # Now, we round the damages so we can use negative binomial regression with them
  bn_df$Damage <- round(bn_df$Damage)
```

## Finally, we run the Bayesian regression model

``` r
# Change the shape parameter to incorporate spatial uncertainty due to county size
  lambda_val <- area_x[area_index, 'Area']
  shape_param <- (7 - (lambda_val)*4)
  print(shape_param)
  
  # We have to do this for brms to take it as a variables, for some reason.
  stanvars <- stanvar(shape_param, name='shape_param')
  
  # Here is where we fit the Bayesian regression model. We use weakly informative priors. Depending on the pair, we change their mean
  # brm_multiple automatically combines the inferences from the many datasets
model <- brm_multiple(bf(Damage ~ Precursor + duration + population_density + total_home_value + prcp + Year*Precursor + Year, 
                             hu ~ Precursor + duration + population_density + total_home_value + prcp + Year*Precursor + Year),
                          data = imp,
                          family = hurdle_negbinomial(link = 'log', link_hu  = 'logit'),
                          # For wildfires we use a weakly informative prior with a positive mean
                          prior =  c(prior(student_t(shape_param, .693, 1), "b", coef = Precursor),
                                     #prior(cauchy(1.098, shape_param), "b", coef = Precursor), 
                                     prior(cauchy(0, 2.5), "b", coef = population_density),
                                     prior(cauchy(0, 2.5), "b", coef = total_home_value),
                                     prior(student_t(3, .693, 1), "b", coef = prcp),
                                     prior(cauchy(0, 2.5), "b", coef = duration), 
                                     prior(normal(10,10), "Intercept"),
                                     prior(cauchy(0,2.5), class='b', dpar='hu'),
                                     prior(normal(0,3), class='Intercept', dpar='hu'),
                                     prior(cauchy(3, 2), 'shape')), 
                          chains = 4,warmup = 4000, backend = 'rstan', iter = 5000, seed = 4635,
                          stanvars=stanvars)
```

    ## Compiling the C++ model

    ## Fitting imputed model 1

    ## Start sampling

    ## Fitting imputed model 2

    ## Start sampling

    ## Fitting imputed model 3

    ## Start sampling

    ## Fitting imputed model 4

    ## Start sampling

    ## Fitting imputed model 5

    ## Start sampling

    ## Fitting imputed model 6

    ## Start sampling

    ## Fitting imputed model 7

    ## Start sampling

    ## Fitting imputed model 8

    ## Start sampling

    ## Fitting imputed model 9

    ## Start sampling

    ## Fitting imputed model 10

    ## Start sampling

    ## Fitting imputed model 11

    ## Start sampling

    ## Fitting imputed model 12

    ## Start sampling

    ## Fitting imputed model 13

    ## Start sampling

    ## Fitting imputed model 14

    ## Start sampling

    ## Fitting imputed model 15

    ## Start sampling

    ## Fitting imputed model 16

    ## Start sampling

    ## Fitting imputed model 17

    ## Start sampling

    ## Fitting imputed model 18

    ## Start sampling

    ## Fitting imputed model 19

    ## Start sampling

    ## Fitting imputed model 20

    ## Start sampling

``` r
  # This check to see how good of a fit our model is
  print(pp_check(model) + xlim(0,1000000))
```

    ## Using 10 posterior draws for ppc type 'dens_overlay' by default.

![](compounding_hazards_demo_files/figure-markdown_github/pressure-1.png)

``` r
  # These are the conditional effects of compounding events
  print(plot(conditional_effects(model, effects = "Precursor")))
```

![](compounding_hazards_demo_files/figure-markdown_github/pressure-2.png)

    ## $Precursor

![](compounding_hazards_demo_files/figure-markdown_github/pressure-3.png)

``` r
  # Get the coefficients
  ints <- data.frame(posterior_interval(model))
  
  print(paste('The compounding effect has a 95 percent credible interval of', exp(ints[which(row.names(ints) == 'b_Precursor'), 1]), exp(ints[which(row.names(ints) == 'b_Precursor'), 2])))
```

    ## [1] "The compounding effect has a 95 percent credible interval of 1.09385383800218 1.42896309942217"

``` r
  print(paste('The coefficient of the interaction term (which tells us the multiplicative change of the compounding effect over time) has a 95 percent credible interval of', exp(fixef(model)[16,3]), exp(fixef(model)[16,4])))
```

    ## [1] "The coefficient of the interaction term (which tells us the multiplicative change of the compounding effect over time) has a 95 percent credible interval of 0.941160685941328 1.47276316573151"
