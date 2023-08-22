Infant Mortality Study
================
Sam Dulin

## Dataset Description

### Dataset Explanation

The original dataset describes the infant mortality rates (less than 1
year in age) based upon race/ethnicity for the years 2007-2016 in New
York City. The columns of the data which are needed to understand our
analyses below are year, maternal race or ethnicity, infant mortality
rate, infant deaths, and finally the total number of live births. The
year is just the recorded year of death for the infant. Maternal race or
ethnicity is either the race or ethnicity of the mother of the dead
infant. Throughout this paper I will refer to this column as the
race/ethnicity of the infant, rather than saying the race/ethnicity of
the mother of the infant, just for the sake of brevity. Infant deaths
are the number of infant deaths in a given year for a specific
race/ethnicity. So, an example of a column entry for infant deaths would
be that 24 Puerto Rican children died in 2016 in New York City. The
total number of live births is the number of children born for that
race/ethnicity within a given year. For example, 7,159 Puerto Ricans
were born in New York City in 2016. Now, the infant mortality rate is
the number of infants per 1,000 births who die in a given year for a
given race/ethnicity. So for example, for every 1,000 births of Puerto
Rican children in 2016, we would expect 3.4 to die. Let me describe
briefly how this is calculated in the context of the 2016 Puerto Rican
example I’ve been using: you would divide 7,159 by 1,000 to get 7.159.
Then you would divide 24 by 7.159 to get the infant mortality rate,
which is 3.4 once you round. Combining all this information we see that
a row of the data will give us the infant mortality rate, infant deaths,
and live births for a specific race/ethinicity within a given year.

### Dataset Characteristics and Issues

Now, this is population data which was collected by the Department of
Health and Mental Hygiene (DOHMH) by looking at NYC death certificates
for infant deaths, and the count of live births was determined by
looking at NYC birth certificates (#reference 1). One issue with this
data is that numerous babies are born and never get a birth certificate.
Presumably, most children who do not have a birth certificate are not
born in a hospital, and therefore are probably more likely to die
neonatal due to complications while giving birth. Therefore, these kids
are not a part of the data set, and this is an issue with the
collection. Another issue with the data collection that could use
improvement is that only the race/ethnicity of the mother is included,
but it is seemingly of equal importance in regards to the child’s
survival what the father’s ethnicity is as well. Also, some minor issues
I had with the original data, first, was that it had “Non-Hispanic
Black” and “Black Non-Hispanic”. This obviously denotes the same group
of people, so I combined them under the same name of “Non-Hispanic
Black”. The same was the case for “White Non-Hispanic” and “Non-Hispanic
White”. Second, the data didn’t have any information about infant
mortality for the “Other/Two or More” race/ethinicity, so I subsetted
the data and removed this category. This edited data set is named
infant, as seen below.

## Numerical Representation

``` r
infant <- orig[which(orig$Materal.Race.or.Ethnicity != "Other/Two or More"),]
infant$Materal.Race.or.Ethnicity[infant$Materal.Race.or.Ethnicity == 
                                   "Black Non-Hispanic"] <- "Non-Hispanic Black"
infant$Materal.Race.or.Ethnicity[infant$Materal.Race.or.Ethnicity == 
                                   "White Non-Hispanic"] <- "Non-Hispanic White"

ggplot(infant, aes(x = Materal.Race.or.Ethnicity, 
                   y = as.numeric(Infant.Mortality.Rate), 
                   fill = Materal.Race.or.Ethnicity))  + 
  stat_summary(fun.y = "mean", geom = "bar") + 
  stat_summary(fun.y= "mean" , geom="text", aes(label=..y..), vjust=1.5, 
               position=position_dodge(.9), color="white") +
  labs(title="Mean Infant Mortality Rate by Race/Ethnicity",
                     x="Race/Ethnicity", y="Infant Mortality Rate")+
  scale_fill_discrete(name = "Race/Ethnicity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) 
```

![](InfantMortalityStudy_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#reference 1, 2, 5, and 6
```

The conclusion which we can draw from this is that the average infant
mortality rate was significantly higher for Non-Hispanic Blacks and and
Puerto Ricans in New York City from 2007-2016. Also, we can conclude
that Non-Hispanic Whites and Asian/Pacific Islanders have the lowest
average infant mortality rate in NYC for 2007-2016. A possible
explanation for this trend is that Non-Hispanic Blacks and Puerto Ricans
may not have as good of access to healthcare as Non-Hispanic Whites and
Asian/Pacific Islanders. Also perhaps the healthcare the Non-Hispanic
Blacks and Puerto Ricans have access to is not as high in quality as the
healthcare to which Non-Hispanic Whites and Asian/Pacific Islanders have
access in NYC.

## Graphical Representation

### Graphic \#1

``` r
ggplot(infant, aes(x = as.numeric(Year), y = as.numeric(Infant.Mortality.Rate), 
                   fill = as.factor(Year))) +
  geom_boxplot()+
  scale_x_continuous(breaks = seq(2007,2016, by = 1))+ 
  scale_y_continuous(breaks = seq(0,15, by = .5))+
  labs(title="Boxplot of Infant Mortality Rate by Year",
                     x="Year", y="Infant Mortality Rate")+
  scale_fill_discrete(name = "Year")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](InfantMortalityStudy_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#reference 1, 2, 5, and 6
```

From this graphical representation we can conclude that over the 9 years
from 2007-2016, there has been a slow decline in the mean infant
mortality rate in NYC. Another trend we see is that the range of the
infant mortality rate has decreased just a bit, generally speaking, from
2007 to 2016. A possible explanation for the decreasing average infant
mortality rate is the various improvements that have been made in
medical technologies over the course of 2007 to 2016.

A possible explanation for the range of the infant mortality rate
decreasing somewhat is that the inequalities between healthcare access
for different demographics is gradually going away. One reason why this
may be the case is the Affordable Care Act being introduced over this
time span, which required healthcare coverage for dependents of those
who were recipients (#reference 3), and which provided coverage to
people who may not have had it in the past.

### Graphic \#2

``` r
ggplot(infant, aes(x = as.numeric(Year), y = as.numeric(Infant.Mortality.Rate), 
                   color = Materal.Race.or.Ethnicity)) + 
  geom_line() + geom_point() +
  scale_y_continuous(breaks = seq(0,11, by = 1)) +
  scale_x_continuous(breaks = seq(2007,2016, by = 1)) +
  labs(title="Infant Mortality Rate by Year and Race/ethnicity",
                     x="Year", y="Infant Mortality Rate")+
  scale_color_discrete(name = "Race/ethnicity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](InfantMortalityStudy_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#reference 1, 2, 5, and 6
```

From this graphic, we can conclude that the infant mortality rate has
been dropping the most for Non-Hispanic Blacks and Puerto Ricans in NYC
from the years 2007-2016. We also can conclude that the infant mortality
rates have stayed approximately the same from 2007-2016 for Non-Hispanic
Whites, Asian and Pacific Islanders, and Other Hispanics. One
explanation for the drop we see in infant mortality rates for Puerto
Ricans in 2014, is the Health Department in NYC created the Center for
Health Equity, which sought to reduce healthcare inequities among
various demographics (#reference 4). Another explanation to describe
some of the dropping infant mortality rate once again is the passing of
the Affordable Care Act, which helped provide healthcare to families who
previously did not have coverage.

## Question Regarding Types of Infant Deaths

Infant mortality is often broken up into types of infant deaths:
neonatal deaths(28 days and less) and postneonatal deaths(from 28 to 364
days). Most neonatal deaths are a result of things like infections and
premature birth(#reference 7). Meanwhile, most postneonatal deaths are
the result of sudden infant death syndrome and accidental
injuries(#reference 7). Thus, inequities in the neonatal and
postneonatal deaths amongst races may suggest possible inequities in
healthcare access/quality, sanitation, proper nutrition, and related
items. Focus on these sorts of inequities is important to provide a
foundation and incentive for projects, such as the establishment in 2014
of a Center for Health Equity in NYC (#reference 8). Therefore, we pose
the following question: Is there an association between race and type of
infant death (i.e. neonatal and postneonatal) in NYC from 2000 to 2019?

## Data explanation

This is population data which was collected by the Department of Health
and Mental Hygiene (DOHMH) by reviewing NYC death certificates for
infant deaths, and the count of live births was determined by reviewing
NYC birth certificates from 2007 to 2016 (#reference 9). The data set
records the infant mortality rates (less than 1 year in age) based upon
race/ethnicity for the years 2007-2016 in New York City. The category
“Other/Two or More” was removed because it did not contain
neonatal/postneonatal data. Also since “Non-Hispanic White” and “White
Non-Hispanic” both occur in the data, but are the same thing, so we gave
them the same name. This was also done for “Non-Hispanic Black” and
“Black Non-Hispanic” so that they fall under the same name. The columns
of the data which are needed to understand our analyses below are year,
maternal race or ethnicity for the baby, neonatal deaths, and
postneonatal deaths. Thus, one possible row of our data set shows, among
other things, that in 2007, 120 Non-White Hispanic babies died neonatal
and 72 postneonatal in NYC. This data is appropriate because it contains
a significant portion of the time period we are interested in, and so
our data is a good representative of that larger time period. Also, it
is appropriate because it contains all the neonatal and postneonatal
deaths amongst various races in NYC, and these were collected by a
reliable source.

## Generalization

Our sample was collected in New York City from 2007-2016. We can
generalize these results to be to be true for the 21st century so far,
but we leave out 2020 because of the likelihood that COVID-19
significantly changed both infant birth and infant death patterns in NYC
during that period. That is, we generalize to the additional 10 years of
2000-2019 in NYC. While we don’t have enough information to identify
exact causes of any potential disparities between the races in regards
to type of infant death, we make the assumption that changes (e.g.,
innovations and relative availability) in healthcare since 2016 have
been insignificant with respect to effects on health inequality in NYC.
Also, any association we observe between race and type of infant death
likely existed from 2000-2007 because, once again, we have no evidence
of healthcare system changes during those years in NYC that would have
made a significant difference. Thus, it is appropriate to generalize
results to 2000-2019.

## Chosen Test

We have chosen to conduct a chi-squared for two-way tables. This test is
appropriate because we have the category of types of infant death, and
then the category of race, and we want to know if there is a
relationship between the two. The null hypothesis is that there is no
association between race and type of infant death, and our alternative
is that there is an association. We can then use this test to determine
if there is an association. For the cells of the table we have summed up
the total number of postneonatal or neonatal deaths over all years of
the data (2007-2016) for each race. Furthermore, our two way table,
which is listed below, satisfies the assumptions that no cells contain
zero individuals and no more than 20% of the cells can expect to see
fewer than 5.

``` r
group1 <- infant[which(infant$Materal.Race.or.Ethnicity == "Non-Hispanic Black"),]
group2 <- infant[which(infant$Materal.Race.or.Ethnicity == "Non-Hispanic White"),]
group3 <-infant[which(infant$Materal.Race.or.Ethnicity == 
                        "Asian and Pacific Islander"),]
group4 <- infant[which(infant$Materal.Race.or.Ethnicity == "Puerto Rican" |
                         infant$Materal.Race.or.Ethnicity == "Other Hispanic"),]
num1 <- sum(group1$Neonatal.Infant.Deaths)
num2 <- sum(group1$Postneonatal.Infant.Deaths)
num3 <- sum(group2$Neonatal.Infant.Deaths)
num4 <- sum(group2$Postneonatal.Infant.Deaths)
num5 <- sum(group3$Neonatal.Infant.Deaths)
num6 <- sum(group3$Postneonatal.Infant.Deaths)
num7 <- sum(group4$Neonatal.Infant.Deaths)
num8 <- sum(group4$Postneonatal.Infant.Deaths)
M <- matrix(c(num1, num2, num3, num4, num5, num6, num7, num8), nrow = 2, ncol = 4)
dimnames(M) <- list("Type of Infant Death"=c("Neonatal", "Postneonatal"),
                    Race=c("Black","White", "Asian/Pacific Islander", "Hispanic"))
M
```

    ##                     Race
    ## Type of Infant Death Black White Asian/Pacific Islander Hispanic
    ##         Neonatal      1392   796                    402     1181
    ##         Postneonatal   833   380                    186      621

## Test and Conclusion

``` r
test1 <- chisq.test(M, correct=FALSE)$p.value
test1
```

    ## [1] 0.005354739

We see from the p-value above that we reject our null hypothesis, and so
we can conclude that there is a relationship between race and type of
infant death in NYC. In other words, we conclude that from 2000-2019 in
NYC there has been a relationship between race and the type of infant
death (i.e. neonatal and postneonatal). As was mentioned earlier, this
association of race and type of infant death could potentially be the
result of many factors. Those factors include differences in healthcare
access and quality, sanitation, proper nutrition, and income among
different races in NYC. So while NYC took a major step in 2014 with
establishment of the Health Equity Center, there is still much more to
be done with respect to research and delivery of new health programs.

## References

1.  <https://data.cityofnewyork.us/Health/Infant-Mortality/fcau-jc6k>.
2.  <https://catalog.data.gov/dataset/infant-mortality>
3.  <https://www.dol.gov/agencies/ebsa/about-ebsa/our-activities/resource-center/faqs/young-adult-and-aca#:~:text=to%20age%2026%3F-,The%20Affordable%20Care%20Act%20requires%20plans%20and%20issuers%20that%20offer,and%20to%20all%20employer%20plans.>.
4.  <https://www1.nyc.gov/site/doh/about/press/pr2016/pr018-16.page>.
5.  <https://stackoverflow.com/questions/14622421/how-to-change-legend-title-in-ggplot>.
6.  <https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2>
7.  <https://www.who.int/news-room/fact-sheets/detail/newborns-reducing-mortality#:~:text=first%2024%20hours.-,Preterm%20birth%2C%20intrapartum%2Drelated%20complications%20(birth%20asphyxia%20or%20lack,diarrhoea%2C%20birth%20defects%20and%20malaria.>
8.  <https://www.cdc.gov/nchs/products/databriefs/db326.htm#:~:text=Mortality%20rates%20for%20the%20five%20leading%20causes%20of%20postneonatal%20death,system%2C%20and%206.4%20for%20homicide.>
9.  <https://data.cityofnewyork.us/Health/Infant-Mortality/fcau-jc6k>
