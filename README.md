---
title: "Assignment 2"
author: "Arianna Hernandez"
format: html
editor: visual
embed-resources: true
---

## **Assignment 02 - Data Viz and Wrangling**

September 27, 2024

## **Due Date**

This assignment is due by 11:59pm Pacific Time, October 11th, 2024.

For this assignment, we will be analyzing data from USC’s Children’s Health Study. The learning objectives are to conduct data wrangling and visualize the data with key questions in mind.

```{r, echo=FALSE, output = FALSE}
library(tidyverse)
library(dplyr)
library(ggplot2)
library(leaflet)
library(viridis)
```

## **Data Wrangling**

You will need to download two datasets from <https://github.com/USCbiostats/data-science-data>. The [individual](https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_individual.csv) and [regional](https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_regional.csv) CHS datasets in `01_chs`. The individual data includes personal and health characteristics of children in 12 communities across Southern California. The regional data include air quality measurements at the community level. Once downloaded, you can merge these datasets using the location variable. Once combined, you will need to do the following:

```{r}
individual <- read.csv("chs_individual.csv")
regional <- read.csv("chs_regional.csv")
mdata <- merge(individual, regional, by = "townname", all = TRUE)
```

1.  After merging the data, make sure you don’t have any duplicates by counting the number of rows. Make sure it matches.

    In the case of missing values, impute data using the average amongst individuals with the same values for the “male” and “hispanic” variables. For categorical variables, take the mode. If you are interested (and feel adventurous) in the theme of Data Imputation, take a look at this paper on “Multiple Imputation” using the Amelia R package [here](https://gking.harvard.edu/files/gking/files/amelia_jss.pdf).

```{r}
nrow(individual)
nrow(mdata)
cdata <- mdata[!duplicated(mdata), ]
summary(cdata) ##checking which vars have NA values
str(cdata)

catVar<- c("asthma","active_asthma","father_asthma","mother_asthma","wheeze","hayfever","allergy","educ_parent","smoke", "gasstove")
nVar<- c("agepft", "height","weight", "bmi", "fev","fvc","mmef","no_24hr","pm2_5_fr")

#imputation of mean for numerical variables
for (var in nVar) {
  cdata[[var]] <- ave(
    cdata[[var]],
    cdata$male,
    cdata$hispanic,
    FUN = function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)
  )
}

#defining mode function
mode <- function(x) {
  un <- unique(x)
  un[which.max(tabulate(match(x, un)))]
}
#finding mode for categorical variables
for(var in catVar){
  cdata[[var]]<- ave(
    cdata[[var]],
    cdata$male,
    cdata$hispanic,
    FUN = function(x) ifelse(is.na(x), mode(x),x)
  )
}
summary(cdata)
  
```

2.  Create a new categorical variable named “obesity_level” using the BMI measurement (underweight BMI\<14; normal BMI 14-22; overweight BMI 22-24; obese BMI\>24). To make sure the variable is rightly coded, create a summary table that contains the minimum BMI, maximum BMI, and the total number of observations per category.

```{r}
cdata<- cdata %>%
mutate(obesity_level = if_else(
  bmi< 14,"underweight BMI",if_else(
    bmi>=14 & bmi< 22,"normal BMI", if_else(
      bmi>= 22 & bmi < 24,"overweight BMI", "obese BMI"))))

cdata$obesity_level <- factor(cdata$obesity_level, levels = c("underweight BMI", "normal BMI", "overweight BMI", "obese BMI"))

#summary table
summary<- cdata %>%
  group_by(obesity_level) %>%
  summarise(
    minBMI= min(bmi),
    maxbMI = max(bmi),
    total = n()
  )

print(summary)
```

3.  Create another categorical variable named “smoke_gas_exposure” that summarizes “Second Hand Smoke” and “Gas Stove.” The variable should have four categories in total.

```{r}
cdata<- cdata %>%
mutate(smoke_gas_exposure = if_else(
  smoke == 0 & gasstove == 0, "No Exposure", if_else(
    smoke == 1 & gasstove == 0, "Second-hand Smoke Exposure", if_else(
      smoke == 0 & gasstove == 1, "Gas Exposure", "Both Smoke and Gas Exposures")
    )
  )
)
cdata$smoke_gas_exposure <- factor(cdata$smoke_gas_exposure, levels = c("No Exposure", "Second-hand Smoke Exposure","Gas Exposure", "Both Smoke and Gas Exposures"))
levels(cdata$smoke_gas_exposure)
```

4.  Create four summary tables showing the average (or proportion, if binary) and sd of “Forced expiratory volume in 1 second (ml)” (an asthma indicator) by town, sex, obesity level, and “smoke_gas_exposure.”

```{r}
sum_town <- cdata %>%
  group_by(townname) %>%
  summarise(
    fev_avg = mean(fev),
    fev_sd = sd(fev)
  )
print(sum_town)

sum_sex <- cdata %>%
  group_by(male) %>%
  summarise(
    fev_avg = mean(fev),
    fev_sd = sd(fev)
  )
print(sum_sex)

sum_ol <- cdata %>%
  group_by(obesity_level) %>%
  summarise(
    fev_avg = mean(fev),
    fev_sd = sd(fev)
  )
print(sum_ol)
      
sum_sge <- cdata %>%
  group_by(smoke_gas_exposure) %>%
  summarise(
    fev_avg = mean(fev),
    fev_sd = sd(fev)
  )
print(sum_sge)
```

## **Looking at the Data (EDA)**

The primary questions of interest are:

1.  What is the association between BMI and FEV (forced expiratory volume)?

```{r}
ggplot(data = cdata, mapping = aes(x = bmi, y = fev)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Association of BMI and FEV", x = "BMI", y ="FEV")

summary(lm(fev~bmi, data = cdata))

ggplot(data = cdata, mapping = aes(x = obesity_level, y = fev)) +
  geom_boxplot() +
  labs(title = "Association of obesity level and FEV", x = "Obesity Levels", y ="FEV")

summary(lm(fev~obesity_level, data = cdata))
```

<span style="color: red;"> Looking at the scatter plot and linear model between BMI score and FEV, we can see that there is a positive relationship between the two. At the intercept, we can see that the FEV score is assumed to be at 1452.46 and goes up 31.23ml with every unit increase of BMI, and with a p \< 0.001, we can assume this difference is significant.

<span style="color: red;">Further looking into the different obesity levels and their effects on FEV, we can also see this overall increase in the boxplots. A regression model on the different levels shows that BMI is significantly related to FEV in all the categories. However, the R squared indicates that only about 11.36% of the variance in the data can be explained by the model, so though there is a significant relationship, we can assume BMI is not a strong predictor of FEV.

2.  What is the association between smoke and gas exposure and FEV?

```{r}

ggplot(data = cdata, mapping = aes(x = smoke_gas_exposure, y = fev)) +
  geom_boxplot() +
  labs(title = "Association of Smoke and Gas Exposure levels and FEV", x = "Smoke/Gas Levels", y ="FEV")


summary(lm(fev~smoke_gas_exposure, data = cdata))
```

<span style="color: red;">The boxplot shows very similar levels of FEV for each different smoke/gas exposure level. Looking at the regression model, the only significant relationship is FEV and no gas/smoke exposure, though the overall p score (0.52) and r squared of 0.002 indicates no overall significance.

3.  What is the association between PM2.5 exposure and FEV?

```{r}
ggplot(data = cdata, mapping = aes(x = pm25_mass , y = fev)) +
  geom_jitter(aes(color = pm25_mass), width = 0.2, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Association of PM 2.5 Exposure and FEV", x = "PM 2.5 Levels", y ="FEV")

summary(lm(fev~pm25_mass, data = cdata))
summary(lm(fev~pm25_so4 + pm25_no3 + pm25_nh4 + pm25_oc + pm25_ec + pm25_om, data = cdata))
```

<span style="color: red;">In this graph we can see that there is a slight negative relationship between PM 2.5 levels and FEV score, with the estimated intercept being at 2073 and decreasing 3.016 with each unit increase of PM 2.5. The P = 0.01 so we can assume significance, however R squared of 0.005 indicates it is not a strong predictor of FEV. I also looked at the relationship between the components and overall FEV effect and found no significant difference.

Follow the EDA checklist from week 3 and the previous assignment. Be sure to focus on the key variables.

## **Visualization**

Create the following figures and interpret them. Be sure to include easily understandable axes, titles, and legends.

1.  Facet plot showing scatterplots with regression lines of BMI vs FEV by “townname”.

```{r}
ggplot(data = cdata, aes(x = bmi, y = fev)) +
  geom_point(aes(color = obesity_level)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ townname) + 
  labs(title = "FEV vs BMI by Town",
       x = "Body Mass Index (BMI) ",
       y = "Forced Expiratory Volume (FEV)")
```

<span style="color: red;">All the towns seem to have a somewhat similar-positive relationship between BMI and FEV score. However, Atascadero, Lake Elsinore, and Upland seem to have the strongest relationship. Riverside's regression seems like it might be affected by the plotted points around 40, which seem to be outliers.

2.  Stacked histograms of FEV by BMI category and FEV by smoke/gas exposure. Use different color schemes than the ggplot default.

```{r}
cdata %>%
  ggplot(aes(x = fev, fill = obesity_level)) + 
  geom_histogram(position = "stack", color = "black") +
  scale_fill_viridis_d(option = "C") + 
  labs(title = "FEV by BMI Category",
       x ="Forced Expiratory Volume (FEV)",
       y = "Body Mass Index Category")

cdata %>%
  ggplot(aes(x = fev, fill = smoke_gas_exposure)) + 
  geom_histogram(position = "stack", color = "black") +
  scale_fill_viridis_d(option = "B") + 
  labs(title = "FEV by Smoke and Gas Exposures",
       x ="Forced Expiratory Volume (FEV)",
       y = "Smoke/Gas Exposure")
```

<span style="color: red;">The FEV by BMI category shows a normal-ish distribution of FEV scores for all the BMI categories, with the overweight and obese categories showing a longer right tail and therefore higher FEV, while the underweight category seems to have a longer left tail, indicating lower FEV Values. This graph shows the same trend we saw earlier, where higher BMI scores increase FEV slightly.

<span style="color: red;">As for the smoke and gas categories, each category also seems to have normal distributions. The least amount of children were exposed to both smoke and gas, and the lowest FEV score was seen for kids with gas exposure, while the highest FEV was seen for kids with gas exposure, and exposure to neither second-hand smoke nor gas. The highest population size can be seen with people with no exposure.

3.  Barchart of BMI by smoke/gas exposure.

```{r}
cdata %>%
  ggplot(aes(x =bmi, fill = smoke_gas_exposure)) + 
  geom_histogram(color = "black") +
  scale_fill_viridis_d(option = "G") + 
  labs(title = "Body Mass Index by Smoke and Gas Exposure ",
       x ="Body Mass Index Category",
       y = "Smoke and Gas Exposure")
```

<span style="color: red;">Here, we can see that kids with no exposure fell mainly within the range of a "normal" BMI, with some in the "overweight" and "obese" categories. The lowest BMI was seen for children with gas stoves in their home, while the highest BMI's were seen for kids with both Gas exposure and second-hand smoke, as well as just gas exposures. This graph indicates that there is a possible relationship between gas/smoke exposure and unhealthy body weight in the children of this study.

4.  Statistical summary graphs of FEV by BMI and FEV by smoke/gas exposure category.

```{r}
cdata %>%
  ggplot(aes(x = obesity_level, y = fev)) + 
    stat_summary(fun.min = min, fun.max = max, fun = median, geom = "crossbar", width = 0.4, color = "purple") +
    labs(title = "Statistical Summary of FEV by BMI Category",
         x = "BMI Category",
         y = "Forced Expiratory Volume (FEV)")

cdata %>%
  ggplot(aes(x = smoke_gas_exposure, y = fev)) + 
    stat_summary(fun.min = min, fun.max = max, fun = median, geom = "crossbar", width = 0.4, color = 'blue') +
    labs(title = "Statistical Summary of FEV by Smoke/Gas Exposure Category",
         x = "Smoke/Gas Exposure Category",
         y = "Forced Expiratory Volume (FEV)")
```

<span style="color: red;">Here, we can see that the children within the "underweight" BMI category had the lowest median FEV, while children within the "overweight" and "obese" categories had the highest median FEV scores. Though kids within the "normal" BMI category had the highest variance in FEV, their median sits in between all the other categories--indicating a relationship between FEV and BMI.

<span style="color: red;">Additionally, the smoke/ gas graph shows that the medians between all four categories are somewhat the same. Second-hand smoke exposure is slightly higher than all the others, but not by any significant amount. However, as we had seen previously, we can see the highest variance in values in both the "no exposure" category, as well as the "gas exposure" category. From this graph it is worth further investigating the effects of gas exposure and FEV values.

5.  A leaflet map showing the concentrations of PM2.5 mass in each of the CHS communities.

```{r}
mass.pal <- colorNumeric(c('goldenrod','darkorange','brown'), domain=cdata$pm25_mass)


leaflet(cdata) %>%
  addProviderTiles('CartoDB.Positron') %>%  
  addCircles(
    lat = ~lat, 
    lng= ~lon, 
    radius = ~pm25_mass*300,  # Adjust radius based on PM2.5 concentration
    color = ~ mass.pal(pm25_mass),      # Color of the circles
    stroke = FALSE,
    fillOpacity = 0.5,
    popup = ~paste(townname, "<br>", "PM2.5 Mass:", pm25_mass, "µg/m³")  # Popup text
  ) %>%
  addLegend('bottomleft', pal=mass.pal, values=cdata$pm25_mass,
          title='PM 2.5 Mass, µg/m³ ', opacity=1)
```

<span style="color: red;">Highest concentration of PM 2.5 mass is seen in Mira Loma, at 29.97 µg/m³, while Lompoc seems to have the lowest concentration at 5.96 µg/m³. Graph indicates highest PM 2.5 levels can be found nearest to Los Angeles, while the most northern/southern areas seem to have the lowest PM 2.5 Concentrations.

6.  Choose a visualization to examine whether PM2.5 mass is associated with FEV.

```{r}
cdata<- cdata %>%
  mutate(pm25_cat = cut(pm25_mass, breaks = quantile(pm25_mass),
                        labels = c("0%-25%", "25% - 50%", "50% - 75%", "75% - 100%"),
                        include.lowest = TRUE))

ggplot(cdata, aes(x = pm25_cat, y = fev)) + 
  geom_boxplot(fill = "lavenderblush") +  
  geom_jitter(aes(color = pm25_mass), width = 0.2, alpha = 0.5) + 
  geom_smooth(method = "lm", aes(group = 1), color = "gold4", se = FALSE) +
  scale_color_viridis(option = "A") +
  labs(title = "Relationship between FEV and PM2.5 Mass",
       x = "PM2.5 Mass (µg/m³)",
       y = "Forced Expiratory Volume (FEV)") 
```

<span style="color: red;">As seen in the previous scatter plot, there seems to be a slightly negative relationship between FEV and PM 2.5 mass, however it doesn't seem very significant. Variance seems about the same for all the categories, and the medians vary slightly, but not enough to cause concern. From this graph, I would conclude there is no relationship between the two.
