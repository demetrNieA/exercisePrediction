---
title: "Prediction of how well exercise is performed"
author: "demetr"
date: "July 17, 2016"
output: html_document
---

# Synopsis

In this report we analyze "Weight Lifting Exercises Dataset" dataset in order to form a prediction model which will be able to classify exercise to how well it was performed based on collected metrics.

# Reproducability


# Data set

```{r , echo = FALSE, message = F}
library(dplyr)
library(ggplot2)
library(xtable)
library(knitr)
library(pander)

panderOptions("digits", 2)
x <- Sys.setlocale("LC_TIME", "C")
```

```{r , cache = TRUE, echo = FALSE}
fuelCons <- tbl_df(read.csv("https://raw.github.com/amercader/car-fuel-and-emissions/master/data.csv"))
```

We decide to use "Car fuel consumptions and emissions 2000-2013" dataset, as it has most current and applicable data. All data can be found on data.okfn.org web site: http://data.okfn.org/data/amercader/car-fuel-and-emissions

This is big dataset with information regarding different characteristics: 

```{r , echo = FALSE}
dim(fuelCons)
```

## Data processing

We decide to remove from dataset entries with combined MPG > 100, as they are too extreme and can affect overall model. See fig 1 for details. Also we removed entries with missing transmission type.

```{r , echo = FALSE}
histMPG <- qplot(combined_imperial,
                 data = fuelCons, 
                 binwidth = 3, 
                 xlab = "Average MPG of urban and extra urban tests", 
                 ylab = "Count of cars")
fuelCons <- fuelCons %>%
  filter(combined_imperial < 100, transmission_type != "") %>%
  mutate(transmission_typeX = as.factor(ifelse(transmission_type == 'Manual', 'Manual', 'Automatic'))) %>%
  mutate(particulates_emissions = as.numeric(as.character(particulates_emissions)))
```

# Regression Models

We use information about average MPG of urban and extra urban tests as target outcome and trying to explain it by several model.

* First of all we believe that engine capacity and fuel type should have effect on MPG as well as transmission type. See figure 2 for exploratory plot.
* Also engine efficiency may be biased by year and emission level.

```{r}
fit1 <- lm(urban_imperial ~ transmission_type + engine_capacity + fuel_type,
            data = fuelCons, na.action = na.exclude)
fit2 <- lm(urban_imperial ~ transmission_type + engine_capacity + fuel_type +
              particulates_emissions + co_emissions + thc_emissions + year,
            data = fuelCons, na.action = na.exclude)
```

Unfortunately we can't use formal methods to compare this models (`anova`) as it has missing values, so we look at the coefficients significance level (see in appendix). According to the significance we should add year to our model. So resulted model is

```{r}
fit3 <- lm(urban_imperial ~ transmission_type + engine_capacity + fuel_type + year,
            data = fuelCons, na.action = na.exclude)
```

We plotted resulting residuals (see figure 3) and it seems model can be improved, we should collect more variables to explain rest of the residuals. According to normality of the residuals (see figure 4) model appears to be correct and applicable.

## Conclusion

Based on chosen model manual transmission appears to be more effective (given same fuel type, engine capacity and year of creation). With 95% confidence using manual transmission will increase MPG by 2.1 with confidence interval [2, 2.2]. For all resulted confidence intervals please refer appendix.

\pagebreak






# Appendix

## Figure 1: Histogram of overall MPG for all vecicles (unmodified dataset)
```{r, fig.width=8, fig.height=4, echo = FALSE, warning = FALSE}
histMPG
```

## Figure 2: Relations between engine capacity, MPG and other characteristics
```{r, fig.width=8, fig.height=4, echo = FALSE, warning = FALSE}
qplot(urban_imperial,
      engine_capacity, 
      data = fuelCons, 
      col = fuel_type, 
      facets = transmission_type ~ ., 
      xlab = "MPG", 
      ylab = "Engine capacity, cc") +
  guides(colour = guide_legend(title = "Fuel type"))
```

\pagebreak

## Models summary

### Model 1 (engine capacity, fuel type, transmission type)
```{r , echo = FALSE}
pander(fit1)
```

### Model 2 (engine capacity, fuel type, transmission type, all types of emissions, year)
```{r , echo = FALSE}
pander(fit2)
```

### Model 3 (engine capacity, fuel type, transmission type, year)
```{r , echo = FALSE}
pander(fit3)
```

## Figure 3 - Residuals vs fitted values
```{r ,fig.width=8, fig.height=4, echo = FALSE, warning = FALSE}
qplot(fit3$fitted.values, fit3$residuals, geom = c("point", "smooth"),
      xlab = "Fitted values, MPG",
      ylab = "Residuals, MPG")
```

## Figure 4 - Normality of residuals
```{r ,fig.width=8, fig.height=4, echo = FALSE, warning = FALSE, message = FALSE}
qplot(fit3$residuals, ..density.., xlab = "Residuals, MPG", geom = c("histogram")) + geom_density(fill = "green", col = "red", alpha = 0.4)
```

\pagebreak





## Resulted confidence intervals summary
```{r}
pander(cbind(Coefficient = coef(fit3), confint(fit3)))
```