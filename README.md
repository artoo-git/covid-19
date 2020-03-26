# covid-19
Non-linear logistic regression models for the Covid-19 outbreak

Data source is the [Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19) repo.

Just two snippets to plot models and prediction on the covid-19 outbreak data

These are definitely far from perfect and I would not use these for any official report so please use with caution. This code was written as hobby and as an opportunity to learn more about non-linear logistic regression.

basic reference to non linear logistic models in R can be found at [datascienceplus](https://datascienceplus.com/first-steps-with-non-linear-regression-in-r/) and at [bscheng - modeling-logistic-growth-data](https://bscheng.com/2014/05/07/modeling-logistic-growth-data-in-r/).

## Descriptive statistics across countries. Total count and daily increase


![alt text](https://github.com/artoo-git/covid-19/blob/master/images/Rplot06.png)

As I write (26 of March) the daily growth of italy has been subexpoential for a few days. This *may* indicate that Italy is approaching the inflection point. 

Below is the daily variation by country (new positive tests in one day). Again, *It seems* that the lockdown measures taken in Italy have started to see the first effects on the daily increase **but only 12 days after their implementation by the Italian Government.**

Looking at the changes for Italy both plots, one could speculate that the timings of the virus (incubation time, duration of disease) are so that any counter-measure taken by a goverment today will have a delayed effect of roughly two weeks (possibly less).

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/daycount.png)


## Extrapolations from the logistic model

In this logistic curve below (Projectio at 5 days), the growth factor *G* can be expressed as: 

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/render.png)

e.g the number of new cases today divided by the number of new cases yesterday. When this value is greater than 1 every day we have an xponential growth.  For comparison, the value of the ration observed across countries has been around ~ 1.3. This similar growth is evident observing the similarity of the slopes plotted in the total count graph (first plot).

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/ITmodel.png)

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/FRmodel.png)

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/UKmodel.png)


