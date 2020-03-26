# covid-19
Non-linear logistic regression models for the Covid-19 outbreak

Data source is the [Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19) repo.

Just two snippets to plot models and prediction on the covid-19 outbreak data

These are definitely far from perfect and I would not use these for any official report so please use with caution. This code was written as hobby and as an opportunity to learn more about non-linear logistic regression.

basic reference to non linear logistic models in R can be found at [datascienceplus](https://datascienceplus.com/first-steps-with-non-linear-regression-in-r/) and at [bscheng - modeling-logistic-growth-data](https://bscheng.com/2014/05/07/modeling-logistic-growth-data-in-r/).

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/Rplot06.png)

And here is the plot for the daily variation

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/daycount.png)


In this logistic curve below (Projectio at 5 days), the growth factor *G* can be expressed as: 

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/render.png)

e.g the number of new cases today divided by the number of new cases yesterday. Between 15th of March and the 21st this growth factor has been decreasing on average: This *may* indicate that Italy is approaching the inflection point. Because of the timings (incubation time, duration of disease) every measure will have a delayed effect of roughly two weeks. For comparison, the canonical exponential growth of covid is G ~ 1.3.

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/ITmodel.png)

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/FRmodel.png)

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/UKmodel.png)


