# covid-19
Non-linear logistic regression models for the Covid-19 outbreak

Data source is the [Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19) repo.

Just two snippets to plot models and prediction on the covid-19 outbreak data

These are definitely far from perfect and I would not use these for any official report so please use with caution. This code was written as hobby and as an opportunity to learn more about non-linear logistic regression.

basic reference to non linear logistic models in R can be found at [datascienceplus](https://datascienceplus.com/first-steps-with-non-linear-regression-in-r/) and at [bscheng - modeling-logistic-growth-data](https://bscheng.com/2014/05/07/modeling-logistic-growth-data-in-r/).

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/Rplot06.png)

\* In this logistic curve, the growth factor *G* can be expressed as: 

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/render.png)

e.g the number of new cases today divided by the number of new cases yesterday. Between 15th of March and the 18th this growth factor has been around ~1: This *may* indicate that Italy is approaching the inflection point in the next few days after an intial 23 days of exponential growth ( G ~ 1.3).



![alt text](https://github.com/artoo-git/covid-19/blob/master/images/ITmodel.png)

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/ITplateau.png)

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/ITmodelD.png)


![alt text](https://github.com/artoo-git/covid-19/blob/master/images/FRmodel.png)

