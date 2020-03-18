# covid-19
Non-linear logistic regression models for the Covid-19 outbreak

Data source is the [Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19) repo.

Just two snippets to plot models and prediction on the covid-19 outbreak data

These are definetly far from perfect and I would not trust myself with the results so please use with caution. This is code that is meant to be used to learn about logistic regression.

basic reference to non linear logistic models in R can be found at [datascienceplus](https://datascienceplus.com/first-steps-with-non-linear-regression-in-r/) and at [bscheng - modeling-logistic-growth-data](https://bscheng.com/2014/05/07/modeling-logistic-growth-data-in-r/).

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/Rplot06.png)
* Italy's growth factor *G* which can be expressed as: 

![alt text](http://www.sciweavers.org/upload/Tex2Img_1584561993/render.png)

e.g the number of new cases today divided by the number of new cases yesterday. Between 15th of march and the 18th this growth factor has beeb around ~1: This *may* indicate that Italy has approached the inflection point after ~ 23 days of near exponential growth ( G ~ 1.3).



![alt text](https://github.com/artoo-git/covid-19/blob/master/images/ITmodel.png)

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/ITplateau.png)

![alt text](https://github.com/artoo-git/covid-19/blob/master/images/ITmodelD.png)


![alt text](https://github.com/artoo-git/covid-19/blob/master/images/FRmodel.png)

