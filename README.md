# covid-19
Non-linear logistic regression models for the Covid-19 outbreak

Data source is the [Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19) repo.

Just two snippets to model the covid-19 outbreak data (see in the models folder)

These are definitely far from perfect and I would not use these for any official report so please use with caution. This code was written as hobby and as an opportunity to learn more about non-linear logistic regression.


## Descriptive statistics across countries. Total count and daily increase


Detected cases by country  |  Death-count by coutry
:-------------------------:|:-------------------------:
![alt text](https://github.com/artoo-git/covid-19/blob/master/images/Rplot6.png)  | ![alt text](https://github.com/artoo-git/covid-19/blob/master/images/Rplot6_D.png)





Looking at the changes in the rate for death and new cases (both plots), one could speculate that the timings of the virus (incubation time, duration of disease) are so that any counter-measure taken by a goverment today will have a delayed effect of roughly two weeks (possibly less).



Daily detected cases by country  |  Daily death-count by coutry
:-------------------------------:|:----------------------------------:
![alt text](https://github.com/artoo-git/covid-19/blob/master/images/daycount.png) | ![alt text](https://github.com/artoo-git/covid-19/blob/master/images/daycount_D.png)


## Extrapolations from the logistic model 

[being reworked in a SIRD model] work in progress - contribute if you like



