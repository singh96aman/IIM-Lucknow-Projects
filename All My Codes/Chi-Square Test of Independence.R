## Recall that in this study, scientists studied the effect of a new drug on a sample of 84 Arthritis patients.
##Each patients was given a 'Treatment' - either 'Treated' using this new drug or given a 'Placebo'.
##And in response to the 'Treatment, the extent to which each patient 'Improved' varied. Each patient showed either 'None', 'Some' or 'Marked' improvements.

##Recall that in R, the 'xtabs()' functions gives us a two-way contingency table, as follows..

##Now suppose we wish to test whether the 'Treatment' type {'Placebo','Treated'} causes the Arthritis among patients to be 'Improved' {'None'; 'Some'; 'Marked'}

##The *Pearson's chi-squared test* is a statistical test applied to sets of categorical data to evaluate how likely it is that any observed difference between the sets arose by chance. 

##In this case we can use it to tesst a null hypothesis that the proportion of patients who were 'Treated' and showed 'Marked' improvement is higher than the proportion of patients who were given a 'Placebo' and showed 'Marked' improvement.

##In R, we can apply the function 'chisq.test()' to the above two-way table in order to produce a chi-square test of independence of the row and column variables. 

## ------------------------------------------------------------------------
library(vcd)

## ----(i) Arthritis dataset-----------------------------------------------
library (vcd)
head(Arthritis)

## ----(ii) 2-Way Table----------------------------------------------------
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
addmargins(mytable)

## ----(iii) Pearson-------------------------------------------------------
chisq.test(mytable)

##This output suggests a relationship between the treatment received and level of improvement ( p < 0.01)

##The p-values are the probability of obtaining the sampled results, assuming independence of the row and 
##column variables in the population. Since, the probability is small ( p < 0.01 ), we reject the Null
##hypothesis that treatment type and outcome are independent.

##Next, consider impact of the 'Sex' of the patient {'Female', 'Male'} on how much the Arthritis 'Improved'
##Once again, we can create a 2-way contingency tables


## ----(iv) 2-way table----------------------------------------------------
library(vcd)
mytable <- xtabs(~Improved+Sex, data=Arthritis)
addmargins(mytable)

## ----(v) Pearson---------------------------------------------------------
chisq.test(mytable)

##This time, there doesn't appear to be a relationship between the 'Sex' of the patient and how much their
##Arthritis 'Improved', since we see that ' p > 0.05 '

##Since, this probability is not small ( p > 0.05 ), we fail to reject the null hypothesis that 'Treatment' and 'Sex' are independent. 
