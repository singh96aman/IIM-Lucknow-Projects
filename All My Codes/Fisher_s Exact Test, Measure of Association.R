## ------------------------------------------------------------------------
library(vcd)

##This is an alternate to the Pearson Chi-Squared Test

##Fisher's Exact test is a way to test the association between two categorical variables when you have small cell sizes 
##(expected values less than 5). Chi-square test is used when the cell sizes are expected
##to be large. If your sample size is small (or you have expected cell sizes < 5 ), you
##should use Fisher's Exact test. Otherwise, the two tests will gie relatively the same answers. 

## ----(vi) Fisher---------------------------------------------------------
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)

#Measures of Association for a two-way table
#The significance tests (Pearson's Chi-Squared, Fisher's Exact Test) evaluate whether sufficent evidence
#exits to reject a null hypothesis of independence between variables.
#Suppose, we have rejected the null hypothesis of independence.
#In statistics, the **Cramer's V** and the **Phi Coefficient** are two well-known ways for this purpose. 
#They are used as a **post-test** to determine **strengths of association** after the Pearson's Chi-Squared test has determined significance ( p < 0.01 )
#One limitation of the Phi Coefficient is that it can only be used to measure association betweenn two binary variables (e.g. living/dead, black/white, success/failure)

# In R, the 'assocstats()' function in package 'vcd' can be used to measure the association as follows.


## ----(vii) Measure of Association----------------------------------------
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
addmargins(mytable)
assocstats(mytable)

#In general, larger magnitudes of Cramer's V indicate associations.

