# Data Science Portfolio for Sam Dulin

This Portfolio is a sample of projects I have done for data analysis and exploring machine learning algorithms. They are a mixture of Python, R, and MATLAB. 

## Projects in Python

### Machine Learning for Forest Fire Prediction
[GitHub](/PythonProjects/forest_fires.ipynb)
[nbviewer](https://nbviewer.org/github/samdulin/DataSciencePortfolio/blob/main/PythonProjects/forest_fires.ipynb)

This dataset is 519 observations of forest fires in Portugal. We compare deep learning, random forest regression, and support vector regression models at predicting the x and y spatial coordinates of the forest fires. All three of the models had a significantly smaller mean abosulte error than the SVM model used by Cortez and Morais in their original paper on this dataset from 2007 (https://core.ac.uk/download/pdf/55609027.pdf).

## Projects in R

### Filling in Missing Values Using Classifiers
[GitHub](/RProjects/missingValues.md)

One common problem in biological datasets is that of having missing values. In particular, humans have a lot of trouble determing bird sex. So, I tasked myself with creating a classifier and using feature extraction to be able to predict bird sex. The algorithm outperformed random assignement by a factor greater than 10 in most cases.

### Statistical Study of Infant Mortality Death in NYC
[GitHub](/RProjects/InfantMortalityStudy.md)

This dataset is infant mortality death collected for the years 2007-2016 in New York City. I analyze the relationship between race and infant mortality rate using various statistical visaulizations. I also explore the relationship between the type of infant death and race using a chi-square test.

## Projects in MATLAB

### Optimal Transport

In packages like SciPy, the Wasserstein distance can be measured in the 1D case. But what if we want to measure it in 2 dimensions? The algorithm used here to do so is based on the work done by Cui, Dieci, and Zhao in their paper: https://arxiv.org/pdf/2105.09502.pdf. This code was a joint effort with Scott McIntyre.

In particular, the algorithm computes the 'optimal transport'. The question can be phrased as follows: if we have a pile of sand, what is the most efficient way to move that sand so as to make a sand castle? The 'optimal transport' is the answer to that problem. Optimal transport has a rich history, and is finding increasingly more applications in machine learning.

