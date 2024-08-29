# Project Portfolio for Sam Dulin

This Portfolio is a small sample of research and coding projects I have worked on. They are a mixture of Python, R, and MATLAB. The demo for compounding weather hazards is work I have done while at USACE (paper forthcoming), the Optimal Transport project was part of a research project I completed at Georgia Tech, and the rest of the projects in here are a hodge-podge of recreational projects I thought were interesting. 

## Projects in Python

### Machine Learning for Forest Fire Prediction
[GitHub](/PythonProjects/forest_fires.ipynb)
[nbviewer](https://nbviewer.org/github/samdulin/DataSciencePortfolio/blob/main/PythonProjects/forest_fires.ipynb)

This dataset is 519 observations of forest fires in Portugal. We compare deep learning, random forest regression, and support vector regression models at predicting the x and y spatial coordinates of the forest fires. All three of the models had a significantly smaller mean abosolute error than the SVM model used by Cortez and Morais in their original paper on this dataset from 2007 (https://core.ac.uk/download/pdf/55609027.pdf).

## Projects in R

### Quantifying Compounding Effects on Infrastructure Damage Demo
[GitHub](/RProjects/CompoundHazards/compounding_hazards_tutorial.md)

Compounding effects from sequences of natural hazards often amplify the magnitude and complexity of disruptions, posing profound challenges to infrastructure. Despite ongoing theoretical discussions and literature exploring the ramifications of such events, there is a pressing need to comprehensively quantify the associated risks. The process to quantify a natural hazard pairs has three steps: basic data cleaning, multiple imputation, and then a Bayesian regression model. This demo is intended to give an overview of the project by walking through a single example of the model for quantifying the impact of wildfires on floods in southern California. This work was a collaboration with M. Smith, B. Ellinport, B. Trump, J. Keenan, and I. Linkov.

### Filling in Missing Values Using Classifiers
[GitHub](/RProjects/missingValues/missingValues.md)

One common problem in biological datasets is that of having missing values. In particular, humans have a lot of trouble determing bird sex. There are a variety of statistical methods to deal with this, which I have written about in my [blog](https://samdulin.wordpress.com/2024/05/30/missing-data-the-good-the-bad-and-the-ugly/). In the case where we wanted one specific value, however, I tasked myself with creating a classifier and using feature extraction to be able to predict bird sex. The algorithm outperformed random assignement by a factor greater than 10 in most cases.

### Statistical Study of Infant Mortality Death in NYC
[GitHub](/RProjects/InfantMortalityStudy.md)

This dataset is infant mortality death collected for the years 2007-2016 in New York City. I analyze the relationship between race and infant mortality rate using various statistical visaulizations. I also explore the relationship between the type of infant death and race using a chi-square test.

## Projects in MATLAB

### Optimal Transport
[GitHub](/MATLABProjects/BVP_OT_multi.m)

In packages like SciPy, the Wasserstein distance can be measured in the 1D case. But what if we want to measure it in 2 dimensions? The algorithm used here to do so is based on the work done by Cui, Dieci, and Zhao in their paper: https://arxiv.org/pdf/2105.09502.pdf. This code was a joint effort with Scott McIntyre.

In particular, the algorithm computes the 'optimal transport'. The question can be phrased as follows: if we have a pile of sand, what is the most efficient way to move that sand so as to make a sand castle? The 'optimal transport' is the answer to that problem. Optimal transport has a rich history, and is finding increasingly more applications in machine learning, such as image processing and optimization.

