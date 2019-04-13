# 2018-SSC-Case-Study
https://ssc.ca/en/case-study/case-study-2-what-predicts-popularity-ted-talks

There are four objectives for this Case Study:
1.	Define TED Talk popularity
2.	Determine features of TED Talks that predict popularity.
3.	Assess if features of TED Talk popularity change with time.
4.	Assess if features of TED Talk popularity change with theme. 

Objective 1: Three variables were used to create a popularity score: views, comments, and ratings. Our team was interested in creating a vector for the popularity score; therefore, we were interested in compressing our 16-dimenstional data. I performed cluster analysis on the popularity variables. Examples of cluster analyses were principal component analysis, k-means clustering, and hierarchical clustering. After different cluster analyses results were compared, I developed a context-based score for different clusters in the popularity score that kept showing up in the different cluster analyses. 

Objective 2: I used additive models to assess features of TED Talks that predict popularity. Additive models fit nonlinear functions to each categorical covariate. The additive models were fit using the ‘mgcv’ package in R. My roles included understanding the syntax of mgcv to fit AM. For the AM, we used thin-plate splines with shrinkage. I selected a few functions from the AM and plotted them.

Objective 3:  To answer this question, I used varying coefficient models (VCMs). VCMs model a function of time with each continuous and categorical variable. The final VCM was fitted under a cubic spline with shrinkage. I then explored different plotting tools to best display the results. Selected functions from the fitted VCM were plotted at different time-points to demonstrate how the features of TED Talk popularity change with time. 

Objective 4: Using the results of a factor analysis performed on theme from another team member, I repeated the methods for Objective 2 for each theme. The number of videos in each theme was different; therefore I repeated the analysis with the minimum sample size and compared to the analyses with the total sample size to assess if using the total sample size was appropriate. 

