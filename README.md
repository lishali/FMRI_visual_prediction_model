FMRI_visual_prediction_model
============================
Introduction
============================

The human brain processes a dizzying array of electromagnetic radiation into a coherent *picture* of the world. How it does so is a fascinating scientific question.  After all, the bounty of visual metaphors that pervade thought and speech is a clear reminder of the integral part vision plays in cognition.  In this lab, armed with some FMRI data collected by the Gallant lab, we build and analyze predictive models for brain response to natural pictures, and study these models to inform the scientific question of how we process visual information, with the goal of decoding brain responses back to visual data. 

Data
============================
The response dataset was collected by an FMRI machine, which measures the intensity of blood flow to regions of the brain.  The degree of resolution of these measurements are $2mm \times 2mm \times 4mm$ regions called voxels.  The subject was shown thousands of natural images (1750 of which are provided for the training set of this lab and 120 for testing) and the intensity of blood flow was recorded for each voxel as a vector of response, of length the number of pictures.  FMRI data is riddled with noise, and the dataset was already preprocessed in many ways, such as decorrelating voxel intensities with ambient intensities (periodic or random) across the brain. 

Our dataset is restricted to the responses of 20 voxels, all located in what is denoted the $V1$ processing center of the brain.  V1 is regarded as the first processing center, and many papers suggest that our brain at this level does a Gabor wavelet like transformation.  This is motivation for the initial transformation of the feature set, given by $120 \times 120$ pixel intensities of a grayscale image, using Gabor wavelets.  This transformation is essentially a localized complex Fourier transforms of the $120 \times 120$ dimensional space.  Our data matrix is thus a matrix of the coefficients of the basis of wavelets.

EDA
============================

![](/figures/voxel_plot.png)

![](/figures/response_density_EDA.pdf)

![](/figures/intensity_density_EDA.pdf)


The first figure graphs where the 20 voxels lie with respect to each other.  The voxels were coloured by splitting up one of the axis in half to give better depth perception.  We are aware that the topology of our visual field is preserved in the mapping of stimulus to V1.  We don't know how this mapping relates to the euclidean distance of the voxels, but it keeps us thinking about whether voxels that are further far apart are in charge of further parts of the visual field.  We will check this assumption in the interpretation part of the lab.  

The other two plots above give us an idea of the density of measurements, across voxels (middle figure) and for the first 10 features (leftmost figure).  The density of intensities across voxels is comparably distributed, and normal-like.  The density of coefficients is less so, we see there are some which remain 0.  This motivates our normalizing the feature dataset before performing analysis.  

LASSO
============================

##Preprocessing
Before doing LASSO, and any of the other models, we centered and scaled our feature data.  In the case of feature shrinkage in regression models, this ensures the penalty will effect all features equally.  Since each feature is a coefficient of the Gabor wavelets, they are not initially on a comparable scale, hence the need to scale by the standard deviation of each feature column.  

##Model Selection: CV, ES-CV, AIC, BIC, AICc

LASSO provides a family of regression models parametrized by the penalty magnitude $\lambda$; the greater the $\lambda$ the more degrees of freedom we lose.  In our problem $p >> n$, so feature shrinkage is needed. Choosing the right value of lambda to minimize the prediction error, yet not overfit, and demonstrate stability, comprise our model selection criteria.  AIC, BIC and AICc don't provide theoretical guarantees for this high dimensional regime, but it is interesting that they do corroborate the choices of lambda by CV and ES-CV as seen by the graphs below.  We select the graphs for voxels 1, 5, 10, 15, 20, as they maximize the physical distance between voxels. 10 and 20 seem to be outliers, as the graphs of the rest of the voxels, not displayed in this lab, resemble that of voxel 1. Compared to other feature shrinkage models, LASSO has the added benefit of interpretability; the $L^1$ norm is "pointy" on the axis (given by the feature set) and thus sends feature to 0 with increasing $\lambda$.   We analyze the model performance of 100 models per voxel, parametrized by a grid of 100 possible lambda values.

The following plots of full of information.  Cross validation was performed on 80\% of the 1750 data points.  The remaining 20\% was left for testing.  We tried both 5 fold and 10 fold cross validation, reporting below the results for 10 fold cross validation as they provided more stable ESCV results, but ultimately the conclusions did not change.  We present three methods from choosing an optimal lambda from the cross validation folds, Lambda.min, Lambda.1se and ES-CV.  Lambda.min corresponds to  the $\lambda$ that minimizes cross validation error, and is displayed by the leftmost vertical line in the CV plots.  The right-most line corresponds to Lambda.1se, which is the largest lambda that we can get away with that is within 1 stander error from Lambda.min.  This choice of $\lambda$ channels the philosophy that we move in the direction of increasing regularization; all else being equal, we go for the simpler solution.   

The graphs are aligned so one can compare the lambda chosen by CV methods with the AIC, BIC and AICc plot.  Though the latter three estimators achieved minimums for the largest lambda in the domain of lambdas considered, there is an obvious point at which their derivative decreases dramatically, to a almost flat slope.  This was the point that almost always corresponded to the lambdas chosen by CV methods within a small degree of error, hence providing nice support for the choices made by CV methods.  Finally, we can read of the degrees of freedom of the LASSO model for each voxel on the top of the x-axis, which consistently chose around 90 parameters at Lambda.min. Voxel 10, 16 and 20 were outliers.  

![](/figures/Voxel_1_AIC.pdf)

![](/figures/Voxel_5_AIC.pdf)

![](/figures/cv_lasso1.pdf)

![](/figures/cv_lasso5.pdf)


For the entire lab result details look in [writeup](Writeup.pdf)



\section{Conclusion}

In this lab, we were guided by the scientific question of how the brain processes visual data.  The dataset belonged to the regime of high dimensional problems, and many feature selection methods were used to optimize for both goals of predictability and interpretability.  Though random forest performed better in terms of the former goal, LASSO came out on top as achieving a balance of the two.  The penalty chosen and prediction error were stable against changes in (most) voxels, number of cross validation folds and size of the test set.  The predictions produced by the LASSO model captured the correlation structure of the original 20 voxels, though with lighter intensity.  This model also helped us derive some interpretive analysis of the features most important to voxels 1 and 10. 
