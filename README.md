# Unsupervised-Anomaly-Detection
Unsupervised Anomaly Detection in Time Series Using Hidden Markov Models
Group 19

## Authors
Arshdeep Kaur (aka232) – 301540172

Tushar Singh (tsa156) – 301560473

David Mulej (dma122) – 301539538

Victor Nguyen (avn) – 301458739

## Project Scope
This project focuses on identifying anomalous patterns in electricity consumption data using unsupervised machine learning techniques. Specifically, we implement and evaluate Hidden Markov Models (HMMs) on time series data to detect deviations that could indicate operational faults or cyber threats in supervisory control systems. The analysis includes:

Data Cleaning & Preprocessing (feature scaling, interpolation, outlier removal)

Dimensionality Reduction using Principal Component Analysis (PCA)

Model Training and Selection using log-likelihood and BIC metrics

Anomaly Detection using log-likelihood deviation thresholds

The dataset spans four years (2006–2009), and models are trained on the first three years and tested on the fourth.

## Technical Workflow
Preprocessing: Handle missing values, interpolate, and standardize features using Z-score.

PCA: Reduce dimensionality and identify the top contributing features.

HMM Training: Use depmixS4 to train models with 4–8 hidden states on selected features.

Evaluation: Compute normalized log-likelihood for train/test data and select best model via BIC.

Anomaly Detection: Divide test data into weeks and calculate deviation from training likelihood to detect anomalies.

## Required Libraries
This project is implemented in R. The following libraries must be installed:

zoo:	Interpolating missing values using na.approx,
ggplot2:	Visualizing trends and distributions,

stats:	Performing PCA using prcomp(),
depmixS4:	Core package for HMM modeling and log-likelihood evaluation.
