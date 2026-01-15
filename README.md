![Status](https://img.shields.io/badge/Status-Phase_1_Complete-success)
![Language](https://img.shields.io/badge/Language-R-blue)
![Tools](https://img.shields.io/badge/Tools-Tidyverse_%7C_MICE-purple)
![License](https://img.shields.io/badge/License-MIT-green)

# Laptop Price Analysis: Data Engineering & EDA

This project turns a raw, messy Amazon laptop data into a clean dataset for market analysis. The data is available at [Kaggle](https://www.kaggle.com/datasets/talhabarkaatahmad/laptop-prices-dataset-october-2023) 

Real-world data is rarely ready for modeling. It contains inconsistent units, typing errors, and unstructured text. I built this pipeline to fix those issues and ensure statistical integrity before running any price statistical models.

**Current Status:** Phase 1 Complete (Data Cleaning, Feature Engineering, and Exploratory Analysis).

## The Problem

You cannot analyze market trends if your data is wrong. The raw dataset presented challenges that would break a standard regression model, like:

1.  **Domain Errors:** High-performance laptops were listed with storage in "MB" instead of "GB". This creates massive outliers.
2.  **Unstructured Text:** Critical specs like CPU and GPU were hidden in long description strings.
3.  **Missing Data:** several rows lacked specific technical details.

## My Solution

I used R and the Tidyverse ecosystem to build a robust cleaning pipeline.

### 1. Correcting Domain Errors
I found a logic error in the source data. Modern SSDs do not have "512MB" of storage. I wrote a script to detect these context-based errors and standardized all storage units to GB. This step prevented artificial outliers from skewing the price analysis.

### 2. Regex Parsing
I used `stringr` and regular expressions to extract structured features from text descriptions. Example:
* **CPU:** Extracted processor families (e.g., "Core i7", "Ryzen 5"), clock speeds and verified standard measures (GHz).
* **GPU:** Separated integrated graphics from dedicated video cards.
* **Screen:** Standardized resolution metrics (inches).

### 3. Statistical Imputation (MICE)
Deleting rows with missing values introduces bias and reduces sample size. I avoided this by using MICE (Multivariate Imputation by Chained Equations). This method uses the relationships between other hardware specs to estimate the missing values accurately.

## Tech Stack

* **Language:** R
* **Data Manipulation:** dplyr, tidyr, stringr
* **Imputation:** mice
* **Visualization:** ggplot2

## Key Findings (EDA) (This section is under construction)

After cleaning the data, I found clear patterns in the market.

**Price vs. Hardware Tiers**
The extracted CPU and GPU tiers show a strong correlation with price. The cleaning process successfully separated entry-level machines from high-performance workstations.

<img width="1726" height="1074" alt="image" src="https://github.com/user-attachments/assets/a59e49a8-ed8a-4dc5-9e57-adbf08ab0708" />

> *Figure 1: Validation of CPU Feature Engineering. The boxplot confirms the price hierarchy across the five extracted CPU tiers. The progressive increase in median price demonstrates that the Regex parsing correctly categorized the processors based on performance levels, from Low-End to High-End.*

<br> <img width="1726" height="1074" alt="image" src="https://github.com/user-attachments/assets/a701b6b8-efe5-4f47-b33b-862770711616" />

> *Figure 2: Validation of Feature Engineering. The boxplot demonstrates a clear price progression across the extracted GPU tiers. The distinct separation between "Integrated" and "High-End" medians confirms that the regex logic successfully isolated different market segments.*



**Anomalies Detected**
I used clustering on the hardware specs to find pricing anomalies. Some laptops with "Premium" hardware specs are currently priced in the "Intermediate" range. This indicates potential arbitrage opportunities.

## Next Steps

I am currently working on Phase 2. I will build a predictive model to estimate the fair market value of laptops based on the features I engineered.
