🌎 Language: [English](README.md) | [Español](README.es.md)

# Statistical Supervised Learning Methods for Land Cover Detection from Satellite Imagery

## 📌 Abstract

This repository contains the **R implementation** of a **Bachelor’s thesis in Actuarial Science (UNAM)** whose objective is to **classify land cover (agriculture vs. non-agriculture)** using **Landsat 5 satellite imagery**, through **statistical supervised learning methods**.

The work adopts a **reproducible and comparative** approach, integrating:

- Satellite image preprocessing  
- Spectral and texture feature engineering  
- Training of classification models  
- Rigorous evaluation of predictive performance  

Special emphasis is placed on **practical limitations**, **computational costs**, and **temporal generalization**, aspects that are highly relevant in both **academic** and **industrial** contexts.

---

## 🎓 Academic context

This repository corresponds to the following thesis:

> **“Statistical supervised learning methods for land cover detection from satellite imagery”**  
> **Author:** Héctor Miguel Olivares García  
> **Bachelor’s Degree in Actuarial Science – Faculty of Sciences, UNAM (2023)**

📄 The complete thesis document can be found in the `doc/` folder.

---

## 🌍 Motivation and problem statement

Land cover classification is a key tool for:

- Environmental monitoring  
- Territorial planning  
- Public policy evaluation  
- Climate change and sustainability studies  

Traditional field-based data collection methods are often **expensive**, **slow**, and **difficult to update**.  
This project explores the use of **freely available satellite imagery**, combined with **classical statistical and machine learning methods**, as a **scalable and reproducible** alternative.

---

## 🎯 Project objective

To classify satellite image pixels into two categories:

- **Agriculture**
- **Non-agriculture**

using **multispectral information**, **derived indices**, and imagery from **different seasons of the same year**, in order to analyze the performance and stability of the trained models.

---

## 🧠 General methodology

### 1️⃣ Satellite image acquisition
- Landsat 5  
- Publicly available labeled data  

### 2️⃣ Preprocessing
- Radiometric correction  
- Geographic reprojection  
- Spectral indices: **NDVI, SAVI, MSAVI**  
- Texture indices using **GLCM**

### 3️⃣ Dataset construction
- Each pixel is treated as an observation  
- Spectral + texture variables + land cover label  

### 4️⃣ Supervised models
- Logistic, probit, and LASSO regression  
- K-Nearest Neighbors (KNN)  
- Support Vector Machines (SVM)  
- Random Forest  

### 5️⃣ Evaluation
- Repeated Hold-Out  
- K-fold cross-validation (K = 10)  
- Metrics: accuracy, sensitivity, and specificity  

### 6️⃣ Temporal generalization
- Training on one season  
- Evaluation on a different season of the same year  

---

## 📊 Main results

- **Random Forest** achieved the best performance:
  - Approximate accuracy of **87%**
  - Strong performance on the minority class (agriculture)
- Significant differences in **execution times**
- Performance degradation when switching seasons, highlighting:
  - The need for continuous monitoring
  - The importance of retraining under seasonal changes

---

## 💻 Technologies and tools

- **Language:** R  
- **IDE:** RStudio  
- **Satellite imagery:** `raster`, `terra`, `RStoolbox`  
- **Machine Learning:** `caret`, `randomForest`, `e1071`  
- **Texture analysis:** `glcm`  
- **Computing:** parallel programming  

> ℹ️ Some parts of the code were updated with respect to the original thesis due to library changes or deprecations.

---

## 📁 Repository structure

```text
├── doc/
│   └── Tesis_Olivares_García_Héctor.pdf
│
├── scripts/
│   ├── 00_func.R
│   ├── 01_preproc.R
│   ├── 02_aplication.R
│   └── 03_examples.R
│
└── README.md
```
📚 References

This project is based, among others, on the methodology described in:

Kamusoko, C. (2013, 2019). Remote Sensing Image Classification in R

This work was a key reference for:

  - Image preprocessing

  - Feature construction

  - Classification and evaluation strategies

⚠️ Limitations and future work
Limitations

  - Specific geographic region (Harare, Zimbabwe)

  - Deep learning models were not explored

  - Temporal generalization remains a major challenge

Future work

  - Incorporate spatial or temporal models

  - Evaluate higher-resolution imagery

  - Automate retraining pipelines

📎 How to cite this work

If you use this repository or the thesis as an academic reference:

Olivares García, H. M. (2023). Statistical supervised learning methods for land cover detection from satellite imagery. Faculty of Sciences, UNAM.

👤 Author

Héctor Miguel Olivares García
Actuary – UNAM
