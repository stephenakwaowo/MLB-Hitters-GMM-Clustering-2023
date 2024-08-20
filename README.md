# Clustering-MLB-Hitters: Gaussian Mixture Models, Sequential Dimensionality Reduction Strategy, and the BONDS Index, Visualized with Shiny


### Project Overview
This project utilizes advanced statistical and machine learning techniques to analyze and cluster MLB hitters based on their performance in the 2023 season. The goal is to uncover distinct patterns and groupings that transcend traditional statistical analysis, offering new insights into player performance.


### Methodology
**1. Sequential Dimensionality Reduction:** Employed an approach to first reduce dimensionality with PCA, ensuring minimal loss of significant variance, followed by t-SNE for more nuanced visualization of high-dimensional data in a low-dimensional space.

**2. Clustering:** Applied Gaussian Mixture Models (GMM) to discover latent player groupings, with flexibility to account for multi-cluster memberships.

**3. Model Selection:** Determined the optimal number of clusters using Akaike Information Criterion (AIC) and Bayesian Information Criterion (BIC).

**4. Validation:** Used silhouette scores to evaluate clustering quality.



### Visualization

1. Utilized t-SNE plots and GMM confidence ellipses to visually depict player clusters.

2. Radar charts were generated to visualize each cluster's profile, highlighting their strengths and weaknesses across different feature categories.


### Key Findings
- Detailed the distinct player profiles emerging from clusters, highlighting potential undervalued skill sets.
- Demonstrated that non-traditional metrics can provide valuable insights into player evaluation.
- The clustering analysis revealed distinct groupings of players that correlate with higher levels of on-field success. Each cluster had unique combinations of strengths in power hitting, swing decisions, bat-to-ball skills, and baserunning. These insights could assist team management and coaching staff in player assessment and strategic decision-making.



### Technologies Used
**Python:** Main programming language.

**pandas, numpy:** Data manipulation and numerical operations.

**matplotlib, seaborn, plotly:** Data visualization.

**scikit-learn:** Machine learning and data preprocessing.

**scikit-fuzzy:** For applying Fuzzy C-Means clustering, allowing for soft cluster assignments where players can belong to multiple clusters with varying degrees of membership.

**shiny:** For deploying interactive web applications to showcase cluster analysis.


### Deployment
The visualization and interaction layer is deployed using Shiny, which allows users to interact with the data:

**- Interactive Radar Charts:** Compare players within and across clusters.

**- Dynamic Metric Tables:** Filter and examine detailed player statistics based on selected clusters.


### How to Use

1. Clone the repository.
2. Install all required libraries as listed in `requirements.txt`.
3. Execute the Jupyter notebook to view the analysis and regenerate the results.
4. Access the Shiny app for interactive visualizations and data exploration.


### Libraries to Use

```Python
import torch
import torch.nn as nn 
import torch.nn. functional as F
import torchvision
import torchvision.transforms as transforms
import skfuzzy as fuzz
import sklearn.metrics as metrics
import pandas as pd
import random
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import datetime
import sqlite3
from random import sample
from sklearn.preprocessing import StandardScaler
from sklearn.neighbors import NearestNeighbors
from sklearn.decomposition import PCA
from sklearn.manifold import TSNE
from sklearn.mixture import GaussianMixture
from matplotlib.patches import Ellipse
from scipy.spatial.distance import pdist, squareform
from sklearn.metrics import silhouette_score
from IPython.display import display
from math import pi
```
```R
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(htmltools)
library(mclust)
library(DT)
library(ggplot2)
library(ggfortify)
library(webshot2)
library(htmlwidgets)
library(rvest)
```

Python:  `pip install -r requirements.txt`

R: Use the provided R script or install packages individually as needed.
