# Clustering-MLB-Hitters: A project on Gaussian Mixture Models, OPTICS, Spectral graph-based clustering & Fuzzy C-Means

### Project Overview
This project aims to group MLB hitters into clusters based on a comprehensive set of non-traditional batting metrics. By moving beyond standard statistics like batting average and RBIs, the project seeks to identify inherent similarities among players and uncover patterns that traditional metrics may overlook.

### Methodology
**1. Data Collection:** Assembled datasets from Fangraphs and Baseball Reference which includes various advanced metrics that provide deeper insights into player performances.

**2. Feature Selection:** Chose to focus on metrics such as O-Contact%, 50th Max Exit Velocity, Barrels per Batted Ball Event (Brls/BBE%), etc., to form the basis for clustering.

**3. Preprocessing:** Standardized the data to ensure fair comparison and employed Principal Component Analysis (PCA) for dimensionality reduction while retaining 95% variance.

**4. **Clustering Algorithms:****

Applied four clustering techniques to create a diverse representation of hitter groupings:

**- OPTICS:** For density-based clustering

**- GMM:** For distribution-based clustering

**- Spectral Clustering with Louvain Modularity:** For graph-based clustering

**- Fuzzy C-Means:** For centroid-based clustering

**5. **Cluster Evaluation:**** Utilized silhouette scores, AIC/BIC criteria, and t-SNE visualizations to evaluate the quality of the clusters and refine the model.

**6. **Insights and Results:**** Analyzed cluster characteristics to identify player types, explored the relationship between clusters and player success, and interpreted how certain clusters may predict future performance.


### Visualization

**Radar charts** were generated to visualize each cluster's profile, highlighting their strengths and weaknesses across different feature categories.

### Key Findings
- Detailed the distinct player profiles emerging from clusters, highlighting potential undervalued skill sets.
- Demonstrated that non-traditional metrics can provide valuable insights into player evaluation.
- The clustering analysis revealed distinct groupings of players that correlate with higher levels of on-field success. Each cluster had unique combinations of strengths in power hitting, swing decisions, bat-to-ball skills, and baserunning. These insights could assist team management and coaching staff in player assessment and strategic decision-making.


### Technologies Used
Python for data analysis and modeling, with libraries such as pandas, NumPy, scikit-learn, and Matplotlib.


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
