---
# Documentation: https://sourcethemes.com/academic/docs/managing-content/

title: "Regression with interval-censored covariates: Application to cross-sectional incidence estimation"
authors: [Douglas Ezra Morrison, Oliver Laeyendecker, Ron Brookmeyer]
date: 2021-04-17
doi: "https://doi.org/10.1111/biom.13472"

# Schedule page publish date (NOT publication's date).
publishDate: 2019-06-22T14:27:48-07:00

# Publication type.
# Legend: 0 = Uncategorized; 1 = Conference paper; 2 = Journal article;
# 3 = Preprint / Working Paper; 4 = Report; 5 = Book; 6 = Book section;
# 7 = Thesis; 8 = Patent
publication_types: ["2"]

# Publication name and optional abbreviated publication name.
publication: "Biometrics"
publication_short: ""

abstract: "A method for generalized linear regression with interval-censored covariates is described, extending previous approaches. A scenario is considered in which an interval-censored covariate of interest is defined as a function of other variables. Instead of directly modeling the distribution of the interval-censored covariate of interest, the distributions of the variables which determine that covariate are modeled, and the distribution of the covariate of interest is inferred indirectly. This approach leads to an estimation procedure using the EM algorithm. The performance of this approach is compared with two alternative approaches, one in which the censoring interval midpoints are used as estimates of the censored covariate values, and another in which the censored values are multiply imputed using uniform distributions over the censoring intervals. A simulation framework is constructed to assess these methods’ accuracies across a range of scenarios. The proposed approach is found to have less bias than midpoint analysis and uniform imputation, at the cost of small increases in standard error."

# Summary. An optional shortened abstract.
summary: ""

tags: [Cross-Sectional Incidence Estimation, R]
categories: []
featured: false

# Custom links (optional).
#   Uncomment and edit lines below to show custom links.
# links:
# - name: Follow
#   url: https://twitter.com
#   icon_pack: fab
#   icon: twitter

url_pdf: 
url_code: https://github.com/d-morrison/rwicc
url_dataset:
url_poster: 
url_project: https://d-morrison.github.io/rwicc
url_slides:
url_source:
url_video:
---
