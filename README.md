# ClinPlots
### An R package to create clinico-genomic charts

There is a need to rapidly and effectively summarize multivariate risk information for patients and physicians. Many visualizations that accomplish this task are exhibited in the paper [*Clinical assessment incorporating a personal genome*](http://www.thelancet.com/journals/lancet/article/PIIS0140-6736(10)60452-7/fulltext), which provides the motivation for developing this package. Two examples of these figures can be seen below, whose reproduction will become automated through this package.

![Figure 1](https://cloud.githubusercontent.com/assets/12614369/21712667/06142324-d3c4-11e6-8a78-d03a1b4898cc.png)

![Figure 2](https://cloud.githubusercontent.com/assets/12614369/21381729/0a219992-c72b-11e6-8c65-8d51112e35eb.png)

A sample script to generate both plots, using the data above:
```R
require(ggplot2)
require(scales)
require(gridExtra)
require(grid)
library(ClinPlots)

clinical_risk <- read.csv(system.file("extdata", "clinical_risk.txt", package = "ClinPlots"), sep='\t')
disorderCohortRiskPlot(clinical_risk)

MI_df <- read.csv(system.file("extdata", "MI_genomic_risk.txt", package = "ClinPlots"), sep='\t')
genomicRiskSummaryPlot(MI_df, "pt_probability") #running genomicRiskSummaryPlot(MI_df, 7) will produce same result
```
