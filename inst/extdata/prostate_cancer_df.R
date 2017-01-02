#create myocardial infarction data frame based on Figure 1A in repo
#repo link: https://github.com/JakeConway/ClinPlots
prostate_cancer_df <- data.frame(
  gene=c(NA, 'LPA', 'THBS2', 'LDLR', 'LIPC', 'ESR2', 'ESR2', 'FXN'),
  SNP_loc=c(NA, 'rs3798220', 'rs8089', 'rs14158', 'rs11630220', 'rs1271572', 'rs35410698', 'rs3793456'),
  genotype=c(NA, 'CT', 'AC', 'GG', 'AG', 'CC', 'GG', 'AA'),
  LR=c(NA, 1.86, 1.09, 2.88, 1.15, 0.73, 1.03, 0.94),
  studies=c(NA, 2, 1, 1, 1, 1, 1, 1),
  samples=c(NA, 17031, 4868, 3542, 3542, 3089, 1094, 1094),
  pt_probability=c(2.0, 3.7, 4.0, 10.6, 12.0, 9.1, 9.4, 8.9)
  )

#write table to a file your working directory (will be called prostate_cancer.txt)
#use setwd('/Users/your_username/Desktop') to make the working directory your desktop
write.table(prostate_cancer_df, file='prostate_cancer.txt', quote=FALSE, sep='\t', row.names = FALSE)