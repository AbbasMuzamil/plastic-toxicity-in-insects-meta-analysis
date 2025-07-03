# Setup R environment

#Read in R packages, make a ggplot theme to be used throughout the code and for all figures.


install.packages("MuMIn")
install.packages("multcomp")
install.packages("emmeans")
install.packages("ggeffects")
install.packages("forcats")
install.packages("stringr")
install.packages("clubSandwich")
install.packages("cowplot")

setRepositories()
library(metafor)
library(MuMIn)
library(tidyverse)
library(multcomp)
library(emmeans)
library(ggeffects)
library(forcats)
library(stringr)
library(clubSandwich)
library(cowplot)

##Make ggplot theme to use throughout
theme_JR <- function (base_size = 12, base_family = "") 
{
  theme(
    panel.background = element_rect(fill=NA),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_line(color="black"),
    legend.title = element_text(size=18,color="black"),
    legend.text = element_text(size=16,color="black"),
    legend.key = element_rect(fill=NA,color=NA),
    axis.text = element_text(size=12,color="black"),
    axis.title = element_text(size=16,color="black"),
    strip.background =element_rect(fill=NA, color = "black")
  )
}

##Read in dataset
fulldat2<- read_excel(file.choose(), sheet = 1, col_names = T)

#```

## Grand Mean effect of GCDs on disease

#Analysis of grand mean indicates that overall, global change will increase disease.

#```{r, Overall model}
mgrand <- rma.mv(SMDHyi, SMDHvic,
                 random = list(~1|id2, ~1|Study),
                 data = fulldat2)

mgrandro <- robust(mgrand, cluster=id2, clubSandwich=TRUE)
mgrandro

##I2 (heterogeneity statistic) calculation
W <- diag(1/mgrandro$vi)
X <- mgrandro$X
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(mgrandro$sigma2) / (sum(mgrandro$sigma2) + (mgrandro$k-mgrandro$p)/sum(diag(P)))

##I2 = 0.9988417

##17.77732 is due to between study variation; 82.10685 is due to within study variation
100 * mgrand$sigma2 / (sum(mgrand$sigma2) + (mgrand$k-mgrand$p)/sum(diag(P)))

