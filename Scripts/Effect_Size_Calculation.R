#Code to calculate SMDH and LRR from raw data and coefficients

##load packages
library(tidyverse); library(esc); library(metafor); library(effectsize);  library(writexl)

theme_JR <- function (base_size = 12, base_family = "")
{
  theme(
    panel.background = element_rect(fill=NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color="black"),
    legend.title = element_text(size=18,color="black"),
    legend.text = element_text(size=16,color="black"),
    legend.key = element_rect(fill=NA,color=NA),
    axis.text = element_text(size=12,color="black"),
    axis.title = element_text(size=16,color="black")
  )
}

##read in dataset
meta1<- read_excel(file.choose(), sheet = 1, col_names = T)



#Calculate Log Response Ratio (LRR)
d2 <- escalc(measure = "SMD",
             m1i = TM, sd1i = TSD, n1i = Tn,
             m2i = CM, sd2i = CSD, n2i = Cn,
             data = meta1)


df <- d2 %>%
  mutate(yi = case_when(
    End.Point %in% c("Food selection rate", "Locomotory activity", "Stage Duration", "Climbing Activity", "Emergence ratio", 
                      "Emerged individual number", "Egg to Pupae", "Embryo to Eclosure", "Adult Emerged", 
                      "Number of eggs", "Total Offspring", "Body length", "Body weight", "Wing length", 
                     "Wing Size", "Growth Rate", "Protein Level", "Frame Number used","Frame cover adult", "Lipid Level", "Micro elements level","Left wing",
                     "Right wing", "Food intake","Protein level, Lipid level; Micro elements level", "Protein Content", "TG Content", "Survival", "D-Glucose content", "Shannon Diversity",
                     "Simpson Diversity", "Interior Width", "Case Length", "Sum activity") & yi > 0 ~ yi * -1,
    End.Point %in% c("Food selection rate", "Locomotory activity", "Stage Duration", "Climbing Activity", "Emergence ratio", 
                      "Emerged individual number", "Egg to Pupae", "Embryo to Eclosure", "Adult Emerged", 
                      "Number of eggs", "Total Offspring", "Body length", "Body weight", "Wing length", 
                      "Wing Size", "Growth Rate", "Protein Level", "Frame Number used","Frame cover adult", "Lipid Level", "Micro elements level","Left wing",
                      "Right wing", "Food intake","Protein level, Lipid level; Micro elements level", "Protein Content",  "Survival", "TG Content", "D-Glucose content", "Shannon Diversity",
                      "Simpson Diversity", "Interior Width", "Case Length", "Sum activity") & yi <= 0 ~ yi * 1,
    End.Point %in% c("Sleep time", "Residence Time", "Development time", 
                      "Emerging time", "Larval cycle", "LPO", "ROS", "CAT", 
                      "SOD", "GST", "MDA","Propionic acid", "Butyric acid", 
                      "Caprylic acid", "Lauric acid", "Myristic acid", "Myristoleic acid", 
                      "Pentadecylic acid", "Palmitic acid", "Palmitoleic acid", "Margaric acid", 
                      "Stearic acid", "Elaidic acid", "Oleic acid", "Vaccenic acid", "Linoleic acid", 
                      "Linolenic acid", "g-Linolenic acid", "Arachidic acid", "Gondoic Acid", "C20:2", 
                      "Arachidonic acid", "Eicosapentaenoic acid", "Behenic Acid", "Docosadienoic acid", 
                      "Adrenic acid", "Docosahexaenoic acid", "Nervonic acid", "Anti-Oxidant") & yi < 0 ~ yi * -1,
    End.Point %in% c("Sleep time", "Residence Time","Development time", 
                     "Emerging time", "Larval cycle", "LPO", "ROS", "CAT", 
                     "SOD", "GST", "MDA", "Propionic acid", "Butyric acid", 
                     "Caprylic acid", "Lauric acid", "Myristic acid", "Myristoleic acid", 
                     "Pentadecylic acid", "Palmitic acid", "Palmitoleic acid", "Margaric acid", 
                     "Stearic acid", "Elaidic acid", "Oleic acid", "Vaccenic acid", "Linoleic acid", 
                     "Linolenic acid", "g-Linolenic acid", "Arachidic acid", "Gondoic Acid", "C20:2", 
                     "Arachidonic acid", "Eicosapentaenoic acid", "Behenic Acid", "Docosadienoic acid", 
                     "Adrenic acid", "Docosahexaenoic acid", "Nervonic acid", "Anti-Oxidant")& yi >= 0 ~ yi * 1,
    TRUE ~ yi
  ))
                    

meta1$LRRyi = df$yi
meta1$LRRvi = df$vi




#Calculate Hedge's g (SMDH)
d3 <- escalc(measure = "SMDH",
             m1i = TM, sd1i = TSD, n1i = Tn,
             m2i = CM, sd2i = CSD, n2i = Cn,
             data = meta1)

df2 <- d3 %>%
  mutate(yi = case_when(
    End.Point %in% c("Food selection rate", "Locomotory activity", "Stage Duration", "Climbing Activity", "Emergence ratio", 
                     "Emerged individual number", "Egg to Pupae", "Embryo to Eclosure", "Adult Emerged", 
                     "Number of eggs", "Total Offspring", "Body length", "Body weight", "Wing length", 
                     "Wing Size", "Growth Rate", "Protein Level", "Frame Number used","Frame cover adult", "Lipid Level",  "Survival", "Micro elements level","Left wing",
                     "Right wing", "Food intake","Protein level, Lipid level; Micro elements level", "Protein Content", "TG Content", "D-Glucose content", "Shannon Diversity",
                     "Simpson Diversity", "Interior Width", "Case Length", "Sum activity") & yi > 0 ~ yi * -1,
    End.Point %in% c("Food selection rate", "Locomotory activity", "Stage Duration", "Climbing Activity","Emergence ratio", 
                     "Emerged individual number", "Egg to Pupae", "Embryo to Eclosure", "Adult Emerged", 
                     "Number of eggs", "Total Offspring", "Body length", "Body weight", "Wing length", 
                     "Wing Size", "Growth Rate", "Protein Level", "Frame Number used","Frame cover adult", "Lipid Level",  "Survival", "Micro elements level","Left wing",
                     "Right wing", "Food intake","Protein level, Lipid level; Micro elements level", "Protein Content", "TG Content", "D-Glucose content", "Shannon Diversity",
                     "Simpson Diversity", "Interior Width", "Case Length", "Sum activity") & yi <= 0 ~ yi * 1,
    End.Point %in% c("Sleep time", "Residence Time", "Development time", 
                     "Emerging time", "Larval cycle", "LPO", "ROS", "CAT", 
                     "SOD", "GST", "MDA", "Propionic acid", "Butyric acid", 
                     "Caprylic acid", "Lauric acid", "Myristic acid", "Myristoleic acid", 
                     "Pentadecylic acid", "Palmitic acid", "Palmitoleic acid", "Margaric acid", 
                     "Stearic acid", "Elaidic acid", "Oleic acid", "Vaccenic acid", "Linoleic acid", 
                     "Linolenic acid", "g-Linolenic acid", "Arachidic acid", "Gondoic Acid", "C20:2", 
                     "Arachidonic acid", "Eicosapentaenoic acid", "Behenic Acid", "Docosadienoic acid", 
                     "Adrenic acid", "Docosahexaenoic acid", "Nervonic acid", "Anti-Oxidant") & yi < 0 ~ yi * -1,
    End.Point %in% c("Sleep time", "Residence Time", "Development time", 
                     "Emerging time", "Larval cycle", "LPO", "ROS", "CAT", 
                     "SOD", "GST", "MDA", "Propionic acid", "Butyric acid", 
                     "Caprylic acid", "Lauric acid", "Myristic acid", "Myristoleic acid", 
                     "Pentadecylic acid", "Palmitic acid", "Palmitoleic acid", "Margaric acid", 
                     "Stearic acid", "Elaidic acid", "Oleic acid", "Vaccenic acid", "Linoleic acid", 
                     "Linolenic acid", "g-Linolenic acid", "Arachidic acid", "Gondoic Acid", "C20:2", 
                     "Arachidonic acid", "Eicosapentaenoic acid", "Behenic Acid", "Docosadienoic acid", 
                     "Adrenic acid", "Docosahexaenoic acid", "Nervonic acid", "Anti-Oxidant")& yi >= 0 ~ yi * 1,
    TRUE ~ yi
  ))




meta1$SMDHyi = df2$yi
meta1$SMDHvi = df2$vi


##Need to change variances: many magnitudes of difference between high and low 
##LRR and SMDH variance values; Wolfgang Viechtbauer (author of `metafor` package)
##recommends changing those very large and very small values to less extreme values 

decr <- sort(meta1$LRRvi, decreasing = T)
incr <- sort(meta1$LRRvi)

decr2 <- sort(meta1$SMDHvi, decreasing = T)
incr2 <- sort(meta1$SMDHvi)


##convert all variances that are above 10 to be the greatest variance value in
##the dataset that is below 10 (for both LRR and SMDH)
##simultaneously convert all variances below 0.00001 (1E-5) to be the smallest
##variance value that is above 0.00001; this results in 7 magnitudes of difference
##between high and low variance values; while converting 21 LRR and 45 SMDH variances
##this is ~0.7% and ~1.5% of the data, respectively

##This approach allows for variance data to be very close to that of the original
##data, while allowing the rma.mv function to actually run; error will happen if 
##magnitude of difference between high and low variance values is < 8

meta1 = meta1 %>%
  mutate(LRRvic = ifelse(LRRvi < incr[min(which(incr > 0.0001))],
                            incr[min(which(incr > 0.0001))],
                            ifelse(LRRvi > decr[min(which(decr < 40))],
                                   decr[min(which(decr < 40))],
                                   LRRvi)),
         SMDHvic = ifelse(SMDHvi < incr2[min(which(incr2 > 0.0001))],
                             incr2[min(which(incr2 > 0.0001))],
                             ifelse(SMDHvi > decr2[min(which(decr2 < 40))],
                                    decr2[min(which(decr2 < 40))],
                                    SMDHvi)))
Save the results to an Excel file
write_xlsx(meta1, "K:/Abbas/Meta-Analysis/Meta-Analysis/meta2.xlsx")

# Print a confirmation message
print("Data frame has been saved to Excel file.")
