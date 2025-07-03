##Repeat first two sets of analyses with LRR (Log Response Ratio) instead of SMDH
library(tidyverse); library(emmeans); library(metafor); library(cowplot)

fulldat2<- read_excel(file.choose(), sheet = 1, col_names = T)


## need to flip sign of LRR(Log Response Ratio) to match SMHD
m2lrr <- rma.mv(LRRyi, LRRvic,
                mods = ~  index.effect - 1,
                random = list(~1|id2, ~1|Study),
                data = meta1)
m2lrro <- robust(m2lrr, cluster=id2 , clubSandwich=TRUE)
m2lrro

Overall <- rma.mv(LRRyi, LRRvi,
                random = list(~1|id2, ~1|Study),
                data = meta1)

overallo <- robust(Overall, cluster=id2 , clubSandwich=TRUE)
overallo


##Set up emmeans reference grid
m2glrr <- qdrg(object = m2lrrro, data = meta1)


m2emlrr <- emmeans(m2glrr, ~ index.effect)
emmeans:::cld.emmGrid(m2emlrr)

figdat_KBFlrr <- data.frame(m2emlrr) %>%
  filter(index.effect != "")
figdat_KBFlrr$grouping = c("")
figdat_KBFlrr$EffLab = c(1,2,3,4,5,6,7)


#create a new column for KBF labels used in figure 
figdat_KBFlrr$Key.Biological.Factor2 <- c(
  "Behaviour", 
  "Development", 
  "Fecundity",
  "Feeding",
  "Growth",
  "Health",
  "Survival")


figdat_KBFlrr$k = (meta1 %>%
                     filter(!is.na(LRRyi) & !is.na(LRRvi)) %>%
                     group_by(index.effect) %>%
                     summarize(count = n()))$count

figdat_KBFlrr$n = (meta1 %>%
                     filter(!is.na(LRRyi) & !is.na(LRRvi)) %>%
                     group_by(index.effect) %>%
                     summarize(count = length(unique(id2))))$count


figdat_KBFlrr$KBF_xlab <- paste(figdat_KBFlrr$Key.Biological.Factor2, "\n(n = ", 
                                figdat_KBFlrr$n, ", k = ",
                                figdat_KBFlrr$k, ")", sep = "")





#make the plot
LRR3A <- ggplot(figdat_KBFlrr, aes(x = KBF_xlab, 
                                   y = emmean,
                                   color = Key.Biological.Factor2)) +
  geom_hline(yintercept = 0,                   #make a reference line at 0
             linetype = "dashed", 
             color = "black", 
             linewidth = 1) +
  geom_point(size = 3) +                               #plot points
  geom_errorbar(aes(ymin = lower.CL,        #include error bars
                    ymax = upper.CL),     
                width=0.25,
                size = 1) +                   #adjust width of bar
  geom_text(aes(label = grouping,
                x = EffLab),
            color = "black",
            position = position_nudge(x = 0.25,  y = 0.025))+
  # scale_y_continuous(limits = c(-0.275,1.05),      #change limits of Y axis
  #                    breaks = seq(-0.25,1,0.25))+      #set Y axis breaks
  
  scale_color_manual(values = c("#5E976E","#cad76c","#976543",
    "#58355E",
    "#FFCA3A",
    "#EC0B43",
    "#63ADF2"))+
  xlab("Key Biological") +               #relabel X and Y axes
  ylab("Log Response Ratio") +
  scale_x_discrete(limits = rev(levels(as.factor(figdat_KBFlrr$KBF_xlab))))+
  coord_flip()+
  theme_JR()+                                   #call your theme
  theme(legend.position = "none",
        axis.title.y = element_blank())

LRR3A

#end.points  
###Need to remove some variables, due to lack of replication
## Drop Fungicide & Sulfur containing
variable_counts <- meta1 %>%
  group_by(End.Point) %>%
  tally(name = "count")

# Filter out the variables with fewer than 10 occurrences
filtered_data <- meta1 %>%
  semi_join(variable_counts %>% filter(count >= 10), by = "End.Point")

sort(unique(filtered_data$End.Point))
sort(unique(filtered_data$index.effect))

mKBFactor_sublrr <- rma.mv(LRRyi, LRRvic,
                           mods = ~  End.Point - 1,
                           random = list(~1|id2, ~1|Study),
                           data = filtered_data)

mKBFactor_sublrrro <- robust(mKBFactor_sublrr, cluster=id2, clubSandwich=TRUE)
mKBFactor_sublrrro


figdat_sublrr <- data.frame(est = mKBFactor_sublrrro$beta,
                            ci.low = mKBFactor_sublrrro$ci.lb,
                            ci.up = mKBFactor_sublrrro$ci.ub,
                            KBF = row.names(mKBFactor_sublrrro$beta))





figdat_sublrr$End.Point = as.factor(gsub('End.Point', '', figdat_sublrr$KBF))

figdat_sublrr$KBF = fct_rev(filtered_data$index.effect[match(figdat_sublrr$End.Point,
                                                                    filtered_data$End.Point)])

#
figdat_sublrr$KBF = fct_rev(figdat_sublrr$KBF)

figdat_sublrr$End.Point <- str_to_sentence(figdat_sublrr$End.Point)

figdat_sublrr$End.Point[1] = "Anti-Oxidant"
figdat_sublrr$End.Point[2] = "Body length"
figdat_sublrr$End.Point[3] = "Body weight"
figdat_sublrr$End.Point[4] = "Climbing Activity"
figdat_sublrr$End.Point[5] = "D-Glucose content"
figdat_sublrr$End.Point[6] = "Emergence ratio"
figdat_sublrr$End.Point[7] = "Emerging time"
figdat_sublrr$End.Point[8] = "Food intake"
figdat_sublrr$End.Point[9] = "GSH"
figdat_sublrr$End.Point[10] = "Locomotory activity"
figdat_sublrr$End.Point[11] = "MDA"
figdat_sublrr$End.Point[12] = "Number of eggs"
figdat_sublrr$End.Point[13] = "Protein Content"
figdat_sublrr$End.Point[14] = "ROS"
figdat_sublrr$End.Point[15] = "Shannon Diversity"
figdat_sublrr$End.Point[16] = "Simphson Diversity"
figdat_sublrr$End.Point[17] = "Sleep time"
figdat_sublrr$End.Point[18] = "SOD"
figdat_sublrr$End.Point[19] = "Stage Duration"
figdat_sublrr$End.Point[20] = "Survival"
figdat_sublrr$End.Point[21] = "TG Content"



figdat_sublrr$End.Point = as.factor(figdat_sublrr$End.Point)

subknlrr = (filtered_data %>%
              group_by(End.Point) %>%
              summarize(kcount = n(),
                        ncount = length(unique(id2))))

figdat_sublrr$n = subknlrr$kcount[c(1,2,4,5,3,6:17,18,19,21,20)]
figdat_sublrr$k = subknlrr$kcount[c(1,2,4,5,3,6:17,18,19,21,20)]

figdat_sublrr$KBF_xlab <- as.factor(paste(figdat_sublrr$End.Point, "\n(n = ", 
                                          figdat_sublrr$n, ", k = ",
                                          figdat_sublrr$k, ")", sep = ""))
# 


figdat_sublrr$KBF_xlab = factor(figdat_sublrr$KBF_xlab,
                                levels(figdat_sublrr$KBF_xlab)[c(2,3,4,10,17,6,7,19,8,12,20,1,5,9,11,13,14,18,21,15,16)])





figdat_sublrr$KBF = factor(figdat_sublrr$KBF,
                         levels(figdat_sublrr$KBF)[c(1,2,3,4,5,7,6)])





#make the plot
LRR3B <- ggplot(figdat_sublrr, aes(x = KBF_xlab, 
                                   y=est,
                                   color = KBF)) +            
  geom_hline(yintercept = 0,                   #make a reference line at 0
             linetype = "dashed", 
             color = "black", 
             size = 1) +
  geom_point(size = 3) +                               #plot points
  geom_errorbar(aes(ymin = ci.low,        #include error bars
                    ymax = ci.up),     
                width=0.33,
                linewidth = 0.6) +                   #adjust width of bar
  scale_y_continuous(limits = c(-19,16),      #change limits of Y axis
                     breaks = seq(-18,15,3))+      #set Y axis breaks
  xlab("") +               #relabel X and Y axes
  ylab("Log Response Ratio") +
  scale_color_manual(values = c("#5E976E","#222081",
                                "#b64873","#FFCA3A",
                                "#58355E","#EC0B43",
                                "#63ADF2"))+
  coord_flip()+
  scale_x_discrete(limits = rev(levels(figdat_sublrr$KBF_xlab)))+
  theme_JR()+                                   #call your theme
  theme(legend.position = "none",
        axis.text.y = element_text(size = 9),
        axis.title.y = element_blank())

LRR3B


LRR3_WHOLE = cowplot::align_plots(LRR3A,
                                  LRR3B,
                                  align = 'hv', axis = 'l')
LRR3_fig = cowplot::plot_grid(LRR3_WHOLE[[1]], 
                              LRR3_WHOLE[[2]], 
                              labels = c("A)","B)"),
                              ncol = 1,
                              rel_heights = c(0.5,1),
                              label_x = 0,
                              label_y = 0.975)
LRR3_fig



ggsave("./LRR.tiff",
       dpi = 400,
       width = 10,
       height = 16,
       units = "in")




