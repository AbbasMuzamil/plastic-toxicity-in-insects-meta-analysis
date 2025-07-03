library(metafor)
library(MuMIn)
library(tidyverse)
library(multcomp)
library(emmeans)
library(ggeffects)
library(forcats)
library(ggplot2)
##Make ggplot theme to use throughout
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


#############input_Data###############


meta1<- read_excel(file.choose(), sheet = 1, col_names = T)

############################
library(car)
library(lme4)
library(ggeffects)

##########################################


# Assuming your data frame is named 'meta1'
meta1$Sample_Size <- meta1$Tn + meta1$Cn



ste <- function(x) sd(x, na.rm = T)/sqrt(length(x))


data.frame(meta1 %>%
             group_by(index.effect) %>%
             summarize(meanSS = mean(Sample_Size, na.rm = T),
                       seSS = ste(Sample_Size)))


meta1$SE_meta <- sqrt(meta1$SMDHvic)
SEmod <- lmer(SE_meta ~ index.effect + (1|id2),
              data = meta1)
emmeans:::cld.emmGrid(emmeans(SEmod, specs = ~ index.effect))

SEfigdat <- data.frame(ggemmeans(SEmod, terms = "index.effect"))
SEfigdat$index.effect = c("Fecundity",
                                  "Feeding",
                                  "Behaviour Response",
                                  "Growth",
                                  "Survival Rate","Health", "Development")
SEfigdat$index.effect = as.factor(SEfigdat$index.effect)
# SEfigdat$Global.Change.Driver = factor(SEfigdat$Global.Change.Driver,
#                                         levels(SEfigdat$Global.Change.Driver)[c(1,3,2,4,5)])
SEfigdat$grouping = c("")
SEfigdat$EffLab = c(5,3,4,2,1,6,7)



SEfig <- ggplot(SEfigdat, aes(x = forcats::fct_rev(index.effect),
                              y = predicted, color = index.effect))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, size = 1)+
  geom_point(size = 3)+
  geom_text(aes(label = grouping,
                x = EffLab),
            color = "black",
            position = position_nudge(x = 0.25)
  )+
  scale_y_continuous(limits = c(0.2,2),
                     breaks = c(0.2,0.5,0.8,1.1,1.4,1.7,2))+
  scale_color_manual(values = c("#5E976E",
                                "#58355E",
                                "#FFCA3A",
                                "#EC0B43",
                                "#63ADF2",
                                "#7a6382",
                                "#9b8720"))+
  xlab("Biological Traits") +               #relabel X and Y axes
  ylab("Standard error\nof effect size") +
  coord_flip()+
  theme_JR()+                                   #call your theme
  theme(legend.position = "none")

SEfig



SSmod <- glmer(Sample_Size ~ index.effect + (1|id2),
               family = poisson, data = meta1)
emmeans:::cld.emmGrid(emmeans(SSmod, specs = ~ index.effect))

SSfigdat <- data.frame(ggemmeans(SSmod, terms = "index.effect"))

SSfigdat$index.effect = c("Fecundity",
                          "Feeding",
                          "Behaviour Response",
                          "Growth",
                          "Survival Rate","Health", "Development")
SSfigdat$index.effect = as.factor(SSfigdat$index.effect)
# SEfigdat$index.effect = factor(SEfigdat$index.effect,
#                                         levels(SEfigdat$index.effect)[c(1,3,2,4,5,6,7)])
SSfigdat$grouping = c("")
SSfigdat$EffLab = c(5,3,4,2,1,6,7)

SSfig <- ggplot(SSfigdat, aes(x = forcats::fct_rev(index.effect),
                              y = predicted, color = index.effect))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, size = 1)+
  geom_point(size = 3)+
    scale_y_continuous(trans = "log", breaks = c(5,10,15,20))+
  scale_color_manual(values = c("#5E976E",
                                "#58355E",
                                "#FFCA3A",
                                "#EC0B43",
                                "#63ADF2",
                                "#7a6382",
                                "#9b8720"))+
  xlab("Biological Traits") +               #relabel X and Y axes
  ylab("Sample size\nper effect size") +
  coord_flip()+
  theme_JR()+                                   #call your theme
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())


SSfig



SESS_whole = cowplot::align_plots(SEfig, SSfig,
                                  align = 'h', axis = 'l')

cowplot::plot_grid(SESS_whole[[1]], SESS_whole[[2]],
                   labels = c("A)","B)"),
                   ncol = 2,
                   rel_widths = c(1,0.5),
                   label_x = c(0.89,0.775),
                   label_y = 0.925)

ggsave("./SS_SE_KBF.tiff",
       dpi = 600,
       width = 10,
       height = 7,
       units = "in")





#####Summary stats
##Table s2
##Groupings within global change drivers




data.frame(meta1 %>%
             group_by(Species,
                      index.effect) %>%
             summarize(studies = length(unique(id2)),
                       count = n()))

data.frame(meta1 %>%
             filter(!is.na(Type) & Type != "other" ) %>%
             group_by(Type,
                      index.effect) %>%
             summarize(studies = length(unique(id2)),
                       count = n()))

data.frame(meta1 %>%
             group_by(Country,
                      index.effect) %>%
             summarize(studies = length(unique(id2)),
                       count = n()))


data.frame(meta1 %>%
             group_by(Sex,
                      index.effect) %>%
             summarize(studies = length(unique(id2)),
                       count = n()))



data.frame(meta1 %>%
             group_by(Stage,
                      index.effect) %>%
             summarize(studies = length(unique(id2)),
                       count = n()))



#Polymer Types









data.frame(meta1 %>%
             group_by(Polymer.types,
                      index.effect) %>%
             summarize(studies = length(unique(id2)),
                       count = n()))





# Convert Concentration to a factor if it isn't already
meta1$Concentration <- as.factor(meta1$Concentration)

# Summarize the data by Concentration and index.effect
data.frame(meta1 %>%
             group_by(Concentration,
                      index.effect) %>%
             summarize(studies = length(unique(id2)),
                       count = n()))






data.frame(meta1 %>%
             group_by(Exposure.duration,
                      index.effect) %>%
             summarize(studies = length(unique(id2)),
                       count = n()))







unique_sizes <- unique(meta1$Plastic.Size)
print(unique_sizes)







data.frame(meta1 %>%
             group_by(Plastic.Size,
                      index.effect) %>%
             summarize(studies = length(unique(id2)),
                       count = n()))









##ANY OTHER DESCRIPTIVE STATS

##Total number of studies
length(unique(meta1$id2))
##82 studies - note 30 studies are from Mitchell et al (unique citation number for each family)
## so 1002 - 30 = 972
##Total Number of Effect Sizes
nrow(meta1)
##703 observations









##Distribution of data across various contexts
##Distribution across host taxa
##Distribution across parasite taxa
##Distribution across habitats
##Distribution across drivers
##Distribution across observational/experimental

#Plastic Types
Plasticplot = meta1 %>%
  filter(complete.cases(Type)) %>%
  ggplot(aes(x = Type))+
  scale_y_continuous(limits = c(0, 600),
                     breaks = seq(0, 1000, 100),
                     expand = c(0, 0))+
  geom_bar(color = "black", fill = "grey43", width = 0.5)+
  ylab("Number of effect sizes")+
  xlab("Plastic Types")+
  theme_JR()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.y = element_blank())

Plasticplot


meta1$index.effect2 <- factor(meta1$index.effect)

meta1$index.effect2 <- factor(meta1$index.effect2,
                                         levels(meta1$index.effect2)[c(1,3,2,4,5,6,7)])

#Global change driver
gcdplot = ggplot(meta1, aes(x = index.effect2))+
  scale_y_continuous(limits = c(0, 500),
                     breaks = seq(0,700,100),
                     expand = c(0, 0))+
  scale_x_discrete(labels = c("Behaviour",
                              "Development",
                              "Fecundity",
                              "Growth",
                              "Feeding","Health", "Survival"))+
  geom_bar(aes(fill = index.effect),
           color = "black", width = 0.5)+
  scale_fill_manual(values = c("#5E976E",
                               "#FFCA3A",
                               "#58355E",
                               "#EC0B43",
                               "#63ADF2",
                               "#d78320",
                               "#62d182"))+
  ylab("Number of effect sizes")+
  xlab("Biological Treats")+
  theme_JR()+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.8,vjust = 0.9),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.position = "none")


gcdplot



#SEX
##Insect Sex
datSex = meta1 %>%
  filter(!is.na(Sex)) %>%
  filter(!str_detect(Sex, "-"))

SEXplot = ggplot(datSex, aes(x = Sex)) +
  scale_y_continuous(limits = c(0, 200),
                     breaks = seq(0, 1500, 25),
                     expand = c(0, 0)) +
  geom_bar(color = "black", fill = "grey43", width = 0.5) +
  ylab("Number of effect sizes") +
  xlab("Sex") +
  theme_JR() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))


SEXplot


##Insect Stage 
datStag =meta1 %>%
  filter(!(Stage %in% c("Pre-Pupae", "Pupae", "Egg", "Brood", "Cocoon", "larvae"))) 

Stageplot = ggplot(datStag, aes(x = Stage)) +
  scale_y_continuous(limits = c(0, 500),
                     breaks = seq(0, 2500, 100),
                     expand = c(0, 0)) +
  geom_bar(color = "black", fill = "grey43", width = 0.5) +
  ylab("Number of effect sizes") +
  xlab("Stage") +
    theme_JR()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          axis.title.y = element_blank())

Stageplot






###Polymer Type
datPoly = meta1 %>%
  filter(!is.na(Polymer.types))



Polymertypesplot = ggplot(datPoly, aes(x = Polymer.types)) +
  scale_y_continuous(limits = c(0, 400),
                     breaks = seq(0, 1500, 100),
                     expand = c(0, 0)) +
  geom_bar(color = "black", fill = "grey43", width = 0.5) +
  ylab("Number of effect sizes") +
  xlab("Polymer Types") +
  theme_JR() +
  theme( axis.text.x = element_text(angle = 30, hjust = 0.8,vjust = 0.9),
    axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

# Display the plot
Polymertypesplot





##Plastic Concentration
datConcen = meta1 %>%
  filter(!is.na(Concentration))


Concentationplot = ggplot(datConcen, aes(x = Concentration)) +
    scale_y_continuous(limits = c(0, 500),
                     breaks = seq(0, 1000, 100),
                     expand = c(0, 0)) +
  geom_bar(color = "black", fill = "grey43", width = 0.5) +
  #scale_x_discrete(labels = levels(Concentration$Concentration)) +  # Use levels of Concentration as labels
  ylab("") +
  xlab("Concentration") +
  theme_JR() +
  theme(axis.title = element_text(size = 15))


Concentationplot

fig1_WHOLE = cowplot::align_plots(gcdplot, SEXplot, Stageplot,
                                  Plasticplot, Concentationplot, Polymertypesplot,
                                  align = 'hv', axis = 'l')

figure1 = cowplot::plot_grid(fig1_WHOLE[[1]], fig1_WHOLE[[2]], fig1_WHOLE[[3]],
                             fig1_WHOLE[[4]], fig1_WHOLE[[5]], fig1_WHOLE[[6]],
                             labels = c("A)","B)","C)","D)","E)", "F)"),
                             ncol = 3,
                             label_x = 0.13,
                             label_y = 0.975)




figure1



ggsave("./SummaryFig.tiff",
       figure1,
       dpi = 600,
       width = 12,
       height = 6.5,
       scale = 1.25,
       units = "in")



####################Time Exposure ###########################




###Exposure Duration
datExpos = meta1 %>%
  filter(!is.na(Exposure.duration))

Exposuredurationplot = ggplot(datExpos, aes(x = Exposure.duration)) +
  scale_y_continuous(limits = c(0, 400),
                     breaks = seq(0, 400, 100),
                     expand = c(0, 0)) +
  geom_bar(color = "black", fill = "grey43", width = 0.5) +
  ylab("Number of effect sizes") +
  xlab("Exposure Duration") +
  theme_JR() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))


Exposuredurationplot


##Insect Species

datSpec = meta1 %>%
  filter(!is.na(Species)) 

Speciesplot = ggplot(datSpec, aes(x = Species)) +
  scale_y_continuous(limits = c(0, 200),
                     breaks = seq(0, 1500, 100),
                     expand = c(0, 0)) +
  geom_bar(color = "black", fill = "grey43", width = 0.5) +
  ylab("Number of effect sizes") +
  xlab("Insect species") +
  theme_JR()+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.8,vjust = 0.9),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.position = "none")

Speciesplot


##Insect Order

datOrd <- meta1 %>%
  filter(!is.na(Order)) 

Orderplot = ggplot(datOrd, aes(x = Order)) +
  scale_y_continuous(limits = c(0, 600),
                     breaks = seq(0, 1500, 100),
                     expand = c(0, 0)) +
  geom_bar(color = "black", fill = "grey43", width = 0.5) +
  ylab("Number of effect sizes") +
  xlab("Insect Order") +
  theme_JR() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.8,vjust = 0.9),
    axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

Orderplot


##Insect Families

datFamil = meta1 %>%
  filter(!is.na(Family))

Familyplot = ggplot(datFamil, aes(x = Family)) +
  scale_y_continuous(limits = c(0, 300),
                     breaks = seq(0, 1000, 100),
                     expand = c(0, 0)) +
  geom_bar(color = "black", fill = "grey43", width = 0.5) +
  ylab("Number of effect sizes") +
  xlab("Insect Families") +
  theme_JR()+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.8,vjust = 0.9),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.position = "none")

Familyplot




contdat <- meta1 %>%
  filter(!is.na(Country))

Countryplot = ggplot(contdat, aes(x = Country)) +
  scale_y_continuous(limits = c(0, 400),
                     breaks = seq(0, 500, 50),
                     expand = c(0, 0)) +
  geom_bar(color = "black", fill = "grey43", width = 0.5) +
  ylab("Number of effect sizes") +
  xlab("Country") +
  theme_JR()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8,vjust = 0.9),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.position = "none")

Countryplot





###Plastic Size 
datSize = meta1 %>%
  filter(!is.na(Plastic.Size))

Plastic.sizeplot = ggplot(datSize, aes(x = Plastic.Size)) +
  scale_y_continuous(limits = c(0, 400),
                     breaks = seq(0, 1000, 50),
                     expand = c(0, 0)) +
  geom_bar(color = "black", fill = "grey43", width = 0.5) +
  ylab("Number of effect sizes") +
  xlab("Plastic Size") +
  theme_JR() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

Plastic.sizeplot





figsp_WHOLE = cowplot::align_plots(Exposuredurationplot, Speciesplot,
                                   Orderplot, Familyplot,
                                   Countryplot, Plastic.sizeplot,
                                   align = 'hv', axis = 'l')
figureSpara = cowplot::plot_grid(figsp_WHOLE[[1]], figsp_WHOLE[[2]], figsp_WHOLE[[3]],
                                 figsp_WHOLE[[4]], figsp_WHOLE[[5]], figsp_WHOLE[[6]],
                                 labels = c("A)","B)","C)","D)","E)","F)"),
                                 ncol = 3,
                                 label_x = 0.10,
                                 label_y = 0.975)



figureSpara

ggsave("./SummaryFigureS4.tiff",
       figureSpara,
       dpi = 600,
       width = 16,
       height = 6.25,
       scale = 1.25,
       units = "in")





