



#############Load the Packages and Library###############

install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)


#############input_Data###############

meta1<- read_excel(file.choose(), sheet = 1, col_names = T)





#############Add_Year###############
# Assuming your dataset is already loaded into a dataframe called df

# List of titles and corresponding years
title_year_mapping <- data.frame(
  title = c(
    "Adverse biological effects of ingested polystyrene microplastics using Drosophila melanogaster as a model in vivo organism",
    "Altered gene expression in Chironomus riparius (insecta) in response to tire rubber and polystyrene microplastics",
    "Biodegradation of Polystyrene by Dark (Tenebrio obscurus) and Yellow (Tenebrio molitor) Mealworms (Coleoptera: Tenebrionidae)",
    "Chronic Exposure to Polystyrene Microplastic Fragments Has No Effect on Apis mellifera Survival, but Reduces Feeding Rate and Body Weight",
    "Combined effects of polyethylene microplastics and natural stressors on Chironomus riparius life-history traits",
    "Differentially Charged Nanoplastics Induce Distinct Effects on the Growth and Gut of Benthic Insects (Chironomus kiinensis) via Charge-Specific Accumulation and Perturbation of the Gut Microbiota",
    "Effects of PET microplastics on the physiology of Drosophila melanogaster",
    "Effects of Polyurethane Small-Sized Microplastics in the Chironomid, Chironomus riparius: Responses at Organismal and Sub-Organismal Levels",
    "Evaluation of the potential toxicity of UV-weathered virgin polyamide microplastics to non-biting midge Chironomus riparius",
    "Examining effects of ontogenic microplastic transference on Culex pipiens mortality and adult weight",
    "Exposure to polystyrene microplastic beads causes sex-specific toxic effects in the model insect Drosophila melanogaster",
    "Gut microbiota protects Apis melliferas (Apis mellifera L.) against polystyrene microplastics exposure risks",
    "Ingestion and effects of polystyrene nanoparticles in the silkworm Bombyx mori",
    "Ingestion of small-sized and irregularly shaped polyethylene microplastics affect Chironomus riparius life-history traits",
    "Intake of polyamide microplastics affects the behavior and metabolism of Drosophila melanogaster",
    "Metabolomic responses in freshwater benthic invertebrate, Chironomus tepperi, exposed to polyethylene microplastics: A two-generational investigation",
    "Microplastic ingestion perturbs the microbiome of Aedes albopictus and Aedes aegypti",
    "Microplastic Polystyrene Ingestion Promotes the Susceptibility of Honeybee to Viral Infection",
    "Microplastics affected black soldier fly (Hermetia illucens) pupation and short chain fatty acids",
    "Microplastics alter behavioural responses of an insect herbivore to a plant-soil system",
    "Microplastics in freshwater and their effects on host microbiota",
    "Microplastics incorporated by honeybees from food are transferred to honey, wax and larvae",
    "Nanoplastics Affect the Bioaccumulation and Gut Toxicity of Emerging Perfluoroalkyl Acid Alternatives to Aquatic Insects (Chironomus kiinensis): Importance of Plastic Surface Charge",
    "Neuromuscular, retinal, and reproductive impact of low-dose polystyrene microplastics on Drosophila melanogaster",
    "Effect of Realistic Microplastic Exposure on Growth and Development of Wild-caught Culex (Diptera: Culicidae) Mosquitoe",
    "Ontogenetic Transfer of Microplastics in Bloodsucking Mosquitoes Aedes aegypti L. (Diptera: Culicidae) Is a Potential Pathway for Particle Distribution in the Environment",
    "Photoaging of biodegradable nanoplastics regulates their toxicity to aquatic insects (Chironomus kiinensis) by impairing gut and disrupting intestinal microbiota",
    "Polyethylene microplastics and substrate availability can affect emergence responses of the freshwater insect Chironomus sancticaroli",
    "Polypropylene microplastics affect the physiology in Drosophila melanogaster model",
    "PVC and PET microplastics in caddisfly (Lepidostoma basale) cases reduce case stability",
    "Size-dependent and sex-specific negative effects of micro- and nano-sized polystyrene particles in the terrestrial invertebrate model Drosophila melanogaster",
    "The hazardous impact of true-to-life PET nanoplastics in Drosophila melanogaster",
    "The influence of microplastics on trophic interaction strengths and oviposition preferences of dipterans",
    "Toxic effects of acute exposure to polystyrene microplastics and nanoplastics on the model insect, silkworm Bombyx mori",
    "Transgenerational effects on development following microplastic exposure in Drosophila melanogaster",
    "Unveiling the residual plastics and produced toxicity during biodegradation of polyethylene (PE), polystyrene (PS), and Polyvinyl chloride (PVC) microplastics by mealworms (Larvae of Tenebrio molitor)"
  ),
  year = c(
    2021, 2021, 2019, 2023, 2022, 2023, 2021, 2022, 2021, 2018, 2023, 2020, 2020, 2019, 2022, 2023, 2023, 2021, 2021, 2021, 2022, 2023, 2024, 2021, 2023, 2022, 2024, 2022, 2023, 2020, 2023, 2022, 2018, 2021, 2021, 2023
  )
)

# Join the meta1 dataframe with the title_year_mapping dataframe on the title column
meta1 <- meta1 %>%
  left_join(title_year_mapping, by = "title")

# Now meta1 should have an additional column 'Year' with the respective year values.

#############Find_Sample_size################### 



# Assuming your data frame is named 'meta1'
meta1$Sample_size <- meta1$Tn + meta1$Cn



#############Add the Coloum Family and Oredr in data################### 
# Ensure there is no extra whitespace in species names in the meta1 file
meta1$Species <- trimws(meta1$Species)

# Update the species vector to ensure no whitespace issues
species <- trimws(c("Aedes aegypti", "Aedes albopictus", "Apis mellifera", 
                    "Bombyx mori", "Bradysia difformis", "Chironomus kiinensis", 
                    "Chironomus riparius", "Chironomus sancticaroli", 
                    "Chironomus tepperi", "Culex pipiens", "Culex tarsalis", 
                    "Drosophila melanogaster", "Hermetia illucens", 
                    "Lepidostoma basale", "Tenebrio molitor", "Water boatmen"))

family <- c("Culicidae", "Culicidae", "Apidae", "Bombycidae", 
            "Sciaridae", "Chironomidae", "Chironomidae", "Chironomidae", 
            "Chironomidae", "Culicidae", "Culicidae", "Drosophilidae", 
            "Stratiomyidae", "Lepidostomatidae", "Tenebrionidae", 
            "Corixidae")

order <- c("Diptera", "Diptera", "Hymenoptera", "Lepidoptera", 
           "Diptera", "Diptera", "Diptera", "Diptera", "Diptera", 
           "Diptera", "Diptera", "Diptera", "Diptera", "Trichoptera", 
           "Coleoptera", "Hemiptera")


# Match species with family and order, and fill the columns
meta1$Family <- family[match(meta1$Species, species)]
meta1$Order <- order[match(meta1$Species, species)]



#############Add the Coloum Plastic.Size in data###################


#First replace the rang values 
# Define a list of search patterns and their replacements
# Filter out rows where 'Size' contains "-"

patterns <- c(
  "7.0[-–\u2013\u2014]?9.0[[:space:]]?μm",
  "0.4[-–\u2013\u2014]?0.6[[:space:]]?μm",
  "200[[:space:]]?[-–\u2013\u2014]?[[:space:]]?500[[:space:]]?μm",
  "4.8[-–\u2013\u2014]?5.8[[:space:]]?μm",
  "40[-–\u2013\u2014]?48[[:space:]]?µm",
  "125[-–\u2013\u2014]?250[[:space:]]?µm",
  "217\\.1[[:space:]]?±[[:space:]]?3\\.1[[:space:]]?nm",
  "182\\.5[[:space:]]?±[[:space:]]?2\\.8[[:space:]]?nm",
  "32[-–\u2013\u2014]?63[[:space:]]?µm",
  "63[-–\u2013\u2014]?250[[:space:]]?µm",
  "125[-–\u2013\u2014]?500[[:space:]]?µm",
  "\\(27[[:space:]]?±[[:space:]]?17[[:space:]]?μm",
  "\\(93[[:space:]]?±[[:space:]]?25[[:space:]]?μm",
  "50[-–\u2013\u2014]?100[[:space:]]?nm",
  "5[-–\u2013\u2014]?5.9[[:space:]]?μm",
  "1.0[-–\u2013\u2014]?1.9[[:space:]]?µm",
  "0.4[-–\u2013\u2014]?0.6[[:space:]]?µm",
  "2.0[[:space:]]?±[[:space:]]?0.2[[:space:]]?μm"
)

replacements <- c(
  "8 μm", "0.5 μm", "350 μm", "5.2 μm", "44 μm", "192 μm",
  "219 nm", "183 nm", "54 μm", "196 μm", "320 μm",
  "36 μm", "102 μm", "80 nm", "5.7 μm", "1.5 µm", "0.5 µm",
  "2.1 μm"
)
# Apply the replacement
for (i in seq_along(patterns)) {
  meta1 <- meta1 %>%
    mutate(Size = str_replace_all(Size, regex(patterns[i], ignore_case = TRUE), replacements[i]))
}


# Function to convert sizes to micrometers
convert_to_micrometers <- function(size) {
  if (str_detect(size, "nm")) {
    # Extract numeric value and convert from nm to µm
    size %>%
      str_extract("^[0-9]+\\.?[0-9]*") %>%
      as.numeric() %>%
      { . / 1000 } %>% 
      paste0(" µm")
  } else if (str_detect(size, "mm")) {
    # Extract numeric value and convert from mm to µm
    size %>%
      str_extract("^[0-9]+\\.?[0-9]*") %>%
      as.numeric() %>%
      { . * 1000 } %>%
      paste0(" µm")
  } else {
    # Return the size unchanged if it's already in µm
    size
  }
}

# Function to classify sizes
classify_size <- function(size) {
  size_value <- str_extract(size, "^[0-9]+\\.?[0-9]*") %>% as.numeric()
  
  if (is.na(size_value)) {
    return(NA)
  } else if (size_value < 3) {
    return("Small")
  } else if (size_value >= 3 & size_value < 40) {
    return("Medium")
  } else if (size_value >= 40) {
    return("Large")
  } else {
    return(NA)
  }
}

# Process the data
meta1 <- meta1 %>%
  mutate(Size = str_replace_all(Size, "µm", " μm")) %>%  # Ensure consistent formatting
  mutate(Size = sapply(Size, convert_to_micrometers)) %>% # Convert sizes to micrometers
  mutate(Plastic.Size = sapply(Size, classify_size))      # Classify sizes


#############Add the Coloum Exposure.time in data###################


# Function to convert exposure time to hours
convert_to_hours <- function(x) {
  if (grepl("day", x)) {
    # Extract the numeric part and multiply by 24 to convert days to hours
    hours <- as.numeric(gsub(" day", "", x)) * 24
  } else if (grepl("h", x)) {
    # Extract the numeric part for hours
    hours <- as.numeric(gsub(" h", "", x))
  } else {
    hours <- NA
  }
  return(paste(hours, "h"))
}

# Apply the function to the Exposure.time column
meta1$Exposure.time <- sapply(meta1$Exposure.time, convert_to_hours)

# Ensure the Exposure.time column is numeric by removing the "h" and converting to a number
meta1$Exposure.time.numeric <- as.numeric(gsub(" h", "", meta1$Exposure.time))

# Categorize the exposure time based on the specified conditions
meta1$Exposure.duration <- ifelse(meta1$Exposure.time.numeric < 96, "Short", 
                                  ifelse(meta1$Exposure.time.numeric >= 96 & meta1$Exposure.time.numeric < 336, "Middle", 
                                         ifelse(meta1$Exposure.time.numeric >= 336, "Long", NA)))


#############Add the Coloum Concentration in data###################


# Filtering, mutating, and converting the Dose.g/kg values to concentration categories
meta1 <- meta1 %>%
    mutate(Concentration = case_when(
    as.numeric(`Dose.g/kg`) < 1e-3 ~ "Low",      # Handle very small values as Low
    as.numeric(`Dose.g/kg`) <= 1 ~ "Low",        # Low if 1 or less
    as.numeric(`Dose.g/kg`) > 1 & as.numeric(`Dose.g/kg`) <= 10 ~ "Medium",  # Medium if between 1 and 10
    as.numeric(`Dose.g/kg`) > 10 ~ "High",       # High if greater than 10
    TRUE ~ NA_character_                         # Handle any other unexpected cases
  ))


#############Add the Coloum Polymer.types in data###################


# Define the mapping of original names to new names
polymer_mapping <- c("Polystyrene" = "PS",
                     "PS" = "PS",
                     "Polyurethane" = "PU",
                     "PU" = "PU",
                     "Polyamide" = "PA",
                     "PA" = "PA",
                     "HDPE" = "HDPE",
                     "Polyethylene terephthalate" = "PET",
                     "PET" = "PET",
                     "Polyvinyl chloride" = "PVC",
                     "PVC" = "PVC", 
                     "Polypropylene" = "PP",
                     "PP" = "PP" ,
                     "Polyethylene" = "PE",
                     "PE" = "PE",
                     "Polylactic acid" = "PLA",
                     "PLA" = "PLA", 
                     "polyester" = "PES",
                     "PES" = "PES")

# Filter and rename the polymers
meta1 <- meta1 %>%

  mutate(Polymer.types = polymer_mapping[Polymer.types])  # Rename the polymers

unique(meta1$Polymer.types)
