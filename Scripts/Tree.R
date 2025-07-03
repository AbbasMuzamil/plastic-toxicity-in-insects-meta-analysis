packages <- c("rotl", "ape", "devtools", "stringr")
install.packages(packages)
install.packages("shiny")
library(shiny)
library(rotl)
library(devtools)
library(stringr)
options(download.file.method = "libcurl")  # or "wget" or "curl"
devtools::install_github("phylotastic/datelife")
devtools::install_github("phylotastic/datelifeplot")
library(datelife)
library(datelifeplot)


# Correct usage with a single vector of names
resolved_name <- rotl::tnrs_match_names(names = c("Aedes aegypti", "Aedes albopictus", 
                                                  "Apis mellifera", "Bombyx mori", 
                                                  "Bradysia difformis", "Chironomus kiinensis", 
                                                  "Chironomus riparius", "Chironomus sancticaroli", 
                                                  "Chironomus tepperi", "Culex pipiens", 
                                                  "Culex tarsalis", "Drosophila melanogaster", 
                                                  "Hermetia illucens", "Lepidostoma basale", 
                                                  "Tenebrio molitor"))

resolved_name

# Get OTT IDs
ott_ids <- ott_id(resolved_name)

# Filter out invalid OTT IDs (You can also print to check)
valid_ott_ids <- ott_ids[!is.na(ott_ids) & ott_ids != ""]

# Generate the tree with valid OTT IDs
if(length(valid_ott_ids) > 0) {
  tree <- tol_induced_subtree(ott_ids = valid_ott_ids)
  plot(tree, cex = .8, label.offset = .1, no.margin = TRUE)
} else {
  print("No valid OTT IDs available for tree construction.")
}


# Check individual taxa
for(name in c("Aedes aegypti", "Aedes albopictus", "Apis mellifera", 
              "Bombyx mori", "Bradysia difformis", "Chironomus kiinensis", 
              "Chironomus riparius", "Chironomus sancticaroli", 
              "Chironomus tepperi", "Culex pipiens", "Culex tarsalis", 
              "Drosophila melanogaster", "Hermetia illucens", 
              "Lepidostoma basale", "Tenebrio molitor")) {
  result <- rotl::tnrs_match_names(names = name)
  print(result)
}




# Remove the problematic OTT ID (ott209377 for Chironomus riparius)
valid_ott_ids <- valid_ott_ids[valid_ott_ids != "209377"]

# Generate the tree with valid OTT IDs
if(length(valid_ott_ids) > 0) {
  tree <- tol_induced_subtree(ott_ids = valid_ott_ids)
  plot(tree, cex = .8, label.offset = .1, no.margin = TRUE)
} else {
  print("No valid OTT IDs available for tree construction.")
}


# Remove the OTT IDs from the labels
new_labels <- gsub("_ott[0-9]+$", "", tree$tip.label)
new_labels <- gsub("\\(species in domain Eukaryota\\)", "", new_labels)
# Replace underscores with spaces
new_labels <- gsub("_", " ", new_labels)

# Assign the cleaned labels back to the tree
tree$tip.label <- new_labels

# Plot the tree with the updated labels
plot(tree, cex = .8, label.offset = .1, no.margin = TRUE, lwd = 5)


# Open a TIFF device with specified parameters
tiff("Tree_Plot.tiff", width = 9.5, height = 9.5, units = "in", res = 600)

# Plot the tree
plot(tree, cex = 1.2, label.offset = .1, no.margin = TRUE)

# Close the TIFF device to save the file
dev.off()

