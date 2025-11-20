#Author: Ximena Leon

################################################################################
################################################################################
################################################################################
################# Combining all the song features to assess if #################
############# overall song similarity correlates with geo distance #############
################################################################################

################################################################################
############################# SPOTTED + EASTERN ################################
################################################################################
library(geodist)
library(vegan)

TowheeData <- read.csv("~/PUBLISH_ALL_FINAL_TowheeResultsData_CLEANED.csv")

#to log transform:
TowheeData[,c(7:14)] <- log(TowheeData[,c(7:14)]) 

#to do z-score transformation
TowheeData[,c(7:14)] <- scale(TowheeData[,c(7:14)])

coords <- TowheeData[,c(6,5)]
colnames(coords) <- c("longitude", "latitude")
geo_dist = geodist(coords, measure = "geodesic") #matrix should be same as syll dist
geo_dist <- as.dist(geo_dist)

songdist = dist(TowheeData[,c(7:14)], method = "euclidean")

#each point is the geographic distance between two points
#and I am plotting the geographical distance of those points against the syllable distance (so how different the syllables are)
#testdist=cbind(geo_dist, songdist)
#plot(geo_dist, songdist)
#modelresults=lm(songdist~geo_dist,as.data.frame(testdist))
#summary(modelresults)
#abline(modelresults, col = "red")

#mantel test of matrices 
#library(ade4)
#mantel.rtest(geo_dist, songdist, nrepet = 999)

mantel_res2 <- vegan::mantel(geo_dist, songdist, method = "pearson", perm = 999)

results2 <- data.frame(
  Mantel_r = NA,
  P_value = NA
)

results2$Mantel_r <- mantel_res2$statistic
results2$P_value <- mantel_res2$signif


IBD_SongDistanceModelResults <- results2
#write.csv(IBD_SongDistanceModelResults, row.names = FALSE, "~GeoDist_SongDistanceModelResults_SpottedandEasternCombined.csv")


################################################################################
########################## EACH SPECIES SEPARATELY #############################
################################################################################
library(geodist)
library(vegan)

# Load and preprocess
TowheeData <- read.csv("~/PUBLISH_ALL_FINAL_TowheeResultsData_CLEANED.csv")

#choose the species #repeat for Eastern towhee
TowheeDataSpecies <- TowheeData[TowheeData$Species == "Pipilo maculatus", ] #Pipilo maculatus #Pipilo erythrophthalmus

#to log transform:
TowheeDataSpecies[,c(7:14)] <- log(TowheeDataSpecies[,c(7:14)]) 

#to do z-score transformation
TowheeDataSpecies[,c(7:14)] <- scale(TowheeDataSpecies[,c(7:14)])

coords <- TowheeDataSpecies[,c(6,5)]
colnames(coords) <- c("longitude", "latitude")
geo_dist = geodist(coords, measure = "geodesic") #matrix should be same as syll dist
geo_dist <- as.dist(geo_dist)

songdist = dist(TowheeDataSpecies[,c(7:14)], method = "euclidean")

mantel_res2 <- vegan::mantel(geo_dist, songdist, method = "pearson", perm = 999)

results2 <- data.frame(
  Mantel_r = NA,
  P_value = NA
)

results2$Mantel_r <- mantel_res2$statistic
results2$P_value <- mantel_res2$signif


IBD_SongDistanceModelResults <- results2
#write.csv(IBD_SongDistanceModelResults, row.names = FALSE, "~/GeoDist_SongDistanceModelResults_SpottedTowheeONLY.csv")
#repeat for Eastern towhee
################################################################################