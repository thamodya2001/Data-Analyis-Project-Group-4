
data <- read.csv("C:/Users/Lakmini/Downloads/dermatology_database_1.csv")
#View(dermatology_database_1)


summary(data)
sum(is.na(data))
data=data[,-35]
data
library(FactoMineR)
library(factoextra)

famd_result <- FAMD(data, graph = FALSE)  # Perform FAMD without displaying default graphs

fviz_screeplot(famd_result, addlabels = TRUE)

fviz_famd_ind(famd_result, repel = TRUE,label=FALSE )
fviz_famd_var(famd_result, repel = TRUE,col.var = "darkblue")
              
fviz_contrib(famd_result, "var", axes = 1)

##########################################################
# Polychornic correlation matrix for ordinal data 
#############################################################
names(data)
install.packages("psych")
library(psych)

data=data[,-c(34,35)]
head(data)
plyres=polychoric(data)$rho
plyres

#install.packages("corrplot")
library(corrplot)
# Step 3: Visualize using corrplot
corrplot(
  plyres,
  method = "color",               # use colored squares
  type = "upper",                 # show only upper triangle
  order = "hclust",               # cluster similar variables
  tl.col = "black",               # text label color
  tl.cex = 0.5,                   # text label size
  addCoef.col = "black",          # show correlation values
  number.cex = 0.3,               # size of numbers inside tiles
  col = colorRampPalette(c("blue", "white", "red"))(200), # color scale
  title = "Polychoric Correlation Heatmap",
  mar = c(0, 0, 5, 5)             # margin to fit title
)


##########################################################################


# Load libraries
library(FactoMineR)
library(factoextra)

# Run FAMD on your data
famd_result <- FAMD(data, graph = FALSE)

# 1. Scree plot: Explained variance by dimensions
fviz_screeplot(famd_result, 
               addlabels = TRUE,
               barfill = "skyblue", 
               barcolor = "black", 
               linecolor = "red") +
  ggtitle("Scree Plot - Explained Variance")

# 2. Individuals plot (without labels)
fviz_famd_ind(famd_result, 
              repel = TRUE,
              label = FALSE,
              geom = "point",
              col.ind = "darkgreen") +
  ggtitle("FAMD - Individuals")

# 3. Variables (loading) plot with customized color
fviz_famd_var(famd_result, 
              repel = TRUE,
              col.var = "darkblue") +
  ggtitle("FAMD - Variables (Loadings)")

# Optional: Gradient color based on contribution
# fviz_famd_var(famd_result, 
#               repel = TRUE, 
#               col.var = "contrib",
#               gradient.cols = c("lightblue", "blue", "red"))

# 4. Variable contribution to Dim 1
fviz_contrib(famd_result, 
             choice = "var", 
             axes = 1, 
             top = 15, 
             fill = "steelblue",
             color = "steelblue") +
  ggtitle("Contribution of Variables to Dimension 1")


library(FactoMineR)
library(factoextra)
library(dplyr)

# Run FAMD
famd_result <- FAMD(data, graph = FALSE)

# Get variable contributions
var_contrib <- get_famd_var(famd_result)$contrib

# Set number of top contributors to color
top_n <- 5  # You can increase this if needed

# Identify top variables for Dimension 1 and 2
top_dim1 <- rownames(var_contrib)[order(var_contrib[, 1], decreasing = TRUE)][1:top_n]
top_dim2 <- rownames(var_contrib)[order(var_contrib[, 2], decreasing = TRUE)][1:top_n]

# Create a color vector for all variables (default: grey)
var_colors <- rep("grey70", nrow(var_contrib))
names(var_colors) <- rownames(var_contrib)

# Assign red to top contributors for Dim 1
var_colors[top_dim1] <- "red"

# Assign blue to top contributors for Dim 2 (overwrites red if overlapping)
var_colors[top_dim2] <- "blue"

# Visualize variable loadings with custom colors
fviz_famd_var(famd_result,
              repel = TRUE,
              col.var = var_colors) +
  ggtitle("FAMD - Top Contributors Colored by Dimension")

