library(dplyr)

file.exists("customer_segmentation.csv")

# Membaca dataset
data <- read.csv("customer_segmentation.csv", header = TRUE)

# Mengambiln kolom kuantitatif
quantitative_cols <-  data[, c("Year_Birth", "Income", "Kidhome", "Teenhome", "Recency", 
                       "MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", 
                       "MntSweetProducts", "MntGoldProds", "NumDealsPurchases", 
                       "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases", 
                       "NumWebVisitsMonth", "Z_CostContact", "Z_Revenue")]

head(quantitative_cols)

#----------------------------------Pre-processing------------------------------------------------------
sum(is.na(quantitative_cols))
p <- ncol(quantitative_cols)

# Menangani nilai yang hilang (NA) dengan menggantinya dengan median setiap kolom
quantitative_cols <- quantitative_cols %>% 
  mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Cek kolom dengan standar deviasi nol
constant_cols <- names(quantitative_cols)[sapply(quantitative_cols, function(x) sd(x, na.rm = TRUE) == 0)]
print(constant_cols)
# Hapus kolom
quantitative_cols <- quantitative_cols[, !names(quantitative_cols) %in% constant_cols]
print(names(quantitative_cols))

#PLOT
quantitative_cols <- as.data.frame(quantitative_cols)
quantitative_cols_long <- quantitative_cols %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plot histogram dengan facet
ggplot(quantitative_cols_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribusi Data Kuantitatif", x = "Nilai", y = "Frekuensi")

library(ggplot2)
library(dplyr)
library(tidyr)

# Plot Proporsi Pembelian
spending_cols <- quantitative_cols %>%
  select(MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds)

total_spending <- total_spending %>%
  mutate(Percentage = (Total_Spending / sum(Total_Spending)) * 100)

ggplot(total_spending, aes(x = "", y = Total_Spending, fill = Product)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "black", size = 5) +
  theme_minimal() +
  labs(title = "Proporsi Pembelian per Kategori Produk") +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Pastel1")

# Plot Metode Pembelian
purchase_cols <- quantitative_cols %>%
  select(NumWebPurchases, NumCatalogPurchases, NumStorePurchases)

total_purchases <- purchase_cols %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Purchase_Method", values_to = "Total_Purchases")

total_purchases <- total_purchases %>%
  mutate(Percentage = (Total_Purchases / sum(Total_Purchases)) * 100)

ggplot(total_purchases, aes(x = "", y = Total_Purchases, fill = Purchase_Method)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  theme_minimal() +
  labs(title = "Proporsi Preferensi Tempat Belanja Pelanggan") +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Set2")


#Check KMO
library(psych)
print(psych::describe(mtcars)) 
r <- cor(quantitative_cols)
KMO(r)

#Bartlett Test
bartlett.test(quantitative_cols)

#----------------------------PCA----------------------------------------
summary(quantitative_cols)
#manual
scale_data = scale(quantitative_cols)
r = cov(scale_data)
##Menghitung eigenvalue dan eigenvector
pc <- eigen(r)
pc$values

sumvar <- sum(pc$values)
propvar <- sapply(pc$values, function(x) x/sumvar)*100
cumvar <- data.frame(cbind(pc$values, propvar)) %>% mutate(cum = cumsum(propvar))
colnames(cumvar)[1] <- "value"
num_pc <- length(pc$values)
rownames(cumvar) <- paste0("PC", 1:num_pc)
print(cumvar)

#hasilPCA
pc$vectors
scores <- as.matrix(scale_data) %*% pc$vectors
head(scores)

#loading
loadings <- pc$vectors %*% diag(sqrt(pc$values))
loadings_df <- as.data.frame(loadings)
print(loadings_df)

#Scree Plot
# Load library ggplot2
# Load library ggplot2
library(ggplot2)

# Buat data frame untuk scree plot
scree_data <- data.frame(PC = factor(paste0("PC", 1:length(pc$values)), levels = paste0("PC", 1:length(pc$values))),
                         Eigenvalue = pc$values,
                         PropVar = propvar)

# Plot scree plot menggunakan ggplot2
ggplot(scree_data, aes(x = PC, y = Eigenvalue)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = Eigenvalue, group = 1), color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  scale_y_continuous(breaks = seq(0, ceiling(max(pc$values)), by = 1)) +  # Skala y dalam bilangan bulat
  theme_minimal() +
  labs(title = "Scree Plot",
       x = "Principal Components",
       y = "Eigenvalue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#biplot hasil PCA
# Load library
# Melakukan PCA
pca_result <- prcomp(scale_data, center = TRUE, scale. = TRUE)

install.packages("factoextra")

library(factoextra)
#Biplot
fviz_pca_biplot(pca_result, 
                geom.ind = "point", 
                #col.ind = status.ipm, 
                #palette = c("#FC4E07","#E7B800", "#00AFBB"), 
                addEllipses = TRUE, 
                #legend.title = "Kategori"
)

# correlation circle
contrib_circle <- fviz_pca_var(pca_result, col.var = "contrib",
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                               repel = TRUE) + 
  ggtitle("Kontribusi Variabel")
plot(contrib_circle)

#contribution
contrib_v_PC1 <- fviz_contrib(pca_result, choice = "var", axes = 1, top = 5) + ggtitle("PC1")
contrib_v_PC2 <- fviz_contrib(pca_result, choice = "var", axes = 2, top = 5) + ggtitle("PC2")
contrib_v_PC3 <- fviz_contrib(pca_result, choice = "var", axes = 3, top = 5) + ggtitle("PC3")
contrib_v_PC4 <- fviz_contrib(pca_result, choice = "var", axes = 4, top = 5) + ggtitle("PC4")
plot(contrib_v_PC4)

# R
PCA.mod <- prcomp(scale_data)
summary(PCA.mod)       #vs t(cumvar)
PCA.mod$rotation       #vs pc$vectors
head(PCA.mod$x)        #vs head(scores)

#-------------------------FA----------------------------
varcov = cov(scale_data)
pc = eigen(varcov)
pc$values
pc$vectors
sp = sum(pc$values[1:3])

L1 = sqrt(pc$values[1])*pc$vectors[,1]
L2 = sqrt(pc$values[2])*pc$vectors[,2]
L3 = sqrt(pc$values[3])*pc$vectors[,3]
L4 = sqrt(pc$values[4])*pc$vectors[,4]

L = cbind(L1,L2,L3,L4)
L

# Perform factor analysis 
library(psych)
fa <- fa(r = scale_data, 
         covar = TRUE,
         nfactors = 4, 
         rotate = "varimax") 
fa 

load <- fa$loadings

plot(load[,c(4,4)],type="n") # set up plot
text(load[,c(4,4)],labels=names(data),cex=.7)

fa.diagram(load)
