#PCA


pca = prcomp(d[,1:4], scale=T)
plot(pca)
fviz_eig(pca, addlabels = TRUE, choice="variance")
biplot(pca, xpd=TRUE)

var <- get_pca_var(pca)
fviz_pca_var(pca, col.var = "black")
fviz_cos2(pca, choice = "var", axes = 1:2)
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)