library(circlize)
library(ComplexHeatmap)

tab <-
  resultado@result %>% 
  dplyr::select(ID, geneID,Count) %>% 
  tidyr::separate_rows(geneID,sep = "\\/")

# dados_agregados <- tab %>%
#   group_by(ID, geneID) %>%
#   summarise(value = n(), .groups = "drop")
# 
# # Inicializar o layout circular
# circos.clear()  # Limpar qualquer configuração anterior
# circos.par(start.degree = 90, gap.degree = .2)  # Ajustar parâmetros visuais
# 
# cores <- rainbow(length(unique(c(tab$ID, tab$geneID))))
# 
# chordDiagram(dados_agregados, 
#              annotationTrack = c("grid", "axis"),
#              preAllocateTracks = list(track.height = 0.1),
#              grid.col = cores)


mat <-
  tab %>% 
  mutate(val = 1) %>% 
  pivot_wider(values_from = val,values_fill = 0,
              names_from = geneID) %>% 
  arrange(desc(Count)) %>% 
  slice_head(prop = .1) %>% 
  dplyr::select(-Count) %>% 
  column_to_rownames("ID") %>% 
  as.matrix()

tiff(filename = "heatmap.tif",width = 10000,height = 4000,
     bg = "white",compression = "lzw",res = 450)
Heatmap(t(mat),
        col = c("0" = "white", "1" = "red"),
        clustering_distance_columns = "binary",
        clustering_distance_rows = "binary",
        rect_gp = gpar(col = "black", lwd = 0.01),
        show_column_names = T)
dev.off()
