library(circlize)
library(ComplexHeatmap)

# top30 <- clipr::read_clip()

tab <-
  resultado@result %>% 
  dplyr::select(ID, geneID,Count) %>% 
  tidyr::separate_rows(geneID,sep = "\\/")

tab_filt <- 
  tab %>% 
  dplyr::filter(ID %in% top30)

dados_agregados <- tab_filt %>%
  group_by(ID, geneID) %>%
  summarise(value = n(), .groups = "drop")

# Inicializar o layout circular
circos.clear()  # Limpar qualquer configuração anterior
circos.par(start.degree = 90, gap.degree = .2)  # Ajustar parâmetros visuais

cores <- rainbow(length(unique(c(tab_filt$ID, tab_filt$geneID))))

chordDiagram(
  dados_agregados,
  annotationTrack = c("grid", "axis"),
  preAllocateTracks = list(
    track.height = uh(5, "mm"),  # Unidade relativa
    track.margin = c(uh(0, "mm"), uh(0, "mm"))
  ),
  grid.col = cores,
  directional = 0,
  link.sort = TRUE,
  link.decreasing = TRUE
)

circos.track(
  track.index = 1,
  panel.fun = function(x, y) {
    sector.index = get.cell.meta.data("sector.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    
    # Rotacionar labels para melhor legibilidade
    circos.text(
      mean(xlim),
      ylim[1],
      sector.index,
      facing = "clockwise",
      niceFacing = TRUE,
      adj = c(0, 0.5),
      cex = 0.7  # Tamanho reduzido da fonte
    )
  },
  bg.border = NA
)

# 8. Adicionar título
title("Relações entre geneID e ID", cex.main = 1.2)

# 9. Limpar configurações
circos.clear()

mat <-
  tab %>% 
  mutate(val = 1) %>% 
  pivot_wider(values_from = val,values_fill = 0,
              names_from = geneID) %>% 
  # arrange(desc(Count)) %>% 
  # slice_head(prop = .1) %>% 
  dplyr::filter(ID %in% top30) %>% 
  dplyr::select(-Count) %>% 
  column_to_rownames("ID") %>% 
  as.matrix()

tiff(filename = "heatmap.tif",width = 10000,height = 5000,
     bg = "white",compression = "lzw",res = 600)
Heatmap(t(mat),
        name = "Presence/Absence",
        col = c("0" = "white", "1" = "red"),
        clustering_distance_columns = "binary",
        clustering_distance_rows = "binary",
        rect_gp = gpar(col = "black", lwd = 0.01),
        border = "black",
        show_column_names = T)
dev.off()
