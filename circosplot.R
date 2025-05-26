library(circlize)

links <- tab_filt[, c("geneID", "ID")]

rest_genes <- setdiff(gene_list, unique(links$geneID))

# linka_2 <-
#   tibble(geneID = gene_list) %>% 
#   left_join(links)

all_sectors <- unique(c(links$geneID, rest_genes, links$ID))

links$ID <- factor(links$ID, levels = all_sectors)
links$geneID <- factor(links$geneID, levels = all_sectors)

todos_ids <- unique(links$ID)

cores_genes <- setNames(rainbow(length(gene_list)), gene_list)
cores_ids <- setNames(rep("black", length(todos_ids)), todos_ids)

grid.col <- c(cores_ids, cores_genes)

svg("meu_circosplot.svg", width = 25, height = 25)
circos.clear()
chordDiagram(links,
             directional = 1,
             order = all_sectors,
             grid.col = grid.col,
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = 0.01))

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
              facing = "clockwise",
              niceFacing = TRUE,
              adj = c(0, 0.5),
              cex = 0.7)
}, bg.border = NA)

dev.off()

tiff(filename = "circosplot.tif",width = 12000,height = 12000,
     compression = "lzw",
     res = 600)
circos.clear()
chordDiagram(links,
             directional = 1,
             order = all_sectors,
             grid.col = grid.col,
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = 0.01))

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
              facing = "clockwise",
              niceFacing = TRUE,
              adj = c(0, 0.5),
              cex = 0.7)
}, bg.border = NA)

dev.off()
