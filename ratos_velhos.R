# Analise Ratos 540
library(tidyverse)
library(clusterProfiler)
library(ComplexHeatmap)


Prots <- "GSTP1; MUG2; HBB; ALB; ACTG1; HBA1; MUG1; ACTB; UBB; PDIA3; ENO1; RPS27A; UBC; YWHAG; TKT"

Prots <- str_split_1(string = Prots,pattern = ";")
Prots <- str_remove_all(string = Prots,pattern = " ")


# Função para ler e limpar GMT (removendo DOID:)
read_clean_gmt <- function(gmt_file) {
  # Lê o arquivo GMT usando GSEABase
  gene_sets <- GSEABase::getGmt(gmt_file)
  
  # Extrai os gene sets como uma lista nomeada
  gene_set_list <- GSEABase::geneIds(gene_sets)  # Usando geneIds() em vez de geneSets()
  
  # Remove genes com "DOID:" e filtra conjuntos vazios
  cleaned_sets <- purrr::map(gene_set_list, ~ {
    genes <- .x[!grepl("DOID:", .x)]
    if (length(genes) > 0) return(genes) else return(NULL)
  })
  
  cleaned_sets <- purrr::discard(cleaned_sets, is.null)
  return(cleaned_sets)
}

# Carregar todos os GMTs da pasta
gmt_files <- list.files("genesets/", pattern = "\\.gmt$", full.names = TRUE)
gmt_data <- map(gmt_files, read_clean_gmt) %>% flatten()  # Combina todos os GMTs em uma única lista

tictoc::tic()
resultado_velhos <- enricher(
  gene = Prots,
  pvalueCutoff = 0.05,
  pAdjustMethod = "BH",
  TERM2GENE = data.frame(
    term = rep(names(gmt_data), lengths(gmt_data)),  # Nomes dos gene sets
    gene = unlist(gmt_data)                          # Genes associados
  )
)
tictoc::toc()

resultado_velhos@result |> View()

writexl::write_xlsx(resultado_velhos@result,path = "ratos_velhos_diseases.xlsx")

#heatmap
top20 <- clipr::read_clip_tbl()

tab <-
  resultado_velhos@result %>% 
  dplyr::select(ID, geneID,Count) %>% 
  tidyr::separate_rows(geneID,sep = "\\/")

tab_filt <- 
  tab %>% 
  dplyr::filter(ID %in% top20$ID)

dados_agregados <- tab_filt %>%
  group_by(ID, geneID) %>%
  summarise(value = n(), .groups = "drop")

mat <-
  tab_filt %>% 
  mutate(val = 1) %>% 
  pivot_wider(values_from = val,values_fill = 0,
              names_from = geneID) %>% 
  # arrange(desc(Count)) %>% 
  # slice_head(prop = .1) %>% 
  # dplyr::filter(ID %in% top30$ID) %>% 
  dplyr::select(-Count) %>% 
  column_to_rownames("ID") %>% 
  as.matrix()

mat

tiff(filename = "heatmap_renal540_velhos.tif",width = 10000,height = 8000,
     bg = "white",compression = "lzw",res = 600)
Heatmap(t(mat),
        name = "Presence/Absence",
        col = c("0" = "white", "1" = "red"),
        clustering_distance_columns = "binary",
        clustering_distance_rows = "binary",
        rect_gp = gpar(col = "black", lwd = 0.01),
        border = "black",
        column_names_max_height = unit(10, "cm"),
        show_column_names = T)
dev.off()


library(circlize)

renal_filt <- clipr::read_clip_tbl()

renal_filt_tab <- renal_filt %>% 
  select(ID,geneID,Count) %>% 
  separate_rows(geneID,sep = "\\/")

links <- renal_filt_tab[, c("geneID", "ID")]

rest_genes <- setdiff(Prots, unique(links$geneID))

all_sectors <- unique(c(links$geneID, rest_genes, links$ID))

links$ID <- factor(links$ID, levels = all_sectors)
links$geneID <- factor(links$geneID, levels = all_sectors)

todos_ids <- unique(links$ID)

cores_genes <- setNames(rainbow(length(Prots)), Prots)
cores_ids <- setNames(rep("black", length(todos_ids)), todos_ids)

grid.col <- c(cores_ids, cores_genes)

svg("renal_circosplot_velhos.svg", width = 25, height = 25)
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

tiff(filename = "circosplot_renal_velhos.tif",width = 12000,height = 12000,
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

