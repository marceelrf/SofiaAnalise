library(tidyverse)
library(clusterProfiler)
library(org.Hs.eg.db)
library(GSEABase)


renal_genes <- clipr::read_clip()

renal_genes <- toupper(renal_genes)

tictoc::tic()
resultado_renal <- enricher(
  gene = renal_genes,
  pvalueCutoff = 0.05,
  pAdjustMethod = "BH",
  TERM2GENE = data.frame(
    term = rep(names(gmt_data), lengths(gmt_data)),  # Nomes dos gene sets
    gene = unlist(gmt_data)                          # Genes associados
  )
)
tictoc::toc()

writexl::write_xlsx(x = resultado_renal@result,
                    path = "ora_resultado_renal_diseases.xlsx")