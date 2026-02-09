library(tidyverse)
library(clusterProfiler)
library(org.Hs.eg.db)
library(GSEABase)



# Funções -----------------------------------------------------------------

# Função para ler e limpar GMT (removendo DOID:)
read_clean_gmt <- function(gmt_file) {
  # Lê o arquivo GMT usando GSEABase
  gene_sets <- getGmt(gmt_file)
  
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


renal_genes <- read_lines(file = "genes_renal540.txt")

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
                    path = "ora_resultado_renal540_diseases.xlsx")

