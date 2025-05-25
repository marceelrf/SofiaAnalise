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

# Analise -----------------------------------------------------------------
#Mdh1; Ubb; Uba52; H2az1; Mdh2; Acta2; Tuba1c; Actg2; Acta1; Tubb5; Vdac1; Nme1; Nme2; Ldhb; Hbb; Tubb2a; H2ac25; H2bc1; H2ac18; H2aj; Alb; Actg1; H3-3b; Tubb2b; Tuba1b; Hba1; Actc1; Tpi1; Tuba3a; Tuba3b; Actb; Tubb4b; Tuba1a; H4c16; Ldha; Tubb3
#genes <- clipr::read_clip()

genes_prep <- str_split(string = genes,pattern = "; ",simplify = T)[1,]

gene_list <- toupper(genes_prep)

# Definir genes de fundo (background) - ajuste conforme necessário
# background_genes <- keys(org.Hs.eg.db, keytype = "SYMBOL")

tictoc::tic()
resultado <- enricher(
  gene = gene_list,
  pvalueCutoff = 0.05,
  pAdjustMethod = "BH",
  TERM2GENE = data.frame(
    term = rep(names(gmt_data), lengths(gmt_data)),  # Nomes dos gene sets
    gene = unlist(gmt_data)                          # Genes associados
  )
)
tictoc::toc()

writexl::write_xlsx(x = resultado@result,
                    path = "ora_diseases.xlsx")
