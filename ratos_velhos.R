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
tab <-
  resultado_velhos@result %>% 
  dplyr::select(ID, geneID,Count) %>% 
  tidyr::separate_rows(geneID,sep = "\\/")

dados_agregados <- tab %>%
  group_by(ID, geneID) %>%
  summarise(value = n(), .groups = "drop")

mat <-
  tab %>% 
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
