library(tidyverse)
library(GO.db)
library(AnnotationDbi)
library(clusterProfiler)
library(GSEABase)

placenta_df <- read_tsv(file = "Data/placental_development_GO.txt",
                        col_names = F)


GO_df <- AnnotationDbi::select(GO.db,
       keys = placenta_df$X5,
       columns = c("TERM", "ONTOLOGY"),
       keytype = "GOID")


placenta_df <- 
  placenta_df %>% 
  group_by(X5) %>% 
  nest() %>% 
  inner_join(distinct(GO_df),by = join_by(X5 == GOID)) %>% 
  unnest() %>% 
  ungroup()


pre_enrich <- placenta_df %>% 
  dplyr::select(Symbol = X3,TERM,GOID = X5)

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
gmt_files <- list.files("genesets/",
                        pattern = "\\.gmt$",
                        full.names = TRUE)

gmt_data <- map(gmt_files, read_clean_gmt) %>% flatten()  # Combina todos os GMTs em uma única lista

fn_run_erichment <- function(genes) {

  genes <- toupper(genes)

  results <- enricher(
    gene = genes,
    pvalueCutoff = 0.05,
    pAdjustMethod = "BH",
    TERM2GENE = data.frame(
      term = rep(names(gmt_data), lengths(gmt_data)),  # Nomes dos gene sets
      gene = unlist(gmt_data)                          # Genes associados
    )
  )

  if (is.null(results) || nrow(results@result) == 0) {
    return(NULL)
  }

  return(results@result)
}

terms_list <- pre_enrich |> 
  pull(TERM) |> 
  unique()

placental_development_list <- list()

for(i in seq_along(terms_list)){

  term <- terms_list[i]

  GENES <- pre_enrich |> 
    dplyr::filter(TERM == term) |> 
    dplyr::pull(Symbol)

  GOID <- pre_enrich |> 
    dplyr::filter(TERM == term) |> 
    dplyr::pull(GOID) |> 
    unique()

  res <- fn_run_erichment(GENES)

  GOID_fix <- str_replace(GOID,":","")

  file_name <- paste0(str_replace_all(term," ","_"),"_",GOID_fix,".xlsx")
  print(GOID_fix)

  writexl::write_xlsx(res,path = paste0("placental_development/",file_name))

  cat(paste0("Feito ", i,"/", length(terms_list),"\n"))
}
