library(tidyverse)
library(GO.db)
library(AnnotationDbi)
library(clusterProfiler)
library(GSEABase)
library(msigdbr)

m_df <- msigdbr(species = "Homo sapiens", category = "C5", subcategory = "GO:BP")


ids <- read_tsv(file = "Data/placental_development_GO.txt",col_names = F) |> 
  dplyr::pull(X5)

placental_GO <-
  m_df |> 
  filter(gs_exact_source %in% ids)

placenta_up <- readxl::read_xlsx(path = "Data/Alvos_Placenta.xlsx",
                                  sheet = 1,col_names = c("Genes")) |> 
  dplyr::pull(Genes)

placenta_down <- readxl::read_xlsx(path = "Data/Alvos_Placenta.xlsx",
                                  sheet = 2,col_names = c("Genes")) |> 
  dplyr::pull(Genes)

t2g <- placental_GO %>% dplyr::select(gs_name, gene_symbol)

res_up <- enricher(
  gene = toupper(placenta_up),
  TERM2GENE = t2g,
  pvalueCutoff = 0.05,
  pAdjustMethod = "BH"
)

res_up@result |> View()

writexl::write_xlsx(x = res_up@result,path = "placental_luisa/developmental_up.xlsx")

res_down <- enricher(
  gene = toupper(placenta_down),
  TERM2GENE = t2g,
  pvalueCutoff = 0.05,
  pAdjustMethod = "BH"
)

enrichplot::dotplot(res_up)

# Diseases
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

gmt_data <- map(gmt_files, read_clean_gmt) %>% flatten() 

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

  return(results)
}

Diseases_down <- fn_run_erichment(placenta_down)
Diseases_up <- fn_run_erichment(placenta_up)

fs::dir_create(path = "placental_luisa")

writexl::write_xlsx(Diseases_down@result,path = "placental_luisa/diseases_down.xlsx")
writexl::write_xlsx(Diseases_up@result,path = "placental_luisa/diseases_up.xlsx")

enrichplot::dotplot(Diseases_down)
ggsave(filename = "placental_luisa/dotplot_diseases_down.png",
  dpi = 600,bg = "white",scale = 1.5)

enrichplot::dotplot(Diseases_up)
ggsave(filename = "placental_luisa/dotplot_diseases_up.png",
  dpi = 600,bg = "white",scale = 1.5)


enrichplot::cnetplot(Diseases_up)
enrichplot::cnetplot(Diseases_down)

enrichplot::heatplot(Diseases_down)
ggsave(filename = "placental_luisa/heatmap_diseases_down.png",
  dpi = 600,bg = "white",scale = 1.5)
# Exportar os dados necessários
enrichplot::heatplot(Diseases_up)
ggsave(filename = "placental_luisa/heatmap_diseases_up.png",
  dpi = 600,bg = "white",scale = 2)
# Exportar os dados necessários
library(jsonlite)

extract_cnet_data <- function(enrich_obj, label) {
  df <- as.data.frame(enrich_obj)
  df$direction <- label
  return(df)
}

up_df   <- extract_cnet_data(Diseases_up, "up")
down_df <- extract_cnet_data(Diseases_down, "down")

combined <- rbind(up_df, down_df)
write.csv(combined, "enrichr_combined_diseases.csv", row.names = FALSE)

library(ggraph)
library(igraph)
cnetplot_combined <- function(
    enrich_up,
    enrich_down,
    top_n          = 10,       # número máximo de termos por direção
    min_genes      = 3,        # mínimo de genes por termo para incluir
    layout         = "fr",     # "fr", "stress", "kk", "nicely"
    node_size_gene = 4,        # tamanho base dos nós de genes
    node_size_term = 8,        # tamanho base dos nós de termos
    label_size     = 3,        # tamanho do texto dos nós
    label_terms    = TRUE,     # mostrar label nos termos
    label_genes    = TRUE,     # mostrar label nos genes
    color_up       = "#E63946", # cor para Up
    color_down     = "#457B9D", # cor para Down
    color_both     = "#9B5DE5", # cor para presentes em ambos
    color_edge_up  = "#E6394650",
    color_edge_down= "#457B9D50",
    seed           = 42,
    title          = "Gene-Disease Network",
    subtitle       = NULL
) {
 
  set.seed(seed)
 
  # ---------------------------------------------------------------------------
  # 1. Extrair data frames dos objetos enrichResult
  # ---------------------------------------------------------------------------
  .extract_edges <- function(enrich_obj, direction, top_n, min_genes) {
    df <- as.data.frame(enrich_obj)
 
    # Filtrar por contagem mínima de genes
    df <- df[df$Count >= min_genes, ]
 
    # Ordenar por p.adjust e pegar top_n
    df <- df[order(df$p.adjust), ]
    df <- head(df, top_n)
 
    if (nrow(df) == 0) {
      warning(paste("Nenhum termo encontrado para direção:", direction))
      return(NULL)
    }
 
    # Explodir genes (separados por "/")
    edges <- df %>%
      dplyr::select(Description, geneID, p.adjust) %>%
      tidyr::separate_rows(geneID, sep = "/") %>%
      dplyr::rename(term = Description, gene = geneID) %>%
      dplyr::mutate(direction = direction)
 
    return(edges)
  }
 
  edges_up   <- .extract_edges(enrich_up,   "up",   top_n, min_genes)
  edges_down <- .extract_edges(enrich_down, "down", top_n, min_genes)
 
  # Combinar arestas
  all_edges <- dplyr::bind_rows(edges_up, edges_down)
 
  if (nrow(all_edges) == 0) stop("Nenhuma aresta encontrada. Verifique os objetos de entrada.")
 
  # ---------------------------------------------------------------------------
  # 2. Identificar direção dos nós (genes e termos)
  # ---------------------------------------------------------------------------
  genes_up   <- unique(edges_up$gene)
  genes_down <- unique(edges_down$gene)
  genes_both <- intersect(genes_up, genes_down)
 
  terms_up   <- unique(edges_up$term)
  terms_down <- unique(edges_down$term)
  terms_both <- intersect(terms_up, terms_down)
 
  # Função para classificar direção
  .classify <- function(node, up_set, down_set) {
    is_up   <- node %in% up_set
    is_down <- node %in% down_set
    dplyr::case_when(
      is_up & is_down ~ "both",
      is_up           ~ "up",
      is_down         ~ "down",
      TRUE            ~ "unknown"
    )
  }
 
  # Todos os nós únicos
  all_genes <- unique(all_edges$gene)
  all_terms <- unique(all_edges$term)
 
  nodes_genes <- data.frame(
    name      = all_genes,
    node_type = "gene",
    direction = .classify(all_genes, genes_up, genes_down),
    stringsAsFactors = FALSE
  )
 
  nodes_terms <- data.frame(
    name      = all_terms,
    node_type = "term",
    direction = .classify(all_terms, terms_up, terms_down),
    stringsAsFactors = FALSE
  )
 
  nodes <- dplyr::bind_rows(nodes_genes, nodes_terms)
 
  # ---------------------------------------------------------------------------
  # 3. Montar o grafo igraph
  # ---------------------------------------------------------------------------
  # Arestas: usar apenas uma aresta por par (gene, term), mantendo a direção
  # Se um gene aparece em up e down para o mesmo termo → "both"
  edge_df <- all_edges %>%
    dplyr::group_by(gene, term) %>%
    dplyr::summarise(
      direction = ifelse(n_distinct(direction) > 1, "both", dplyr::first(direction)),
      .groups = "drop"
    ) %>%
    dplyr::rename(from = gene, to = term)
 
  g <- igraph::graph_from_data_frame(
    d        = edge_df,
    vertices = nodes,
    directed = FALSE
  )
 
  # ---------------------------------------------------------------------------
  # 4. Definir paleta e tamanhos
  # ---------------------------------------------------------------------------
  dir_colors <- c(
    "up"      = color_up,
    "down"    = color_down,
    "both"    = color_both,
    "unknown" = "#AAAAAA"
  )
 
  edge_colors <- c(
    "up"   = color_edge_up,
    "down" = color_edge_down,
    "both" = paste0(color_both, "60")
  )
 
  # ---------------------------------------------------------------------------
  # 5. Construir o plot
  # ---------------------------------------------------------------------------
  p <- ggraph(g, layout = layout) +
 
    # Arestas
    geom_edge_link(
      aes(color = direction),
      width = 0.6,
      alpha = 0.7
    ) +
    scale_edge_color_manual(
      values = edge_colors,
      name   = "Aresta",
      labels = c("up" = "Up-regulated", "down" = "Down-regulated", "both" = "Both"),
      guide  = guide_legend(override.aes = list(edge_width = 2))
    ) +
 
    # Nós de genes
    geom_node_point(
      data = function(x) x[x$node_type == "gene", ],
      aes(color = direction),
      size  = node_size_gene,
      shape = 16
    ) +
 
    # Nós de termos (maior, forma diferente)
    geom_node_point(
      data = function(x) x[x$node_type == "term", ],
      aes(color = direction),
      size  = node_size_term,
      shape = 18   # losango para termos
    ) +
 
    scale_color_manual(
      values = dir_colors,
      name   = "Condition",
      labels = c("up" = "Up-regulated", "down" = "Down-regulated",
                 "both" = "Both", "unknown" = "N/A"),
      guide  = guide_legend(override.aes = list(size = 5))
    ) +
 
    # Labels dos termos
    {if (label_terms)
      geom_node_label(
        data       = function(x) x[x$node_type == "term", ],
        aes(label  = name, color = direction),
        size       = label_size,
        repel      = TRUE,
        fontface   = "bold",
        fill       = scales::alpha("white", 0.75),
        label.size = 0.2,
        show.legend = FALSE,
         max.overlaps = Inf
      )
    } +
 
    # Labels dos genes (opcional)
    {if (label_genes)
      geom_node_label(
        data        = function(x) x[x$node_type == "gene", ],
        aes(label   = name, color = direction),
        size        = label_size * 0.8,
        repel       = TRUE,
        fill        = scales::alpha("white", 0.6),
        label.size  = 0.15,
        show.legend = FALSE,
         max.overlaps = Inf
      )
    } +
 
    labs(
      title    = title,
      subtitle = subtitle
    ) +
 
    theme_void(base_family = "sans") +
    theme(
      plot.title      = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 4)),
      plot.subtitle   = element_text(size = 11, hjust = 0.5, color = "grey40", margin = margin(b = 8)),
      legend.position = "right",
      legend.title    = element_text(size = 10, face = "bold"),
      legend.text     = element_text(size = 9),
      plot.margin     = margin(10, 10, 10, 10)
    )
 
  return(p)
}
 
 
 
# =============================================================================
# EXEMPLO DE USO
# =============================================================================
#
# p <- cnetplot_combined(
#   enrich_up   = Diseases_up,
#   enrich_down = Diseases_down,
#   top_n       = 10,
#   min_genes   = 3,
#   layout      = "fr",
#   title       = "Gene-Disease Network",
#   subtitle    = "Up- vs Down-regulated targets"
# )
#
# print(p)
#
# # Exportar em alta resolução para publicação:
# ggsave(
#   filename = "cnetplot_combined.pdf",
#   plot     = p,
#   width    = 12,
#   height   = 10,
#   units    = "in",
#   device   = cairo_pdf   # melhor para fontes em PDF
# )
#
# # Ou PNG em alta resolução:
# ggsave(
#   filename = "cnetplot_combined.png",
#   plot     = p,
#   width    = 12,
#   height   = 10,
#   dpi      = 300
# )
 
p <- cnetplot_combined(
  enrich_up   = Diseases_up,
  enrich_down = Diseases_down,
  top_n       = 10,
  min_genes   = 3,
  layout      = "fr",
  title       = "Gene-Disease Network",
  subtitle    = "Up- vs Down-regulated targets"
)

p

ggsave(
  filename = "cnetplot_combined.png",
  plot     = p,
  width    = 12,
  height   = 10,
  dpi      = 600,
  bg = "white"
)


### Developmental

goid <- read_tsv(file = "Data/placental_development_GO.txt",col_names = F) |> 
  dplyr::select(X5) |> 
  dplyr::distinct()

