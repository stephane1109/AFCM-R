# install.packages("FactoMineR")

#!/usr/bin/env Rscript

# =====================================================================
# AFCM (TDC) + AFC (Burt) — TOUTES les modalités actives dans les deux
# Jeux de variables actifs : A, B, C, D, Sexe, Age, Education
# Recodage strict :
#  - A,B,C,D : 1..5  -> A_1..A_5 ; B_1..B_5 ; C_1..C_5 ; D_1..D_5
#  - sex     : 1->h ; 2->f
#  - age     : 1..6 / "age1..age6" / déjà "ag_*" -> ag_16_24..ag_65+
#  - edu     : 1..6 -> primaire, niv_collège, niv_lycée, niv_Bac+2, niv_Bac+5, niv_Bac+8
# Sorties :
#  - donnees_recodées.csv
#  - tableau_disjonctif_complet.csv
#  - tableau_de_burt.csv
#  - inerties_afcm_tdc.csv ; inerties_afc_burt.csv
#  - modalites_TDC_axes12_tout.png ; modalites_Burt_axes12_tout.png
# Dépendances : FactoMineR, ggplot2, ggrepel
# =====================================================================

suppressPackageStartupMessages({
  library(FactoMineR)
  library(ggplot2)
  library(ggrepel)
})

options(ggrepel.max.overlaps = Inf)

# Paramètres graphiques (style demandé)
TAILLE_TEXTE_BASE <- 12
LARGEUR_POUCES    <- 12
HAUTEUR_POUCES    <- 9
DPI_FIG           <- 150
COL               <- "red"
FORME             <- 17   # triangle rempli
TAILLE_POINT      <- 2.7

# --------- utilitaires ---------
logf <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", paste(..., collapse=" "), "\n")

rep_script <- function(){
  args <- commandArgs(trailingOnly = FALSE)
  farg <- "--file="
  p <- sub(farg, "", args[grep(farg, args)])
  if (length(p) == 0) return(getwd())
  normalizePath(dirname(p))
}

lire_csv_robuste <- function(chemin){
  essais <- list(
    list(sep=";", enc="UTF-8"),
    list(sep=",", enc="UTF-8"),
    list(sep="\t", enc="UTF-8"),
    list(sep=";", enc="latin1"),
    list(sep=",", enc="latin1")
  )
  for (e in essais){
    z <- try(read.table(chemin, header=TRUE, sep=e$sep, quote="\"",
                        fileEncoding=e$enc, stringsAsFactors=FALSE, check.names=FALSE),
             silent=TRUE)
    if (!inherits(z, "try-error") && ncol(z) > 1){
      logf("Lecture OK sep='", e$sep, "' enc='", e$enc, "'.")
      return(z)
    }
  }
  stop("Impossible de lire le CSV correctement.")
}

sauver_png <- function(plot_obj, chemin){
  ggsave(filename = chemin, plot = plot_obj,
         width = LARGEUR_POUCES, height = HAUTEUR_POUCES, dpi = DPI_FIG, bg = "white")
}

# --------- recodage strict & robuste ---------
recoder_item_1_5 <- function(x, pfx){
  v <- suppressWarnings(as.integer(x))
  if (any(is.na(v)) || any(!(v %in% 1:5))){
    ex <- head(x[is.na(v) | !(v %in% 1:5)], 5)
    stop(sprintf("%s non recodable (1..5). Exemples: %s", pfx, paste(ex, collapse=", ")))
  }
  factor(paste0(pfx, "_", v), levels=paste0(pfx, "_", 1:5), ordered=TRUE)
}
recode_sexe_hf <- function(x){
  v <- suppressWarnings(as.integer(x))
  if (any(is.na(v)) || any(!(v %in% c(1,2)))){
    ex <- head(x[is.na(v) | !(v %in% c(1,2))], 5)
    stop(sprintf("Sexe non recodable (1/2). Exemples: %s", paste(ex, collapse=", ")))
  }
  factor(ifelse(v==1,"h","f"), levels=c("h","f"))
}
recode_age_robuste <- function(x){
  cible <- c("ag_16_24","ag_25_34","ag_35_44","ag_45_54","ag_55_64","ag_65+")
  xs <- as.character(x)
  map_age_txt <- c(age1="ag_16_24", age2="ag_25_34", age3="ag_35_44",
                   age4="ag_45_54", age5="ag_55_64", age6="ag_65+")
  if (all(xs %in% c(cible, names(map_age_txt), NA))){
    y <- ifelse(xs %in% names(map_age_txt), map_age_txt[xs], xs)
    return(factor(y, levels=cible, ordered=TRUE))
  }
  v <- suppressWarnings(as.integer(x))
  if (!any(is.na(v)) && all(v %in% 1:6)){
    map_age_num <- c("1"="ag_16_24","2"="ag_25_34","3"="ag_35_44","4"="ag_45_54","5"="ag_55_64","6"="ag_65+")
    y <- unname(map_age_num[as.character(v)])
    return(factor(y, levels=cible, ordered=TRUE))
  }
  ex <- head(x[is.na(v) | !(v %in% 1:6)], 5)
  stop(sprintf("Age non recodable (1..6, 'age1..age6' ou 'ag_*'). Exemples: %s", paste(ex, collapse=", ")))
}
recode_edu_labels <- function(x){
  v <- suppressWarnings(as.integer(x))
  if (any(is.na(v)) || any(!(v %in% 1:6))){
    ex <- head(x[is.na(v) | !(v %in% 1:6)], 5)
    stop(sprintf("Education non recodable (1..6). Exemples: %s", paste(ex, collapse=", ")))
  }
  map_edu <- c("1"="primaire","2"="niv_collège","3"="niv_lycée",
               "4"="niv_Bac+2","5"="niv_Bac+5","6"="niv_Bac+8")
  y <- unname(map_edu[as.character(v)])
  factor(y, levels = unname(map_edu[as.character(1:6)]), ordered=TRUE)
}
recoder_strict_robuste <- function(df, colA="A", colB="B", colC="C", colD="D",
                                   colSexe="sex", colAge="age", colEdu="edu"){
  req <- c(colA,colB,colC,colD,colSexe,colAge,colEdu)
  manq <- setdiff(req, names(df))
  if (length(manq)) stop(paste("Colonnes manquantes :", paste(manq, collapse=", ")))
  A <- recoder_item_1_5(df[[colA]], "A")
  B <- recoder_item_1_5(df[[colB]], "B")
  C <- recoder_item_1_5(df[[colC]], "C")
  D <- recoder_item_1_5(df[[colD]], "D")
  Sexe <- recode_sexe_hf(df[[colSexe]])
  Age  <- recode_age_robuste(df[[colAge]])
  Education <- recode_edu_labels(df[[colEdu]])
  data.frame(A=A,B=B,C=C,D=D, Sexe=Sexe, Age=Age, Education=Education, check.names=FALSE)
}

# --------- TDC explicite ---------
faire_tdc <- function(df_cat, vars=c("A","B","C","D","Sexe","Age","Education")){
  blocs <- list()
  for (v in vars){
    lv <- levels(df_cat[[v]])
    M  <- model.matrix(~ df_cat[[v]] - 1)
    colnames(M) <- lv
    blocs[[v]] <- M
  }
  do.call(cbind, blocs)
}

# ===================== programme principal =====================
main <- function(){
  rep <- rep_script()
  fichier <- file.path(rep, "sciences.csv")
  sortie  <- rep
  
  logf("Début. Toutes les modalités actives en TDC et en Burt.")
  df <- lire_csv_robuste(fichier)
  logf("Dimensions :", nrow(df), "lignes,", ncol(df), "colonnes.")
  logf("Colonnes :", paste(names(df), collapse=", "))
  
  dfr <- recoder_strict_robuste(df, "A","B","C","D","sex","age","edu")
  write.csv(dfr, file.path(sortie, "donnees_recodées.csv"), row.names = FALSE)
  logf("Recodage terminé.")
  
  # ---------- AFCM sur TDC avec TOUTES les variables actives ----------
  acm <- FactoMineR::MCA(dfr, ncp = 2, graph = FALSE)
  
  inert_tdc <- data.frame(axe = paste0("Axe", seq_len(nrow(acm$eig))),
                          part_inertie_pct = round(acm$eig[,2], 1))
  write.csv(inert_tdc, file.path(sortie, "inerties_afcm_tdc.csv"), row.names = FALSE)
  
  # coordonnées de toutes les modalités (A,B,C,D,Sexe,Age,Education)
  mod_tdc <- as.data.frame(acm$var$coord[, 1:2, drop = FALSE])
  mod_tdc$nom <- rownames(mod_tdc)
  names(mod_tdc)[1:2] <- c("Dim1","Dim2")
  
  p_tdc <- ggplot(mod_tdc, aes(Dim1, Dim2, label = nom)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_point(color = COL, shape = FORME, size = TAILLE_POINT) +
    ggrepel::geom_text_repel(color = COL, size = 3.5) +
    theme_minimal(base_size = TAILLE_TEXTE_BASE) +
    labs(
      title   = "Variable categories - MCA (TDC, toutes modalités actives)",
      x       = sprintf("Dim1 (%.1f%%)", acm$eig[1,2]),
      y       = sprintf("Dim2 (%.1f%%)", acm$eig[2,2]),
      caption = "AFCM sur tableau disjonctif complet, toutes les variables actives."
    ) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))
  sauver_png(p_tdc, file.path(sortie, "modalites_TDC_axes12_tout.png"))
  logf(sprintf("AFCM(TDC) — Axe1/Axe2 : %.1f%% / %.1f%%", acm$eig[1,2], acm$eig[2,2]))
  
  # ---------- Tableau de Burt avec TOUTES les variables ----------
  X <- faire_tdc(dfr, vars = c("A","B","C","D","Sexe","Age","Education"))
  write.csv(X, file.path(sortie, "tableau_disjonctif_complet.csv"), row.names = FALSE)
  
  B <- t(X) %*% X
  write.csv(B, file = file.path(sortie, "tableau_de_burt.csv"))
  
  caB <- FactoMineR::CA(as.matrix(B), ncp = 2, graph = FALSE)
  
  inert_burt <- data.frame(axe = paste0("Axe", seq_len(nrow(caB$eig))),
                           part_inertie_pct = round(caB$eig[,2], 1))
  write.csv(inert_burt, file.path(sortie, "inerties_afc_burt.csv"), row.names = FALSE)
  
  mod_burt <- as.data.frame(caB$col$coord[, 1:2, drop = FALSE])
  mod_burt$nom <- rownames(mod_burt)
  names(mod_burt)[1:2] <- c("Dim1","Dim2")
  
  p_burt <- ggplot(mod_burt, aes(Dim1, Dim2, label = nom)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_point(color = COL, shape = FORME, size = TAILLE_POINT) +
    ggrepel::geom_text_repel(color = COL, size = 3.5) +
    theme_minimal(base_size = TAILLE_TEXTE_BASE) +
    labs(
      title   = "Variable categories - CA (Burt, toutes modalités actives)",
      x       = sprintf("Dim1 (%.1f%%)", caB$eig[1,2]),
      y       = sprintf("Dim2 (%.1f%%)", caB$eig[2,2]),
      caption = "AFC sur le tableau de Burt construit avec toutes les variables."
    ) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))
  sauver_png(p_burt, file.path(sortie, "modalites_Burt_axes12_tout.png"))
  logf(sprintf("AFC(Burt) — Axe1/Axe2 : %.1f%% / %.1f%%", caB$eig[1,2], caB$eig[2,2]))
  
  logf("Terminé. Graphes : modalites_TDC_axes12_tout.png ; modalites_Burt_axes12_tout.png")
  logf("CSV : donnees_recodées.csv ; tableau_disjonctif_complet.csv ; tableau_de_burt.csv ; inerties_*")
}

# Exécution
main()







