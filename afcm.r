# install.packages("FactoMineR")

# =====================================================================
# Script complet — AFCM (TDC) + AFC (Burt) + tableaux χ²
# Affichages TERMINAL et exports en utilisant A_1..A_5, B_1..B_5, ...
# Illustratives : Education, Sexe, Age  ×  Questions : A, B, C, D
# =====================================================================

suppressPackageStartupMessages({
  library(FactoMineR)
  library(ggplot2)
  library(ggrepel)
})

options(ggrepel.max.overlaps = Inf)

# ----------------- Paramètres graphiques (fond blanc sans grille) -----------------
TAILLE_TEXTE_BASE <- 12
LARGEUR_POUCES    <- 12
HAUTEUR_POUCES    <- 9
DPI_FIG           <- 150
COL               <- "red"
FORME             <- 17
TAILLE_POINT      <- 2.7

theme_blanc <- function(){
  theme_minimal(base_size = TAILLE_TEXTE_BASE) +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
}

# ----------------- Utilitaires -----------------
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

# ----------------- Recodages stricts (avec labels A_1.., B_1..) -----------------
recoder_item_1_5_avec_prefixe <- function(x, pfx){
  v <- suppressWarnings(as.integer(x))
  if (any(is.na(v)) || any(!(v %in% 1:5))){
    ex <- head(x[is.na(v) | !(v %in% 1:5)], 5)
    stop(sprintf("%s non recodable (1..5). Exemples: %s", pfx, paste(ex, collapse=", ")))
  }
  factor(paste0(pfx, "_", v), levels = paste0(pfx, "_", 1:5), ordered = TRUE)
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
  A <- recoder_item_1_5_avec_prefixe(df[[colA]], "A")
  B <- recoder_item_1_5_avec_prefixe(df[[colB]], "B")
  C <- recoder_item_1_5_avec_prefixe(df[[colC]], "C")
  D <- recoder_item_1_5_avec_prefixe(df[[colD]], "D")
  Sexe <- recode_sexe_hf(df[[colSexe]])
  Age  <- recode_age_robuste(df[[colAge]])
  Education <- recode_edu_labels(df[[colEdu]])
  data.frame(A=A,B=B,C=C,D=D, Sexe=Sexe, Age=Age, Education=Education, check.names=FALSE)
}

# ----------------- TDC explicite -----------------
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

# ----------------- χ² GLOBAL : illustrative vs A/B/C/D -----------------
chi2_global_vs_ABCD <- function(df_cat, var_illustrative){
  questions <- c("A","B","C","D")
  res <- vector("list", length(questions))
  for (i in seq_along(questions)){
    vj <- questions[i]
    xi <- as.factor(df_cat[[var_illustrative]])
    xj <- as.factor(df_cat[[vj]])
    ok <- stats::complete.cases(xi, xj)
    xi2 <- droplevels(xi[ok]); xj2 <- droplevels(xj[ok])
    if (nlevels(xi2) < 2 || nlevels(xj2) < 2){
      res[[i]] <- data.frame(variable_question=vj, chi2=NA_real_, p_value=NA_real_, v_cramer=NA_real_)
      next
    }
    tab <- table(xi2, xj2)
    ct  <- suppressWarnings(chisq.test(tab, correct = FALSE))
    r <- nrow(tab); c <- ncol(tab)
    vcram <- sqrt(as.numeric(ct$statistic) / (sum(tab) * (min(r-1, c-1))))
    res[[i]] <- data.frame(variable_question=vj,
                           chi2=as.numeric(ct$statistic),
                           p_value=as.numeric(ct$p.value),
                           v_cramer=as.numeric(vcram))
  }
  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out
}

export_chi2_tables_separees_exact <- function(df_cat, sortie){
  chemin <- file.path(sortie, "chi2_tables_separees.csv")
  con <- file(chemin, open="wt", encoding="UTF-8")
  
  ecrire_bloc <- function(nom_var_illus){
    writeLines(paste0("Tableau : ", nom_var_illus, " vs A\tB\tC\tD"), con)
    writeLines("variable_question,chi2,p_value,v_cramer", con)
    tab <- chi2_global_vs_ABCD(df_cat, nom_var_illus)
    ord <- match(c("A","B","C","D"), tab$variable_question)
    tab <- tab[ord, , drop = FALSE]
    utils::write.table(tab, con, sep=",", row.names=FALSE, col.names=FALSE, na="", qmethod="double")
    writeLines("", con)
  }
  
  ecrire_bloc("Education")
  ecrire_bloc("Sexe")
  ecrire_bloc("Age")
  
  close(con)
  invisible(chemin)
}

# ----------------- Contingence & χ² cellulaires pour ABCD × Education -----------------
matrices_pour_var_contre_edu <- function(df_cat, varname){
  ord_edu <- levels(df_cat$Education)
  tab <- table(df_cat[[varname]], df_cat$Education)
  tab <- tab[, ord_edu, drop=FALSE]
  ct  <- suppressWarnings(chisq.test(tab, correct = FALSE))
  exp <- ct$expected
  contrib <- (tab - exp)^2 / exp
  residus <- ct$stdres
  rownames(tab)     <- paste0(varname, "_", 1:5)
  rownames(exp)     <- paste0(varname, "_", 1:5)
  rownames(contrib) <- paste0(varname, "_", 1:5)
  rownames(residus) <- paste0(varname, "_", 1:5)
  list(obs = tab, att = exp, contrib = contrib, residus = residus)
}

tables_ABCD_vs_edu <- function(df_cat){
  Ax <- matrices_pour_var_contre_edu(df_cat, "A")
  Bx <- matrices_pour_var_contre_edu(df_cat, "B")
  Cx <- matrices_pour_var_contre_edu(df_cat, "C")
  Dx <- matrices_pour_var_contre_edu(df_cat, "D")
  obs  <- rbind(Ax$obs,  Bx$obs,  Cx$obs,  Dx$obs)
  att  <- rbind(Ax$att,  Bx$att,  Cx$att,  Dx$att)
  cont <- rbind(Ax$contrib, Bx$contrib, Cx$contrib, Dx$contrib)
  res  <- rbind(Ax$residus, Bx$residus, Cx$residus, Dx$residus)
  list(obs=obs, att=att, contrib=cont, residus=res)
}

export_tables_cellulaires <- function(df_cat, sortie){
  T <- tables_ABCD_vs_edu(df_cat)
  write.csv(as.data.frame.matrix(T$obs),      file.path(sortie, "contingence_ABCD_par_edu.csv"), row.names = TRUE)
  write.csv(as.data.frame.matrix(T$att),      file.path(sortie, "attendus_ABCD_par_edu.csv"),    row.names = TRUE)
  write.csv(as.data.frame.matrix(T$contrib),  file.path(sortie, "contrib_chi2_ABCD_par_edu.csv"),row.names = TRUE)
  write.csv(as.data.frame.matrix(T$residus),  file.path(sortie, "residus_std_ABCD_par_edu.csv"), row.names = TRUE)
  logf("Tables écrites : contingence_*.csv, attendus_*.csv, contrib_chi2_*.csv, residus_std_*.csv")
}

# ----------------- Tableaux “colonnes” (ChiDist/Inertia/Dim1/Dim2 + Chi2_col/p_value) -----------------
resume_colonnes_CA <- function(df_cat, rows_var, cols_var, ncp = 2){
  xt <- table(df_cat[[rows_var]], df_cat[[cols_var]], dnn = c(rows_var, cols_var))
  ca <- FactoMineR::CA(as.matrix(xt), ncp = ncp, graph = FALSE)
  
  n_axes_dispo <- ncol(ca$col$coord)
  ncp_eff <- min(ncp, max(1, n_axes_dispo))
  
  masses_c  <- as.numeric(ca$col$mass)
  coords    <- ca$col$coord[, 1:ncp_eff, drop = FALSE]
  chi_dist  <- sqrt(rowSums(coords^2))
  inertia   <- masses_c * (chi_dist^2)
  
  R <- nrow(xt)
  prof_lignes <- rowSums(xt) / sum(xt)
  col_tot     <- colSums(xt)
  exp_cols    <- outer(prof_lignes, col_tot)
  
  chi2_col <- colSums((xt - exp_cols)^2 / exp_cols)
  p_col    <- pchisq(chi2_col, df = R - 1, lower.tail = FALSE)
  
  dim1 <- coords[, 1]
  dim2 <- if (ncp_eff >= 2) coords[, 2] else rep(NA_real_, length(dim1))
  noms_cols <- colnames(xt)
  
  out <- rbind(
    ChiDist   = round(chi_dist, 6),
    Inertia   = round(inertia, 6),
    `Dim. 1`  = round(dim1, 6),
    `Dim. 2`  = round(dim2, 6),
    Chi2_col  = round(chi2_col[noms_cols], 6),
    p_value   = signif(p_col[noms_cols], 6)
  )
  colnames(out) <- noms_cols
  as.data.frame(out, check.names = FALSE)
}

export_tableaux_colonnes_ABC <- function(df_cat, sortie){
  illustratives <- c("Education","Sexe","Age")
  questions     <- c("A","B","C","D")
  for (iv in illustratives){
    for (q in questions){
      tab <- resume_colonnes_CA(df_cat, rows_var = q, cols_var = iv, ncp = 2)
      chemin <- file.path(sortie, sprintf("colonnes_%s_vs_%s.csv", iv, q))
      write.csv(tab, chemin, row.names = TRUE)
    }
  }
  logf("12 fichiers écrits : colonnes_<Illustrative>_vs_<Var>.csv")
}

# ----------------- Impression terminal : tableaux + χ² pour 12 paires -----------------
imprimer_test <- function(nom_lignes, nom_colonnes, tableau, test){
  cat("\n=============================================\n")
  cat(sprintf("Tableau de contingence : %s × %s\n", nom_lignes, nom_colonnes))
  print(tableau)
  cat("\nTest du Chi2 d’indépendance\n")
  cat(sprintf("data:  %s and %s\n", nom_lignes, nom_colonnes))
  cat(sprintf("X-squared = %.3f, df = %d, p-value = %.6f\n",
              as.numeric(test$statistic),
              as.integer(test$parameter),
              as.numeric(test$p.value)))
}

export_tables_et_tests_terminal <- function(dfr, sortie){
  # Lignes déjà codées en A_1.., B_1.., ... on garde telles quelles
  illustratives <- list(Education = dfr$Education, Sexe = dfr$Sexe, Age = dfr$Age)
  questions     <- list(A = dfr$A, B = dfr$B, C = dfr$C, D = dfr$D)
  
  recap <- data.frame(
    illustrative = character(),
    question     = character(),
    chi2         = numeric(),
    df           = integer(),
    p_value      = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (iv_name in names(illustratives)){
    iv <- illustratives[[iv_name]]
    for (q_name in names(questions)){
      qv <- questions[[q_name]]
      tab <- table(qv, iv)  # lignes = A_1..A_5 (ou B_1..B_5, etc.), colonnes = modalités de l'illustrative
      test <- suppressWarnings(chisq.test(tab, correct = FALSE))
      imprimer_test(q_name, iv_name, tab, test)
      write.csv(as.data.frame.matrix(tab),
                file.path(sortie, sprintf("table_%s_vs_%s.csv", q_name, iv_name)),
                row.names = TRUE)
      recap <- rbind(recap, data.frame(
        illustrative = iv_name,
        question     = q_name,
        chi2         = as.numeric(test$statistic),
        df           = as.integer(test$parameter),
        p_value      = as.numeric(test$p.value),
        stringsAsFactors = FALSE
      ))
    }
  }
  write.csv(recap, file.path(sortie, "chi2_recapitulatif.csv"), row.names = FALSE)
  logf("Exports χ² : chi2_recapitulatif.csv + table_<Question>_vs_<Illustrative>.csv")
}

# ===================== Programme principal =====================
main <- function(){
  rep <- rep_script()
  fichier <- file.path(rep, "sciences.csv")
  sortie  <- rep
  
  logf("Début. Lecture et recodage.")
  df <- lire_csv_robuste(fichier)
  logf("Dimensions :", nrow(df), "lignes,", ncol(df), "colonnes.")
  logf("Colonnes :", paste(names(df), collapse=", "))
  
  dfr <- recoder_strict_robuste(df, "A","B","C","D","sex","age","edu")
  write.csv(dfr, file.path(sortie, "donnees_recodées.csv"), row.names = FALSE)
  logf("Recodage terminé.")
  
  # Tables cellule-par-cellule (ABCD × Éducation) avec libellés A_1.., B_1..
  export_tables_cellulaires(dfr, sortie)
  
  # Trois tableaux χ² globaux empilés (Education/Sexe/Age vs A,B,C,D)
  export_chi2_tables_separees_exact(dfr, sortie)
  
  # Tableaux “colonnes” pour chaque illustrative vs A,B,C,D
  export_tableaux_colonnes_ABC(dfr, sortie)
  
  # Impression terminal + exports des tableaux de contingence (lignes A_1.., B_1..)
  export_tables_et_tests_terminal(dfr, sortie)
  
  # AFCM sur TDC (toutes variables actives)
  acm <- FactoMineR::MCA(dfr, ncp = 2, graph = FALSE)
  inert_tdc <- data.frame(axe = paste0("Axe", seq_len(nrow(acm$eig))),
                          part_inertie_pct = round(acm$eig[,2], 1))
  write.csv(inert_tdc, file.path(sortie, "inerties_afcm_tdc.csv"), row.names = FALSE)
  
  mod_tdc <- as.data.frame(acm$var$coord[, 1:2, drop = FALSE])
  mod_tdc$nom <- rownames(mod_tdc)
  names(mod_tdc)[1:2] <- c("Dim1","Dim2")
  
  p_tdc <- ggplot(mod_tdc, aes(Dim1, Dim2, label = nom)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_point(color = COL, shape = FORME, size = TAILLE_POINT) +
    ggrepel::geom_text_repel(color = COL, size = 3.5) +
    theme_blanc() +
    labs(
      title   = "Modalités — AFCM (TDC, toutes actives)",
      x       = sprintf("Axe 1 (%.1f%%)", acm$eig[1,2]),
      y       = sprintf("Axe 2 (%.1f%%)", acm$eig[2,2])
    )
  sauver_png(p_tdc, file.path(sortie, "modalites_TDC_axes12_tout.png"))
  logf(sprintf("AFCM (TDC) — Axe1/Axe2 : %.1f%% / %.1f%%", acm$eig[1,2], acm$eig[2,2]))
  
  # TDC explicite puis Burt
  X <- faire_tdc(dfr, vars = c("A","B","C","D","Sexe","Age","Education"))
  write.csv(X, file.path(sortie, "tableau_disjonctif_complet.csv"), row.names = FALSE)
  
  B <- t(X) %*% X
  write.csv(B, file = file.path(sortie, "tableau_de_burt.csv"))
  
  # AFC sur Burt
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
    theme_blanc() +
    labs(
      title   = "Modalités — AFC (Burt, toutes actives)",
      x       = sprintf("Axe 1 (%.1f%%)", caB$eig[1,2]),
      y       = sprintf("Axe 2 (%.1f%%)", caB$eig[2,2])
    )
  sauver_png(p_burt, file.path(sortie, "modalites_Burt_axes12_tout.png"))
  logf(sprintf("AFC (Burt) — Axe1/Axe2 : %.1f%% / %.1f%%", caB$eig[1,2], caB$eig[2,2]))
  
  logf("Terminé. Les tableaux affichés utilisent le réencodage : A_1.., B_1.., C_1.., D_1..")
}

# Exécution
main()







