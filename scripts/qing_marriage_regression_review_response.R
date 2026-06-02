# ============================================================
# qing_marriage_regression_review_response.R
# 统计分析、稳健性检验、异常案例、韧性模拟
# ============================================================
# 本脚本基于 parser v5 生成的 clean 数据运行，不再重新解析 marriages.txt。
# 输入文件：
#   outputs_revised/family_attributes_clean.csv
#   outputs_revised/marriage_edges_clean.csv
#   outputs_revised/raw_entries_clean.csv
#
# 输出文件夹：
#   outputs_review_response/
# ============================================================

options(stringsAsFactors = FALSE)
set.seed(20260601)

# -----------------------------
# 0. 加载包
# -----------------------------

library(tidyverse)
library(readr)
library(stringr)
library(igraph)
library(broom)
library(lmtest)
library(sandwich)
library(clubSandwich)
library(car)
library(ggplot2)

# -----------------------------
# 1. 路径设置
# -----------------------------

input_dir <- "outputs_revised"
out_dir <- "outputs_review_response"

if (!dir.exists(input_dir)) {
  stop("没有找到 outputs_revised 文件夹。请先运行 parser v5。")
}

if (dir.exists(out_dir)) {
  unlink(out_dir, recursive = TRUE)
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

family_file <- file.path(input_dir, "family_attributes_clean.csv")
edges_file  <- file.path(input_dir, "marriage_edges_clean.csv")
raw_file    <- file.path(input_dir, "raw_entries_clean.csv")
summary_file <- file.path(input_dir, "network_summary.csv")

required_files <- c(family_file, edges_file, raw_file)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "以下文件缺失，请确认 parser v5 已成功运行：\n",
      paste(missing_files, collapse = "\n")
    )
  )
}

family_df <- read_csv(family_file, show_col_types = FALSE)
edges_df  <- read_csv(edges_file, show_col_types = FALSE)
raw_df    <- read_csv(raw_file, show_col_types = FALSE)

if (file.exists(summary_file)) {
  network_summary <- read_csv(summary_file, show_col_types = FALSE)
} else {
  network_summary <- tibble(metric = character(), value = character())
}

# -----------------------------
# 2. 基础清洗与变量检查
# -----------------------------

needed_family_cols <- c(
  "family_id", "province", "core_region", "is_focal_family",
  "unweighted_degree", "weighted_degree", "betweenness", "eigenvector"
)

missing_cols <- setdiff(needed_family_cols, names(family_df))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "family_attributes_clean.csv 缺少以下列：\n",
      paste(missing_cols, collapse = "\n")
    )
  )
}

family_df <- family_df %>%
  mutate(
    family_id = as.character(family_id),
    province = as.character(province),
    core_region = as.character(core_region),
    is_focal_family = as.logical(is_focal_family),
    unweighted_degree = as.numeric(unweighted_degree),
    weighted_degree = as.numeric(weighted_degree),
    betweenness = as.numeric(betweenness),
    eigenvector = as.numeric(eigenvector)
  )

raw_df <- raw_df %>%
  mutate(
    anchor_family = as.character(anchor_family),
    partner_family = as.character(partner_family),
    female_writer_family = as.character(female_writer_family),
    has_writings_any = as.logical(has_writings_any),
    uncertain_writer = as.logical(uncertain_writer)
  )

# -----------------------------
# 3. 构建 Literary Capital Proxy, LCP
# -----------------------------
# 说明：
# 当前 clean 数据不能完整重建 jinshi_count / juren_count。
# 因此修回稿建议把 LCS 改为 Literary Capital Proxy, LCP。
# LCP 只使用 OCR 文本中可复现的文学著作线索和女性作者线索。
#
# confirmed_female_writer_count:
#   根据 parser v5 可判定女性作者所属家族的记录数。
#
# literary_clue_count:
#   所有出现“有《...》”的记录。
#   若可判定 female_writer_family，则归入该家族；
#   若不能判定，则保守归入 anchor_family，作为 uncertain literary clue。
#
# uncertain_literary_clue_count:
#   出现著作线索但无法明确归属女性作者家族的记录数。
# -----------------------------

female_writer_counts <- raw_df %>%
  filter(!is.na(female_writer_family), female_writer_family != "") %>%
  count(family_id = female_writer_family, name = "confirmed_female_writer_count")

literary_clues <- raw_df %>%
  filter(has_writings_any) %>%
  mutate(
    literary_family = case_when(
      !is.na(female_writer_family) & female_writer_family != "" ~ female_writer_family,
      TRUE ~ anchor_family
    )
  ) %>%
  count(family_id = literary_family, name = "literary_clue_count")

uncertain_literary_clues <- raw_df %>%
  filter(has_writings_any, uncertain_writer) %>%
  count(family_id = anchor_family, name = "uncertain_literary_clue_count")

family_df <- family_df %>%
  left_join(female_writer_counts, by = "family_id") %>%
  left_join(literary_clues, by = "family_id") %>%
  left_join(uncertain_literary_clues, by = "family_id") %>%
  mutate(
    confirmed_female_writer_count = replace_na(confirmed_female_writer_count, 0),
    literary_clue_count = replace_na(literary_clue_count, 0),
    uncertain_literary_clue_count = replace_na(uncertain_literary_clue_count, 0),
    has_confirmed_female_writer = ifelse(confirmed_female_writer_count > 0, 1, 0)
  )

minmax_log <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- 0
  z <- log1p(x)
  
  if (length(unique(z)) <= 1) {
    return(rep(0, length(z)))
  }
  
  (z - min(z, na.rm = TRUE)) / (max(z, na.rm = TRUE) - min(z, na.rm = TRUE))
}

zscore_safe <- function(x) {
  x <- as.numeric(x)
  if (sd(x, na.rm = TRUE) == 0 || all(is.na(x))) {
    return(rep(0, length(x)))
  }
  as.numeric(scale(x))
}

family_df <- family_df %>%
  mutate(
    literary_norm = minmax_log(literary_clue_count),
    female_norm = minmax_log(confirmed_female_writer_count),
    female_binary = has_confirmed_female_writer,
    
    # 主分析：文学线索 0.6 + 女性作者线索 0.4
    LCP_main_0604 = 0.6 * literary_norm + 0.4 * female_norm,
    
    # 敏感性分析 1：等权
    LCP_equal_0505 = 0.5 * literary_norm + 0.5 * female_norm,
    
    # 敏感性分析 2：更强调文学产出
    LCP_literary_heavy_0802 = 0.8 * literary_norm + 0.2 * female_norm,
    
    # 敏感性分析 3：更强调女性作者
    LCP_female_heavy_0208 = 0.2 * literary_norm + 0.8 * female_norm,
    
    # 敏感性分析 4：女性作者用二元变量
    LCP_binary_female_0703 = 0.7 * literary_norm + 0.3 * female_binary,
    
    # family-size / source-exposure proxy
    # 注意：这不是严格人口规模，而是记录暴露量代理指标。
    record_exposure_log = log1p(weighted_degree),
    
    z_weighted_degree = zscore_safe(weighted_degree),
    z_unweighted_degree = zscore_safe(unweighted_degree),
    z_betweenness = zscore_safe(betweenness),
    z_eigenvector = zscore_safe(eigenvector),
    z_record_exposure = zscore_safe(record_exposure_log)
  )

# -----------------------------
# 4. PCA-LCP，可用则输出，不可用则跳过
# -----------------------------

pca_input <- family_df %>%
  transmute(
    literary = literary_norm,
    female = female_norm,
    female_binary = female_binary
  )

pca_sd <- sapply(pca_input, sd, na.rm = TRUE)
pca_keep <- names(pca_sd)[is.finite(pca_sd) & pca_sd > 0]

if (length(pca_keep) >= 2) {
  pca_model <- prcomp(
    as.data.frame(pca_input[, pca_keep, drop = FALSE]),
    center = TRUE,
    scale. = TRUE
  )
  
  pca_score <- pca_model$x[, 1]
  
  if (max(pca_score, na.rm = TRUE) == min(pca_score, na.rm = TRUE)) {
    family_df$LCP_pca <- NA_real_
  } else {
    family_df$LCP_pca <- (pca_score - min(pca_score, na.rm = TRUE)) /
      (max(pca_score, na.rm = TRUE) - min(pca_score, na.rm = TRUE))
  }
  
  pca_loadings <- as.data.frame(pca_model$rotation) %>%
    rownames_to_column("variable")
} else {
  family_df$LCP_pca <- NA_real_
  pca_loadings <- tibble(variable = character())
}

# -----------------------------
# 5. 准备回归数据
# -----------------------------

reg_df <- family_df %>%
  filter(is_focal_family) %>%
  mutate(
    province = ifelse(is.na(province) | province == "", "其他", province),
    province = factor(province),
    core_region = case_when(
      core_region %in% c("江苏", "浙江", "安徽") ~ core_region,
      TRUE ~ "Other"
    ),
    core_region = factor(core_region, levels = c("江苏", "浙江", "安徽", "Other"))
  )

write_csv(reg_df, file.path(out_dir, "regression_dataset_focal_families.csv"))

# -----------------------------
# 6. 回归函数：OLS + province-clustered robust SE
# -----------------------------

fit_models_for_y <- function(data, yvar) {

  d <- data %>%
    filter(!is.na(.data[[yvar]]))

  if (nrow(d) < 30) {
    warning('样本量过小，跳过：', yvar)
    return(list(models = list(), results = tibble()))
  }

  if (sd(d[[yvar]], na.rm = TRUE) == 0) {
    warning('因变量无变化，跳过：', yvar)
    return(list(models = list(), results = tibble()))
  }

  m1 <- lm(
    as.formula(paste0(yvar, ' ~ z_weighted_degree + core_region')),
    data = d
  )

  m2 <- lm(
    as.formula(
      paste0(
        yvar,
        ' ~ z_weighted_degree + z_betweenness + z_eigenvector + core_region'
      )
    ),
    data = d
  )

  m3 <- lm(
    as.formula(
      paste0(
        yvar,
        ' ~ z_unweighted_degree + z_betweenness + z_eigenvector + ',
        'z_record_exposure + core_region'
      )
    ),
    data = d
  )

  m4 <- lm(
    as.formula(
      paste0(
        yvar,
        ' ~ z_unweighted_degree + z_betweenness + z_eigenvector + core_region'
      )
    ),
    data = d
  )

  models <- list(
    Model_1_weighted_degree = m1,
    Model_2_centrality = m2,
    Model_3_exposure_adjusted = m3,
    Model_4_unweighted_degree = m4
  )

  extract_clustered <- function(model_obj, model_name) {

    mf <- model.frame(model_obj)
    row_ids <- suppressWarnings(as.integer(rownames(mf)))

    if (all(!is.na(row_ids)) && max(row_ids, na.rm = TRUE) <= nrow(d)) {
      cluster_vec <- d$province[row_ids]
    } else {
      cluster_vec <- d$province[seq_len(nrow(mf))]
    }

    n_obs <- stats::nobs(model_obj)
    r_sq <- summary(model_obj)$r.squared
    adj_r_sq <- summary(model_obj)$adj.r.squared
    n_clusters <- dplyr::n_distinct(cluster_vec)

    out <- tryCatch(
      {
        V <- clubSandwich::vcovCR(
          model_obj,
          cluster = cluster_vec,
          type = 'CR2'
        )

        clubSandwich::coef_test(
          model_obj,
          vcov = V,
          test = 'Satterthwaite'
        ) %>%
          as_tibble(rownames = 'term') %>%
          rename(
            estimate = beta,
            std_error = SE,
            statistic = tstat,
            p_value = p_Satt
          )
      },
      error = function(e) {
        V <- sandwich::vcovCL(
          model_obj,
          cluster = cluster_vec,
          type = 'HC1'
        )

        lmtest::coeftest(model_obj, vcov. = V) %>%
          broom::tidy() %>%
          rename(
            std_error = std.error,
            p_value = p.value
          )
      }
    )

    out %>%
      mutate(
        dependent_variable = yvar,
        model = model_name,
        n = n_obs,
        r_squared = r_sq,
        adj_r_squared = adj_r_sq,
        n_province_clusters = n_clusters,
        se_type = 'province-clustered robust SE'
      )
  }

  results <- imap_dfr(models, extract_clustered)

  list(models = models, results = results)
}
# -----------------------------
# 7. 主回归 + LCP 权重敏感性
# -----------------------------

lcp_vars <- c(
  "LCP_main_0604",
  "LCP_equal_0505",
  "LCP_literary_heavy_0802",
  "LCP_female_heavy_0208",
  "LCP_binary_female_0703",
  "LCP_pca"
)

all_model_outputs <- list()

for (yv in lcp_vars) {
  if (yv %in% names(reg_df) && !all(is.na(reg_df[[yv]]))) {
    all_model_outputs[[yv]] <- fit_models_for_y(reg_df, yv)
  }
}

regression_results_clustered <- bind_rows(
  lapply(all_model_outputs, function(x) x$results)
)

write_csv(
  regression_results_clustered,
  file.path(out_dir, "regression_results_clustered_all_LCP.csv")
)

# 主表：只保留主因变量 LCP_main_0604
main_regression_table <- regression_results_clustered %>%
  filter(dependent_variable == "LCP_main_0604") %>%
  select(
    dependent_variable,
    model,
    term,
    estimate,
    std_error,
    statistic,
    p_value,
    n,
    r_squared,
    adj_r_squared,
    n_province_clusters,
    se_type
  )

write_csv(
  main_regression_table,
  file.path(out_dir, "Table_5_revised_main_regression.csv")
)

# 敏感性摘要：核心变量的方向和显著性
sensitivity_key_terms <- regression_results_clustered %>%
  filter(
    term %in% c(
      "z_weighted_degree",
      "z_unweighted_degree",
      "z_betweenness",
      "z_eigenvector",
      "z_record_exposure"
    )
  ) %>%
  mutate(
    significant_005 = ifelse(p_value < 0.05, "Yes", "No"),
    direction = ifelse(estimate > 0, "positive", "negative")
  ) %>%
  select(
    dependent_variable,
    model,
    term,
    estimate,
    std_error,
    p_value,
    direction,
    significant_005,
    n,
    r_squared
  )

write_csv(
  sensitivity_key_terms,
  file.path(out_dir, "LCP_sensitivity_key_terms.csv")
)

# -----------------------------
# 8. 模型诊断：以主模型 Model 3 为准
# -----------------------------

main_model <- all_model_outputs[["LCP_main_0604"]]$models$Model_3_exposure_adjusted

diagnostic_table <- tibble(
  diagnostic = c(
    "N",
    "R_squared",
    "Adjusted_R_squared",
    "Breusch_Pagan_p",
    "Max_Cooks_distance",
    "Mean_Cooks_distance",
    "Number_of_province_clusters"
  ),
  value = c(
    nobs(main_model),
    summary(main_model)$r.squared,
    summary(main_model)$adj.r.squared,
    tryCatch(lmtest::bptest(main_model)$p.value, error = function(e) NA_real_),
    max(cooks.distance(main_model), na.rm = TRUE),
    mean(cooks.distance(main_model), na.rm = TRUE),
    n_distinct(model.frame(main_model)$province)
  )
)

write_csv(
  diagnostic_table,
  file.path(out_dir, "diagnostics_table_main_model.csv")
)

# VIF
vif_table <- tryCatch(
  {
    vif_obj <- car::vif(main_model)
    
    if (is.matrix(vif_obj)) {
      as.data.frame(vif_obj) %>%
        rownames_to_column("term")
    } else {
      tibble(
        term = names(vif_obj),
        VIF = as.numeric(vif_obj)
      )
    }
  },
  error = function(e) {
    tibble(
      term = "VIF_failed",
      VIF = NA_real_,
      note = e$message
    )
  }
)

write_csv(
  vif_table,
  file.path(out_dir, "vif_table_main_model.csv")
)

# 残差图
png(
  file.path(out_dir, "diagnostic_residuals_vs_fitted.png"),
  width = 1800,
  height = 1400,
  res = 220
)

plot(
  fitted(main_model),
  resid(main_model),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs Fitted: Main LCP Model"
)
abline(h = 0, lty = 2)

dev.off()

# QQ 图
png(
  file.path(out_dir, "diagnostic_qqplot.png"),
  width = 1800,
  height = 1400,
  res = 220
)

qqnorm(resid(main_model), main = "Normal Q-Q Plot: Main LCP Model")
qqline(resid(main_model), lty = 2)

dev.off()

# Cook's distance 前 30
cooks_table <- tibble(
  row_id = seq_along(cooks.distance(main_model)),
  cooks_distance = as.numeric(cooks.distance(main_model))
) %>%
  bind_cols(model.frame(main_model) %>% as_tibble()) %>%
  arrange(desc(cooks_distance)) %>%
  head(30)

write_csv(
  cooks_table,
  file.path(out_dir, "top_cooks_distance_cases.csv")
)

# -----------------------------
# 9. Negative-control / discordant cases
# -----------------------------
# 审稿人要求关注：
# high centrality but low LCS/LCP
# low centrality but high LCS/LCP
# 这里用 weighted_degree 和 LCP_main_0604 的四分位数定义。
# -----------------------------

negative_control_cases <- reg_df %>%
  mutate(
    degree_quartile = ntile(weighted_degree, 4),
    lcp_quartile = ntile(LCP_main_0604, 4),
    discordant_case_type = case_when(
      degree_quartile == 4 & lcp_quartile == 1 ~ "high_centrality_low_LCP",
      degree_quartile == 1 & lcp_quartile == 4 ~ "low_centrality_high_LCP",
      degree_quartile == 4 & lcp_quartile == 4 ~ "high_centrality_high_LCP",
      degree_quartile == 1 & lcp_quartile == 1 ~ "low_centrality_low_LCP",
      TRUE ~ "middle"
    )
  ) %>%
  filter(discordant_case_type != "middle") %>%
  arrange(discordant_case_type, desc(weighted_degree)) %>%
  select(
    discordant_case_type,
    family_id,
    province,
    core_region,
    weighted_degree,
    unweighted_degree,
    betweenness,
    eigenvector,
    literary_clue_count,
    confirmed_female_writer_count,
    uncertain_literary_clue_count,
    LCP_main_0604,
    LCP_equal_0505,
    LCP_literary_heavy_0802,
    LCP_female_heavy_0208
  )

write_csv(
  negative_control_cases,
  file.path(out_dir, "negative_control_discordant_cases.csv")
)

negative_control_summary <- negative_control_cases %>%
  count(discordant_case_type, name = "n")

write_csv(
  negative_control_summary,
  file.path(out_dir, "negative_control_summary.csv")
)

# -----------------------------
# 10. 节点移除韧性模拟
# -----------------------------
# 用于把“韧性”改成结构韧性，而不是历史冲击因果。
# targeted removal: 移除 weighted degree 最高的节点
# random removal: 随机移除同等比例节点
# -----------------------------

g <- graph_from_data_frame(
  edges_df %>% select(from = family_A, to = family_B, weight),
  directed = FALSE
)

E(g)$weight <- as.numeric(E(g)$weight)

largest_component_ratio <- function(graph) {
  if (vcount(graph) == 0) {
    return(0)
  }
  
  comp <- components(graph)
  max(comp$csize) / vcount(graph)
}

node_removal_simulation <- function(graph,
                                    fractions = seq(0, 0.5, by = 0.05),
                                    n_random = 100) {
  
  node_names <- V(graph)$name
  strength_order <- names(sort(strength(graph, weights = E(graph)$weight), decreasing = TRUE))
  
  targeted <- map_dfr(fractions, function(fr) {
    n_remove <- floor(vcount(graph) * fr)
    
    remove_nodes <- if (n_remove > 0) {
      strength_order[seq_len(n_remove)]
    } else {
      character(0)
    }
    
    g2 <- delete_vertices(graph, remove_nodes)
    
    tibble(
      strategy = "targeted_high_weighted_degree",
      fraction_removed = fr,
      largest_component_ratio = largest_component_ratio(g2),
      random_sd = NA_real_
    )
  })
  
  random <- map_dfr(fractions, function(fr) {
    n_remove <- floor(vcount(graph) * fr)
    
    values <- replicate(n_random, {
      remove_nodes <- if (n_remove > 0) {
        sample(node_names, n_remove)
      } else {
        character(0)
      }
      
      g2 <- delete_vertices(graph, remove_nodes)
      largest_component_ratio(g2)
    })
    
    tibble(
      strategy = "random_removal_mean",
      fraction_removed = fr,
      largest_component_ratio = mean(values),
      random_sd = sd(values)
    )
  })
  
  bind_rows(targeted, random)
}

resilience_curve <- node_removal_simulation(g)

write_csv(
  resilience_curve,
  file.path(out_dir, "node_removal_resilience_curve.csv")
)

p_resilience <- ggplot(
  resilience_curve,
  aes(
    x = fraction_removed,
    y = largest_component_ratio,
    linetype = strategy
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  labs(
    x = "Fraction of removed families",
    y = "Largest component ratio",
    title = "Structural robustness of the Qing literary marriage network"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  file.path(out_dir, "node_removal_resilience_curve.png"),
  p_resilience,
  width = 7,
  height = 5,
  dpi = 300
)

# -----------------------------
# 11. 可直接放进论文的表格
# -----------------------------

# Table 1 revised: 网络基本属性
table_1_network <- tibble(
  indicator = c(
    "Parseable marriage records",
    "Self-loops excluded",
    "Total family nodes, including non-focal marriage partners",
    "Focal literary-family nodes",
    "Distinct weighted family-pair edges",
    "Within-focal-network edges",
    "External/non-focal-family edges",
    "Sum of edge weights",
    "Giant component nodes",
    "Giant component edges",
    "Average path length, giant component",
    "Diameter, giant component",
    "Global clustering coefficient",
    "Louvain modularity"
  ),
  value = c(
    network_summary$value[match("raw_parsed_records", network_summary$metric)],
    network_summary$value[match("self_loops_excluded", network_summary$metric)],
    network_summary$value[match("nodes_all", network_summary$metric)],
    network_summary$value[match("focal_families_from_headers", network_summary$metric)],
    network_summary$value[match("edges_distinct_all", network_summary$metric)],
    network_summary$value[match("edges_within_focal_network", network_summary$metric)],
    network_summary$value[match("edges_external_to_nonfocal_family", network_summary$metric)],
    network_summary$value[match("sum_edge_weights_all", network_summary$metric)],
    network_summary$value[match("giant_component_nodes", network_summary$metric)],
    network_summary$value[match("giant_component_edges", network_summary$metric)],
    network_summary$value[match("average_path_length_giant", network_summary$metric)],
    network_summary$value[match("diameter_giant", network_summary$metric)],
    network_summary$value[match("global_clustering_transitivity", network_summary$metric)],
    network_summary$value[match("louvain_modularity", network_summary$metric)]
  )
)

write_csv(
  table_1_network,
  file.path(out_dir, "Table_1_revised_network_attributes.csv")
)

# Table 3 revised: 中心性前 20
table_3_centrality <- family_df %>%
  filter(is_focal_family) %>%
  arrange(desc(weighted_degree)) %>%
  select(
    family_id,
    province,
    core_region,
    weighted_degree,
    unweighted_degree,
    betweenness,
    eigenvector,
    literary_clue_count,
    confirmed_female_writer_count,
    LCP_main_0604
  ) %>%
  head(20)

write_csv(
  table_3_centrality,
  file.path(out_dir, "Table_3_revised_top_centrality.csv")
)

# Table S1: LCP 组成项
table_s1_lcp_components <- family_df %>%
  filter(is_focal_family) %>%
  select(
    family_id,
    province,
    weighted_degree,
    unweighted_degree,
    literary_clue_count,
    confirmed_female_writer_count,
    uncertain_literary_clue_count,
    LCP_main_0604,
    LCP_equal_0505,
    LCP_literary_heavy_0802,
    LCP_female_heavy_0208,
    LCP_binary_female_0703,
    LCP_pca
  ) %>%
  arrange(desc(LCP_main_0604))

write_csv(
  table_s1_lcp_components,
  file.path(out_dir, "Table_S1_LCP_components_and_variants.csv")
)

# -----------------------------
# 12. README：解释
# -----------------------------

readme_text <- c(
  "# Review-response statistical outputs",
  "",
  "This folder contains statistical analyses based on the cleaned parser-v5 dataset.",
  "",
  "## Key methodological change",
  "The original manuscript used the term Literary Capital Score (LCS).",
  "Because the current reproducible OCR-based dataset does not fully reconstruct jinshi/juren examination counts,",
  "the revised analysis uses a more conservative Literary Capital Proxy (LCP), based on observable literary-output clues and confirmed female-writer clues extracted from the source text.",
  "",
  "## Main files",
  "- regression_dataset_focal_families.csv: focal-family regression dataset.",
  "- Table_1_revised_network_attributes.csv: revised network summary table.",
  "- Table_3_revised_top_centrality.csv: centrality ranking with weighted and unweighted degree.",
  "- Table_5_revised_main_regression.csv: main OLS models with province-clustered robust standard errors.",
  "- LCP_sensitivity_key_terms.csv: sensitivity analysis across alternative LCP definitions.",
  "- diagnostics_table_main_model.csv: model diagnostics.",
  "- vif_table_main_model.csv: VIF/GVIF table.",
  "- negative_control_discordant_cases.csv: high-centrality/low-LCP and low-centrality/high-LCP cases.",
  "- node_removal_resilience_curve.csv/.png: structural robustness simulation.",
  "",
  "## Recommended wording",
  "The revised manuscript should describe the findings as associations rather than causal effects.",
  "The Taiping Rebellion and resilience discussion should be softened unless direct temporal evidence is added.",
  "The LCP should not be described as including examination success unless jinshi/juren counts are manually added."
)

writeLines(
  readme_text,
  file.path(out_dir, "README_review_response_outputs.md")
)

# -----------------------------
# 13. 控制台输出
# -----------------------------

cat("\n============================================================\n")
cat("审稿意见回应版统计分析完成\n")
cat("============================================================\n")
cat("输入文件夹：", input_dir, "\n")
cat("输出文件夹：", out_dir, "\n")
cat("回归样本：focal literary-family nodes\n")
cat("回归样本量：", nrow(reg_df), "\n")
cat("LCP 主变量：LCP_main_0604 = 0.6 * literary clue + 0.4 * female writer clue\n")
cat("标准误：province-clustered robust standard errors\n")
cat("\n请重点查看以下文件：\n")
cat("1. outputs_review_response/Table_1_revised_network_attributes.csv\n")
cat("2. outputs_review_response/Table_3_revised_top_centrality.csv\n")
cat("3. outputs_review_response/Table_5_revised_main_regression.csv\n")
cat("4. outputs_review_response/LCP_sensitivity_key_terms.csv\n")
cat("5. outputs_review_response/negative_control_discordant_cases.csv\n")
cat("6. outputs_review_response/diagnostics_table_main_model.csv\n")
cat("7. outputs_review_response/node_removal_resilience_curve.png\n")
cat("============================================================\n")
