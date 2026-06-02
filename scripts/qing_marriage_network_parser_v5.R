# ============================================================
# 清代文学世家姻亲网络：解析 + 诊断
# 目的：
# 1. 从 marriages.txt 解析婚姻记录；
# 2. 修正“后半部分记录误归到上一家族”的问题；
# 3. 识别完整家族标题、无括号家族标题、省份小节、籍贯不详小节、县/府小节；
# 4. 输出四个核心表：
#    network_summary.csv
#    headers_qc.csv
#    family_attributes_clean.csv
#    marriage_edges_clean.csv
# ============================================================

options(stringsAsFactors = FALSE)
set.seed(20260601)

# -----------------------------
# 0. 加载包
# -----------------------------
library(tidyverse)
library(stringr)
library(igraph)
library(readr)

# -----------------------------
# 1. 文件路径
# -----------------------------
input_txt <- "marriages.txt"
out_dir <- "outputs_revised"

if (!file.exists(input_txt)) {
  stop("没有找到 marriages.txt。请确认 marriages.txt 在 /Users/jiawei/Desktop/post 文件夹里。")
}

if (dir.exists(out_dir)) {
  unlink(out_dir, recursive = TRUE)
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 2. 基础函数
# -----------------------------

clean_line <- function(x) {
  x %>%
    str_replace_all("\ufeff", "") %>%
    str_replace_all("　", " ") %>%
    str_replace_all("（", "（") %>%
    str_replace_all("）", "）") %>%
    str_replace_all("【娶1|【娶l|【娶I|【娶」|【娶〕|【娶］", "【娶】") %>%
    str_replace_all("【适1|【适l|【适I|【适」|【适〕|【适］", "【适】") %>%
    str_replace_all("［娶】|\\[娶】|I娶】|1娶】|娶］", "【娶】") %>%
    str_replace_all("［适】|\\[适】|I适】|1适】|需适】|适］", "【适】") %>%
    str_replace_all("【娶\\]", "【娶】") %>%
    str_replace_all("【适\\]", "【适】") %>%
    str_squish()
}

normalize_location <- function(loc) {
  if (length(loc) == 0 || is.na(loc) || loc == "") {
    return("不详")
  }
  
  loc <- as.character(loc)
  loc <- clean_line(loc)
  
  loc <- str_split(loc, "[，,。；;、\\s\\.]", n = 2, simplify = TRUE)[1]
  loc <- str_replace_all(loc, "[\\*\\]］\\[]", "")
  loc <- str_replace_all(loc, "工$|兀$|底$|九$|尢$|\\)$", "")
  loc <- str_squish(loc)
  
  if (is.na(loc) || loc == "") {
    loc <- "不详"
  }
  
  loc
}

province_list <- c(
  "直隶", "江苏", "浙江", "安徽", "江西", "福建", "湖南", "湖北",
  "河南", "山东", "山西", "陕西", "甘肃", "四川", "贵州", "云南",
  "广东", "广西", "北京", "天津", "上海", "长白", "正蓝旗", "镶黄旗"
)

compound_surnames <- c(
  "爱新觉罗", "赫舍里", "钮祜禄", "瓜尔佳", "舒穆禄",
  "欧阳", "司马", "上官", "诸葛", "夏侯", "司徒", "司空",
  "东方", "西门", "南宫", "公孙", "公羊", "完颜", "纳兰",
  "富察", "佟佳", "马佳", "乌拉", "叶赫"
)

extract_surname <- function(x) {
  if (length(x) == 0 || is.na(x) || x == "") {
    return("未知")
  }
  
  x <- as.character(x)
  x <- clean_line(x)
  
  x <- str_remove(x, "^[0-9口lI乱乙色上生\\.．、:：\\s]+")
  x <- str_remove_all(x, "^[“”\"'‘’「」\\[\\]（）()]+")
  x <- str_squish(x)
  
  m <- str_match(x, "^([^氏]{1,4})氏")
  if (!is.na(m[1, 2])) {
    return(m[1, 2])
  }
  
  for (cs in compound_surnames[order(nchar(compound_surnames), decreasing = TRUE)]) {
    if (str_starts(x, fixed(cs))) {
      return(cs)
    }
  }
  
  one <- str_extract(x, "\\p{Han}")
  ifelse(is.na(one), "未知", one)
}

canonical_family <- function(surname, loc) {
  paste0(surname, "氏（", normalize_location(loc), "）")
}

parse_family_header <- function(x) {
  x <- clean_line(x)
  m <- str_match(x, "([^\\s，,。；;、]{1,8}氏)[（(]([^）)]{1,30})[）)]")
  if (is.na(m[1, 2])) {
    return(NA_character_)
  }
  paste0(m[1, 2], "（", normalize_location(m[1, 3]), "）")
}

extract_location_from_family <- function(family_id) {
  m <- str_match(family_id, "[（(]([^）)]{1,30})[）)]")
  ifelse(is.na(m[, 2]), "不详", m[, 2])
}

extract_province <- function(family_id) {
  loc <- extract_location_from_family(family_id)
  out <- rep("其他", length(loc))
  
  for (p in province_list) {
    hit <- str_detect(loc, fixed(p))
    out[hit & out == "其他"] <- p
  }
  
  out
}

make_core_region <- function(province) {
  ifelse(province %in% c("江苏", "浙江", "安徽"), province, "Other")
}

# -----------------------------
# 3. 处理一行多条记录
# -----------------------------

split_entry_line <- function(line) {
  line <- clean_line(line)
  
  # 如果一行里多个编号连续出现，用 SPLIT 切开
  line2 <- str_replace_all(
    line,
    "([。；;])\\s*([0-9口lI乱乙色上生]{2,5}[\\.．、:：]?\\s*[^。；;]{0,80}【)",
    "\\1@@SPLIT@@\\2"
  )
  
  parts <- unlist(str_split(line2, fixed("@@SPLIT@@")))
  parts <- parts[str_detect(parts, "【\\s*(娶|适)\\s*】")]
  
  if (length(parts) == 0 && str_detect(line, "【\\s*(娶|适)\\s*】")) {
    parts <- line
  }
  
  parts
}

# -----------------------------
# 4. 解析单条婚姻记录
# -----------------------------

parse_one_entry <- function(txt,
                            current_header,
                            current_location,
                            infer_anchor_from_left,
                            line_no) {
  txt <- clean_line(txt)
  
  # 去掉开头编号
  entry_no_raw <- str_extract(txt, "^[0-9口lI乱乙色上生]{1,8}[\\.．、:：]?")
  body <- str_remove(txt, "^[0-9口lI乱乙色上生]{1,8}[\\.．、:：]?\\s*")
  body <- clean_line(body)
  
  marker_loc <- str_locate(body, "【\\s*(娶|适)\\s*】")
  
  if (any(is.na(marker_loc))) {
    return(NULL)
  }
  
  marker <- str_sub(body, marker_loc[1, 1], marker_loc[1, 2])
  marriage_type <- str_match(marker, "(娶|适)")[1, 2]
  
  left_text <- str_sub(body, 1, marker_loc[1, 1] - 1)
  right_text <- str_sub(body, marker_loc[1, 2] + 1, str_length(body))
  
  # anchor family
  if (!infer_anchor_from_left && !is.na(current_header) && current_header != "") {
    anchor_family <- current_header
    anchor_is_focal <- TRUE
  } else {
    # 从左侧推断家族
    left_loc_match <- str_match(left_text, "^(.{0,100}?)[（(]([^）)]{1,30})[）)]")
    if (!is.na(left_loc_match[1, 3]) &&
        !str_detect(left_loc_match[1, 3], "《|卷|册|有|清代|家谱|宗谱")) {
      anchor_loc <- normalize_location(left_loc_match[1, 3])
      left_for_surname <- left_loc_match[1, 2]
    } else {
      anchor_loc <- normalize_location(current_location)
      left_for_surname <- left_text
    }
    
    anchor_surname <- extract_surname(left_for_surname)
    anchor_family <- canonical_family(anchor_surname, anchor_loc)
    anchor_is_focal <- FALSE
  }
  
  # partner family
  # 取右侧第一个括号中的地点作为配偶家族地点
  partner_match <- str_match(right_text, "^(.{0,150}?)[（(]([^）)]{1,35})[）)]")
  
  if (!is.na(partner_match[1, 2])) {
    partner_text <- partner_match[1, 2]
    partner_loc <- normalize_location(partner_match[1, 3])
  } else {
    partner_text <- right_text
    partner_loc <- "不详"
  }
  
  partner_surname <- extract_surname(partner_text)
  partner_family <- canonical_family(partner_surname, partner_loc)
  
  # 女性作者线索
  has_writings_any <- str_detect(body, "有《")
  has_writings_left <- str_detect(left_text, "有《")
  has_writings_right <- str_detect(right_text, "有《")
  
  female_writer_family <- NA_character_
  
  if (marriage_type == "娶" && has_writings_right) {
    female_writer_family <- partner_family
  }
  
  if (marriage_type == "适" && has_writings_left) {
    female_writer_family <- anchor_family
  }
  
  uncertain_writer <- has_writings_any && is.na(female_writer_family)
  
  era_hint <- str_extract(body, "顺治|康熙|雍正|乾隆|嘉庆|道光|咸丰|同治|光绪|宣统")
  
  tibble(
    entry_no_raw = entry_no_raw,
    line_no = line_no,
    anchor_family = anchor_family,
    anchor_is_focal = anchor_is_focal,
    partner_family = partner_family,
    marriage_type = marriage_type,
    current_location = normalize_location(current_location),
    left_text = left_text,
    right_text = right_text,
    partner_text = partner_text,
    partner_location = partner_loc,
    has_writings_any = has_writings_any,
    female_writer_family = female_writer_family,
    uncertain_writer = uncertain_writer,
    era_hint = era_hint,
    source_text = body
  )
}

# -----------------------------
# 5. 主解析函数
# -----------------------------

parse_marriage_text <- function(file) {
  lines <- readLines(file, encoding = "UTF-8", warn = FALSE)
  lines <- clean_line(lines)
  
  province_regex <- paste0("^(", paste(province_list, collapse = "|"), ")$")
  bare_family_regex <- "^\\p{Han}{1,8}氏$"
  full_header_regex <- "\\p{Han}{1,8}氏[（(][^）)]{1,30}[）)]"
  junk_page_regex <- "^\\s*[0-9]{1,4}\\s*$"
  
  current_header <- NA_character_
  current_province <- NA_character_
  current_location <- "不详"
  infer_anchor_mode <- TRUE
  
  records_list <- list()
  headers_list <- list()
  
  rec_i <- 1
  h_i <- 1
  
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    
    if (is.na(line) || line == "" || str_detect(line, junk_page_regex)) {
      next
    }
    
    has_marker <- str_detect(line, "【\\s*(娶|适)\\s*】")
    header_hits <- str_extract_all(line, full_header_regex)[[1]]
    
    # A. 完整家族标题：张氏（直隶丰润）
    if (!has_marker && length(header_hits) > 0) {
      selected_header <- tail(header_hits, 1)
      selected_header <- parse_family_header(selected_header)
      
      current_header <- selected_header
      current_location <- extract_location_from_family(current_header)
      current_province <- extract_province(current_header)
      infer_anchor_mode <- FALSE
      
      headers_list[[h_i]] <- tibble(
        line_no = i,
        raw_line = line,
        n_headers = length(header_hits),
        selected_header = current_header,
        all_headers = paste(header_hits, collapse = " | "),
        header_type = "full_header"
      )
      h_i <- h_i + 1
      next
    }
    
    # B. 省份小节：云南、直隶、江苏等
    if (!has_marker && str_detect(line, province_regex)) {
      current_province <- line
      current_location <- line
      current_header <- NA_character_
      infer_anchor_mode <- TRUE
      
      headers_list[[h_i]] <- tibble(
        line_no = i,
        raw_line = line,
        n_headers = 0,
        selected_header = NA_character_,
        all_headers = NA_character_,
        header_type = "province_section"
      )
      h_i <- h_i + 1
      next
    }
    
    # C. 籍贯不详小节
    if (!has_marker && str_detect(line, "^籍贯不详$")) {
      current_province <- NA_character_
      current_location <- "不详"
      current_header <- NA_character_
      infer_anchor_mode <- TRUE
      
      headers_list[[h_i]] <- tibble(
        line_no = i,
        raw_line = line,
        n_headers = 0,
        selected_header = NA_character_,
        all_headers = NA_character_,
        header_type = "unknown_origin_section"
      )
      h_i <- h_i + 1
      next
    }
    
    # D. 无括号家族标题：杨氏、孙氏、王氏
    if (!has_marker && str_detect(line, bare_family_regex)) {
      loc_for_bare <- ifelse(is.na(current_location) || current_location == "", "不详", current_location)
      current_header <- paste0(line, "（", loc_for_bare, "）")
      infer_anchor_mode <- FALSE
      
      headers_list[[h_i]] <- tibble(
        line_no = i,
        raw_line = line,
        n_headers = 1,
        selected_header = current_header,
        all_headers = current_header,
        header_type = "bare_family_header"
      )
      h_i <- h_i + 1
      next
    }
    
    # E. 地点小节：文安、莱阳、甘泉、江都等
    # 条件：纯中文、较短、不含婚姻标记、不像书名或章节名
    if (!has_marker &&
        str_detect(line, "^\\p{Han}{1,12}$") &&
        !str_detect(line, "上编|下编|部分|资料|家谱|宗谱|诗|文|卷|册|目录|清代|文学|世家|意义")) {
      
      if (!is.na(current_province) &&
          current_province != "" &&
          !str_detect(line, fixed(current_province))) {
        current_location <- paste0(current_province, line)
      } else {
        current_location <- line
      }
      
      current_header <- NA_character_
      infer_anchor_mode <- TRUE
      
      headers_list[[h_i]] <- tibble(
        line_no = i,
        raw_line = line,
        n_headers = 0,
        selected_header = NA_character_,
        all_headers = NA_character_,
        header_type = "location_section"
      )
      h_i <- h_i + 1
      next
    }
    
    # F. 无婚姻标记，不处理
    if (!has_marker) {
      next
    }
    
    # G. 解析婚姻条目
    parts <- split_entry_line(line)
    for (part in parts) {
      rec <- parse_one_entry(
        txt = part,
        current_header = current_header,
        current_location = current_location,
        infer_anchor_from_left = infer_anchor_mode || is.na(current_header),
        line_no = i
      )
      
      if (!is.null(rec)) {
        records_list[[rec_i]] <- rec
        rec_i <- rec_i + 1
      }
    }
  }
  
  records <- bind_rows(records_list)
  headers <- bind_rows(headers_list)
  
  list(records = records, headers = headers)
}

# -----------------------------
# 6. 执行解析
# -----------------------------

parsed <- parse_marriage_text(input_txt)
records <- parsed$records
headers_qc <- parsed$headers

# -----------------------------
# 7. 构建边表
# -----------------------------

records <- records %>%
  filter(
    !is.na(anchor_family),
    !is.na(partner_family),
    anchor_family != "",
    partner_family != ""
  )

focal_families <- records %>%
  filter(anchor_is_focal) %>%
  distinct(anchor_family) %>%
  rename(family_id = anchor_family) %>%
  mutate(is_focal_family = TRUE)

edges_raw <- records %>%
  mutate(
    family_A = pmin(anchor_family, partner_family),
    family_B = pmax(anchor_family, partner_family),
    is_self_loop = family_A == family_B
  )

self_loops <- edges_raw %>% filter(is_self_loop)

edges <- edges_raw %>%
  filter(!is_self_loop) %>%
  group_by(family_A, family_B) %>%
  summarise(
    weight = n(),
    marriage_types = paste(sort(unique(marriage_type)), collapse = ";"),
    n_writing_records = sum(has_writings_any, na.rm = TRUE),
    source_lines = paste(line_no, collapse = ";"),
    .groups = "drop"
  ) %>%
  mutate(
    A_is_focal = family_A %in% focal_families$family_id,
    B_is_focal = family_B %in% focal_families$family_id,
    edge_scope = ifelse(
      A_is_focal & B_is_focal,
      "within_focal_network",
      "external_to_nonfocal_family"
    )
  )

# -----------------------------
# 8. 构建节点表和网络指标
# -----------------------------

all_nodes <- tibble(
  family_id = sort(unique(c(edges$family_A, edges$family_B)))
) %>%
  mutate(
    location = extract_location_from_family(family_id),
    province = extract_province(family_id),
    core_region = make_core_region(province)
  ) %>%
  left_join(focal_families, by = "family_id") %>%
  mutate(
    is_focal_family = ifelse(is.na(is_focal_family), FALSE, is_focal_family)
  )

g <- graph_from_data_frame(
  d = edges %>% select(from = family_A, to = family_B, weight),
  directed = FALSE,
  vertices = all_nodes
)

E(g)$distance <- 1 / E(g)$weight

comp <- components(g)

if (vcount(g) > 0) {
  giant_id <- which.max(comp$csize)
  g_giant <- induced_subgraph(g, vids = V(g)[comp$membership == giant_id])
} else {
  giant_id <- NA_integer_
  g_giant <- g
}

node_metrics <- tibble(
  family_id = V(g)$name,
  unweighted_degree = degree(g, mode = "all", loops = FALSE),
  weighted_degree = strength(g, mode = "all", weights = E(g)$weight, loops = FALSE),
  betweenness = betweenness(g, directed = FALSE, weights = E(g)$distance, normalized = TRUE),
  eigenvector = eigen_centrality(g, directed = FALSE, weights = E(g)$weight)$vector,
  component_id = comp$membership,
  in_giant_component = comp$membership == giant_id
)

family_df <- all_nodes %>%
  left_join(node_metrics, by = "family_id") %>%
  arrange(desc(weighted_degree))

# -----------------------------
# 9. 网络汇总表
# -----------------------------

safe_mean_distance <- function(graph) {
  if (vcount(graph) <= 1) return(NA_real_)
  mean_distance(graph, directed = FALSE, weights = E(graph)$distance)
}

safe_diameter <- function(graph) {
  if (vcount(graph) <= 1) return(NA_real_)
  diameter(graph, directed = FALSE, weights = E(graph)$distance)
}

safe_transitivity <- function(graph) {
  if (vcount(graph) <= 2) return(NA_real_)
  transitivity(graph, type = "global")
}

safe_modularity <- function(graph) {
  if (ecount(graph) == 0 || vcount(graph) <= 2) return(NA_real_)
  modularity(cluster_louvain(graph, weights = E(graph)$weight))
}

network_summary <- tibble(
  metric = c(
    "parser_version",
    "raw_parsed_records",
    "self_loops_excluded",
    "nodes_all",
    "focal_families_from_headers",
    "edges_distinct_all",
    "edges_within_focal_network",
    "edges_external_to_nonfocal_family",
    "sum_edge_weights_all",
    "giant_component_nodes",
    "giant_component_edges",
    "average_path_length_giant",
    "diameter_giant",
    "global_clustering_transitivity",
    "louvain_modularity"
  ),
  value = c(
    "v5",
    as.character(nrow(records)),
    as.character(nrow(self_loops)),
    as.character(vcount(g)),
    as.character(nrow(focal_families)),
    as.character(nrow(edges)),
    as.character(sum(edges$edge_scope == "within_focal_network")),
    as.character(sum(edges$edge_scope == "external_to_nonfocal_family")),
    as.character(sum(edges$weight)),
    as.character(vcount(g_giant)),
    as.character(ecount(g_giant)),
    as.character(safe_mean_distance(g_giant)),
    as.character(safe_diameter(g_giant)),
    as.character(safe_transitivity(g)),
    as.character(safe_modularity(g))
  )
)

# -----------------------------
# 10. 额外诊断：高 weighted_degree 节点
# -----------------------------

top_weighted_degree <- family_df %>%
  arrange(desc(weighted_degree)) %>%
  select(
    family_id,
    province,
    location,
    is_focal_family,
    weighted_degree,
    unweighted_degree,
    betweenness,
    eigenvector
  ) %>%
  head(50)

suspect_nodes <- family_df %>%
  filter(
    weighted_degree >= 100 |
      str_detect(family_id, "云南皆宁|未知|不详")
  ) %>%
  arrange(desc(weighted_degree)) %>%
  select(
    family_id,
    province,
    location,
    is_focal_family,
    weighted_degree,
    unweighted_degree,
    betweenness,
    eigenvector
  )

# -----------------------------
# 11. 输出文件
# -----------------------------

write_csv(network_summary, file.path(out_dir, "network_summary.csv"))
write_csv(headers_qc, file.path(out_dir, "headers_qc.csv"))
write_csv(records, file.path(out_dir, "raw_entries_clean.csv"))
write_csv(edges, file.path(out_dir, "marriage_edges_clean.csv"))
write_csv(family_df, file.path(out_dir, "family_attributes_clean.csv"))
write_csv(self_loops, file.path(out_dir, "self_loops_excluded.csv"))
write_csv(top_weighted_degree, file.path(out_dir, "top_weighted_degree.csv"))
write_csv(suspect_nodes, file.path(out_dir, "suspect_nodes.csv"))

# 输出一个标记文件，防止和旧版本混淆
writeLines(
  c(
    "qing_marriage_network_parser_v5.R",
    paste0("Run time: ", Sys.time()),
    "This is parser v5. headers_qc.csv must contain header_type column."
  ),
  file.path(out_dir, "RUN_VERSION_v5.txt")
)

# -----------------------------
# 12. 控制台显示结果
# -----------------------------

cat("\n============================================================\n")
cat("清代文学世家姻亲网络解析完成：parser v5\n")
cat("============================================================\n")
cat("输出文件夹：", out_dir, "\n")
cat("原始解析记录数：", nrow(records), "\n")
cat("排除自环数：", nrow(self_loops), "\n")
cat("全部节点数：", vcount(g), "\n")
cat("核心标题家族数：", nrow(focal_families), "\n")
cat("全部去重边数：", nrow(edges), "\n")
cat("核心网络边数：", sum(edges$edge_scope == "within_focal_network"), "\n")
cat("外部姻亲边数：", sum(edges$edge_scope == "external_to_nonfocal_family"), "\n")
cat("边权重总和：", sum(edges$weight), "\n")
cat("headers_qc 列名：", paste(names(headers_qc), collapse = ", "), "\n")
cat("\n请重点检查：outputs_revised/network_summary.csv\n")
cat("请重点检查：outputs_revised/headers_qc.csv\n")
cat("请重点检查：outputs_revised/top_weighted_degree.csv\n")
cat("请重点检查：outputs_revised/suspect_nodes.csv\n")
cat("============================================================\n")