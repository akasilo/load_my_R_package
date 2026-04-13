# ============================================================
# 강원대 + 한림대 CDM 데이터 전처리 및 공통 검사 기반 통합
# ============================================================

# ── 패키지 ──────────────────────────────────────────────────
pack <- c('data.table','dplyr','lubridate','stringr','tidyr','tidyverse','visdat','ragg','ggplot2')
new_pack <- pack[!(pack %in% installed.packages()[,"Package"])]
if (length(new_pack)) install.packages(new_pack)

library(data.table); library(lubridate); library(stringr)
library(dplyr); library(tidyr); library(tidyverse)


# ============================================================
# 1. 데이터 로드
# ============================================================

# ── 강원대 CDM ───────────────────────────────────────────────
kw_measure  <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_measurement_$20251022.csv",          encoding = "UTF-8")
kw_cond     <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_condition_occurrence_$20251022.csv", encoding = "UTF-8")
kw_person   <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_person_$20251022.csv",               encoding = "UTF-8")
kw_death    <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_death_$20251022.csv",                encoding = "UTF-8")
kw_visit    <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_visit_occurrence_$20251022.csv",     encoding = "UTF-8")
kw_drug     <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_drug_exposure_$20251022.csv",        encoding = "UTF-8")
kw_caresite <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_care_site_$20251022.csv",            encoding = "UTF-8")

# ── 한림대 CDM (강원대와 동일한 OMOP CDM 컬럼 구조) ──────────
hlym_person  <- fread("/home/byeonggeun/data/hallym_huc/정밀의료구현_한림대_가명화데이터/1.환자정보.csv", encoding = "UTF-8")
hlym_visit   <- fread("/home/byeonggeun/data/hallym_huc/정밀의료구현_한림대_가명화데이터/2.방문정보.csv", encoding = "UTF-8")
hlym_death   <- fread("/home/byeonggeun/data/hallym_huc/정밀의료구현_한림대_가명화데이터/3.사망정보.csv", encoding = "UTF-8")
hlym_cond    <- fread("/home/byeonggeun/data/hallym_huc/정밀의료구현_한림대_가명화데이터/4.진단정보.csv", encoding = "UTF-8")
hlym_measure <- fread("/home/byeonggeun/data/hallym_huc/정밀의료구현_한림대_가명화데이터/5.검사정보.csv", encoding = "UTF-8")
hlym_drug    <- fread("/home/byeonggeun/data/hallym_huc/정밀의료구현_한림대_가명화데이터/6.약물정보.csv", encoding = "UTF-8")


# ============================================================
# 2. 공통 설정 (고혈압 코드)
# ============================================================

hpc <- c("C0037","HPC","HPC1","HPC3","HPC4","HPC5","HPC7","HPC9",
         "IMGHE","OTRE","PRAP","PRI","PRP","PRPFM","PRPZ")

hyper_cond_codes <- c(320128, 4028741, 4289933)
hyper_drug_codes <- c(
  42968069, 42968264, 41127461, 40940024, 42968161,
  42946132, 42946163, 42946137, 42946175,
  42936775, 42936771,
  42938933, 42938903,
  42939570, 42939568,
  1597757,  1597760,
  40165262, 40165246, 40165254,
  1545998,  1545999,
  43293185, 43293194,
  42958197, 42958026
)


# ============================================================
# 3. pivot_wider 전 공통 measurement_concept_id 추출
#    → 컬럼 폭발 방지 (integer overflow 해결)
# ============================================================

kw_ids   <- kw_measure   %>% distinct(measurement_concept_id) %>% pull()
hlym_ids <- hlym_measure %>% distinct(measurement_concept_id) %>% pull()

common_ids <- intersect(kw_ids, hlym_ids)

cat("강원대 고유 검사 수:", length(kw_ids), "\n")
cat("한림대 고유 검사 수:", length(hlym_ids), "\n")
cat("공통 검사 수        :", length(common_ids), "\n")


# ============================================================
# 4-A. 강원대 전처리
# ============================================================

# ── 대상자 추출 (HPC 진료과 방문자) ──────────────────────────
kw_hpc_id <- kw_caresite %>%
  filter(care_site_source_value %in% hpc) %>%
  select(care_site_id)

kw_end_vid <- bind_rows(
  kw_measure %>% filter(care_site_source_value %in% hpc) %>% distinct(person_id),
  kw_cond    %>% filter(care_site_source_value %in% hpc) %>% distinct(person_id),
  kw_drug    %>% filter(medical_dept           %in% hpc) %>% distinct(person_id),
  inner_join(kw_visit, kw_hpc_id, by = "care_site_id")  %>% distinct(person_id)
) %>% distinct(person_id)

# ── measure: 대상자 + 공통 concept_id 동시 필터링 후 select ──
kw_measure_f <- inner_join(kw_measure, kw_end_vid, by = "person_id") %>%
  filter(measurement_concept_id %in% common_ids) %>%
  select(person_id, measurement_concept_id, measurement_date,
         value_as_number, unit_concept_id, visit_occurrence_id)

# ── drug / cond: 고혈압 발생일 산출용 ────────────────────────
kw_drug_f <- inner_join(kw_drug, kw_end_vid, by = "person_id") %>%
  select(person_id, drug_concept_id, drug_exposure_start_date)

kw_cond_f <- inner_join(kw_cond, kw_end_vid, by = "person_id") %>%
  select(person_id, condition_concept_id, condition_start_date)

# ── 고혈압 최초 발생일 ────────────────────────────────────────
kw_hyper <- bind_rows(
  kw_cond_f %>%
    filter(condition_concept_id %in% hyper_cond_codes) %>%
    arrange(person_id, condition_start_date) %>%
    distinct(person_id, .keep_all = TRUE) %>%
    rename(end_date = condition_start_date) %>%
    select(person_id, end_date),
  kw_drug_f %>%
    filter(drug_concept_id %in% hyper_drug_codes) %>%
    arrange(person_id, drug_exposure_start_date) %>%
    distinct(person_id, .keep_all = TRUE) %>%
    rename(end_date = drug_exposure_start_date) %>%
    select(person_id, end_date)
) %>%
  arrange(person_id, end_date) %>%
  distinct(person_id, .keep_all = TRUE)

# ── measure 가로 변환 ─────────────────────────────────────────
kw_measure_wide <- kw_measure_f %>%
  arrange(person_id, measurement_date) %>%
  pivot_wider(
    id_cols     = c(person_id, visit_occurrence_id, measurement_date),
    names_from  = measurement_concept_id,
    values_from = value_as_number,
    names_glue  = "{measurement_concept_id}_value",
    names_sort  = FALSE,
    values_fn   = ~ .x[1]
  )

# ── 고혈압 레이블 부착 ────────────────────────────────────────
kw_final <- kw_measure_wide %>%
  left_join(kw_hyper, by = "person_id") %>%
  filter(is.na(end_date) | measurement_date <= end_date) %>%
  group_by(person_id) %>%
  mutate(
    last_visit     = max(measurement_date),
    last_visit_chk = if_else(measurement_date == last_visit, 1L, 0L),
    hyper          = if_else(!is.na(end_date) & last_visit_chk == 1L, 1L, 0L)
  ) %>%
  ungroup() %>%
  select(-last_visit, -last_visit_chk, -end_date, -visit_occurrence_id) %>%
  mutate(site = "kangwon")

cat("강원대 전처리 완료 | rows:", nrow(kw_final), "| cols:", ncol(kw_final), "\n")


# ============================================================
# 4-B. 한림대 전처리
# ============================================================

# ── 전체 대상자 ───────────────────────────────────────────────
hlym_end_vid <- bind_rows(
  hlym_measure %>% distinct(person_id),
  hlym_cond    %>% distinct(person_id),
  hlym_drug    %>% distinct(person_id)
) %>% distinct(person_id)

# ── measure: 대상자 + 공통 concept_id 동시 필터링 후 select ──
hlym_measure_f <- inner_join(hlym_measure, hlym_end_vid, by = "person_id") %>%
  filter(measurement_concept_id %in% common_ids) %>%
  select(person_id, measurement_concept_id, measurement_date,
         value_as_number, unit_concept_id, visit_occurrence_id)

# ── drug / cond: 고혈압 발생일 산출용 ────────────────────────
hlym_drug_f <- inner_join(hlym_drug, hlym_end_vid, by = "person_id") %>%
  select(person_id, drug_concept_id, drug_exposure_start_date)

hlym_cond_f <- inner_join(hlym_cond, hlym_end_vid, by = "person_id") %>%
  select(person_id, condition_concept_id, condition_start_date)

# ── 고혈압 최초 발생일 ────────────────────────────────────────
hlym_hyper <- bind_rows(
  hlym_cond_f %>%
    filter(condition_concept_id %in% hyper_cond_codes) %>%
    arrange(person_id, condition_start_date) %>%
    distinct(person_id, .keep_all = TRUE) %>%
    rename(end_date = condition_start_date) %>%
    select(person_id, end_date),
  hlym_drug_f %>%
    filter(drug_concept_id %in% hyper_drug_codes) %>%
    arrange(person_id, drug_exposure_start_date) %>%
    distinct(person_id, .keep_all = TRUE) %>%
    rename(end_date = drug_exposure_start_date) %>%
    select(person_id, end_date)
) %>%
  arrange(person_id, end_date) %>%
  distinct(person_id, .keep_all = TRUE)

# ── measure 가로 변환 ─────────────────────────────────────────
hlym_measure_wide <- hlym_measure_f %>%
  arrange(person_id, measurement_date) %>%
  pivot_wider(
    id_cols     = c(person_id, visit_occurrence_id, measurement_date),
    names_from  = measurement_concept_id,
    values_from = value_as_number,
    names_glue  = "{measurement_concept_id}_value",
    names_sort  = FALSE,
    values_fn   = ~ .x[1]
  )

# ── 고혈압 레이블 부착 ────────────────────────────────────────
hlym_final <- hlym_measure_wide %>%
  left_join(hlym_hyper, by = "person_id") %>%
  filter(is.na(end_date) | measurement_date <= end_date) %>%
  group_by(person_id) %>%
  mutate(
    last_visit     = max(measurement_date),
    last_visit_chk = if_else(measurement_date == last_visit, 1L, 0L),
    hyper          = if_else(!is.na(end_date) & last_visit_chk == 1L, 1L, 0L)
  ) %>%
  ungroup() %>%
  select(-last_visit, -last_visit_chk, -end_date, -visit_occurrence_id) %>%
  mutate(site = "hallym")

cat("한림대 전처리 완료 | rows:", nrow(hlym_final), "| cols:", ncol(hlym_final), "\n")


# ============================================================
# 5. 두 병원 데이터 통합
# ============================================================

# person_id 병원 간 중복 방지: site 접두어 부착
combined <- bind_rows(
  kw_final   %>% mutate(person_id = paste0("kw_",   person_id)),
  hlym_final %>% mutate(person_id = paste0("hlym_", person_id))
)

cat("통합 데이터 rows:", nrow(combined),
    "| 강원대:", nrow(kw_final),
    "| 한림대:", nrow(hlym_final), "\n")


# ============================================================
# 6. 저장
# ============================================================

out_dir <- "/home/byeonggeun/save/검진"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

fwrite(kw_final,   file = file.path(out_dir, "kangwon_train_data_20260414.csv"),  bom = TRUE)
fwrite(hlym_final, file = file.path(out_dir, "hallym_train_data_20260414.csv"),   bom = TRUE)
fwrite(combined,   file = file.path(out_dir, "combined_train_data_20260414.csv"), bom = TRUE)

cat("CSV 저장 완료\n")


# ============================================================
# 7. 결측치 요약 및 시각화
# ============================================================
library(visdat); library(ggplot2); library(ragg)

missing_summary <- combined %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(percent_missing = (n_missing / nrow(combined)) * 100)

write.csv(missing_summary, file.path(out_dir, "missing_summary_combined.csv"), row.names = FALSE)
print(missing_summary)

save_vis_miss <- function(df, label, out_dir) {
  p <- vis_miss(df, warn_large_data = FALSE) +
    theme(axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5)) +
    labs(title    = paste("Missing Value Heatmap -", label),
         subtitle = paste("Total Rows:", nrow(df)))
  ggsave(
    filename = file.path(out_dir, paste0("missing_heatmap_", label, ".png")),
    plot = p, width = 8, height = 12, dpi = 300, device = ragg::agg_png
  )
  cat(label, "heatmap 저장 완료\n")
}

save_vis_miss(kw_final,   "kangwon",  out_dir)
save_vis_miss(hlym_final, "hallym",   out_dir)
save_vis_miss(combined,   "combined", out_dir)

cat("모든 작업 완료!\n")
