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
kw_measure   <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_measurement_$20251022.csv",           encoding = "UTF-8")
kw_cond      <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_condition_occurrence_$20251022.csv",  encoding = "UTF-8")
kw_person    <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_person_$20251022.csv",                encoding = "UTF-8")
kw_death     <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_death_$20251022.csv",                 encoding = "UTF-8")
kw_visit     <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_visit_occurrence_$20251022.csv",      encoding = "UTF-8")
kw_drug      <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_drug_exposure_$20251022.csv",         encoding = "UTF-8")
kw_caresite  <- fread("/home/byeonggeun/data/cdm/cdm20251022/cdm_care_site_$20251022.csv",             encoding = "UTF-8")

# ── 한림대 CDM ───────────────────────────────────────────────
# NOTE: 아래 컬럼명은 실제 파일 구조에 맞게 확인 후 섹션 2-B 에서 매핑하세요.
hlym_person  <- fread("/home/byeonggeun/data/hallym_huc/정밀의료구현_한림대_가명화데이터/1.환자정보.csv",   encoding = "UTF-8")
hlym_visit   <- fread("/home/byeonggeun/data/hallym_huc/정밀의료구현_한림대_가명화데이터/2.방문정보.csv",   encoding = "UTF-8")
hlym_death   <- fread("/home/byeonggeun/data/hallym_huc/정밀의료구현_한림대_가명화데이터/3.사망정보.csv",   encoding = "UTF-8")
hlym_cond    <- fread("/home/byeonggeun/data/hallym_huc/정밀의료구현_한림대_가명화데이터/4.진단정보.csv",   encoding = "UTF-8")
hlym_measure <- fread("/home/byeonggeun/data/hallym_huc/정밀의료구현_한림대_가명화데이터/5.검사정보.csv",   encoding = "UTF-8")
hlym_drug    <- fread("/home/byeonggeun/data/hallym_huc/정밀의료구현_한림대_가명화데이터/6.약물정보.csv",   encoding = "UTF-8")

# 컬럼명 확인 (처음 실행 시 주석 해제하여 확인)
# lapply(list(hlym_person=hlym_person, hlym_visit=hlym_visit, hlym_death=hlym_death,
#             hlym_cond=hlym_cond, hlym_measure=hlym_measure, hlym_drug=hlym_drug), colnames)


# ============================================================
# 2-A. 강원대 전처리
# ============================================================

# ── 공통 설정 ────────────────────────────────────────────────
hpc <- c("C0037","HPC","HPC1","HPC3","HPC4","HPC5","HPC7","HPC9",
         "IMGHE","OTRE","PRAP","PRI","PRP","PRPFM","PRPZ")

hyper_cond_codes <- c(320128, 4028741, 4289933)   # 본태성·양성·악성 고혈압
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

# ── 대상자 추출 (HPC 진료과 방문자) ──────────────────────────
kw_hpc_id <- kw_caresite %>%
  filter(care_site_source_value %in% hpc) %>%
  select(care_site_id)

kw_end_vid <- bind_rows(
  kw_measure   %>% filter(care_site_source_value %in% hpc) %>% distinct(person_id),
  kw_cond      %>% filter(care_site_source_value %in% hpc) %>% distinct(person_id),
  kw_drug      %>% filter(medical_dept             %in% hpc) %>% distinct(person_id),
  inner_join(kw_visit, kw_hpc_id, by = "care_site_id")      %>% distinct(person_id)
) %>% distinct(person_id)

# ── 각 테이블 대상자 필터링 ───────────────────────────────────
kw_measure_f <- inner_join(kw_measure, kw_end_vid, by = "person_id") %>%
  select(person_id, measurement_concept_id, measurement_date,
         value_as_number, unit_concept_id, visit_occurrence_id, care_site_source_value)

kw_drug_f <- inner_join(kw_drug, kw_end_vid, by = "person_id") %>%
  select(person_id, drug_concept_id, drug_exposure_start_date,
         stop_reason, quantity, days_supply, dose_unit_source_value,
         sig, visit_occurrence_id, medical_dept, ward_cd)

kw_cond_f <- inner_join(kw_cond, kw_end_vid, by = "person_id") %>%
  select(person_id, condition_concept_id, condition_start_date,
         visit_occurrence_id, care_site_source_value)

kw_person_f <- inner_join(kw_person, kw_end_vid, by = "person_id") %>%
  mutate(birth = as.Date(paste(year_of_birth, month_of_birth, day_of_birth, sep = "-"))) %>%
  select(person_id, gender_concept_id, birth, year_of_birth, month_of_birth, day_of_birth)

kw_death_f <- inner_join(kw_death, kw_end_vid, by = "person_id") %>%
  select(person_id, death_date, cause_source_value)

# ── 고혈압 최초 발생일 ────────────────────────────────────────
kw_hyper_cond <- kw_cond_f %>%
  filter(condition_concept_id %in% hyper_cond_codes) %>%
  arrange(person_id, condition_start_date) %>%
  distinct(person_id, .keep_all = TRUE) %>%
  rename(end_date = condition_start_date) %>%
  select(person_id, end_date)

kw_hyper_drug <- kw_drug_f %>%
  filter(drug_concept_id %in% hyper_drug_codes) %>%
  arrange(person_id, drug_exposure_start_date) %>%
  distinct(person_id, .keep_all = TRUE) %>%
  rename(end_date = drug_exposure_start_date) %>%
  select(person_id, end_date)

kw_hyper <- bind_rows(kw_hyper_cond, kw_hyper_drug) %>%
  arrange(person_id, end_date) %>%
  distinct(person_id, .keep_all = TRUE)

# ── measure 가로 변환 + 고혈압 레이블 부착 ────────────────────
kw_measure_wide <- kw_measure_f %>%
  arrange(person_id, measurement_date) %>%
  pivot_wider(
    id_cols    = c(person_id, visit_occurrence_id, measurement_date),
    names_from = measurement_concept_id,
    values_from = c(value_as_number, unit_concept_id),
    names_glue = "{measurement_concept_id}_{.value}",
    names_sort = FALSE,
    values_fn  = list(value_as_number = ~ .x[1], unit_concept_id = ~ .x[1])
  )

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
  select(-last_visit) %>%
  select(-contains(c("unit", "visit", "condition", "end_date", "care_site",
                     "drug_concept", "stop_reason", "quantity", "days",
                     "sig", "medical", "ward", "chk"))) %>%
  mutate(site = "kangwon")   # 병원 구분 컬럼

cat("강원대 전처리 완료 | rows:", nrow(kw_final), "\n")


# ============================================================
# 2-B. 한림대 전처리
# ============================================================
# NOTE: 아래 rename() 블록은 한림대 원본 컬럼명에 맞게 수정하세요.
#       colnames(hlym_measure) 등으로 실제 컬럼명을 먼저 확인하세요.

# ── 한림대 컬럼명 표준화 (OMOP CDM 규격으로 통일) ─────────────
# (실제 컬럼명 확인 후 좌측=원본컬럼명, 우측=표준명 으로 수정)
hlym_measure_std <- hlym_measure %>%
  rename(
    person_id              = person_id,             # 예: 환자ID → person_id
    measurement_concept_id = measurement_concept_id,# 예: 검사개념ID
    measurement_date       = measurement_date,       # 예: 검사일자
    value_as_number        = value_as_number,        # 예: 검사결과수치
    unit_concept_id        = unit_concept_id,        # 예: 단위개념ID
    visit_occurrence_id    = visit_occurrence_id     # 예: 방문ID
  )

hlym_cond_std <- hlym_cond %>%
  rename(
    person_id              = person_id,
    condition_concept_id   = condition_concept_id,
    condition_start_date   = condition_start_date,
    visit_occurrence_id    = visit_occurrence_id
  )

hlym_drug_std <- hlym_drug %>%
  rename(
    person_id                 = person_id,
    drug_concept_id           = drug_concept_id,
    drug_exposure_start_date  = drug_exposure_start_date,
    visit_occurrence_id       = visit_occurrence_id
  )

hlym_person_std <- hlym_person %>%
  rename(
    person_id        = person_id,
    gender_concept_id = gender_concept_id,
    year_of_birth    = year_of_birth,
    month_of_birth   = month_of_birth,
    day_of_birth     = day_of_birth
  )

hlym_death_std <- hlym_death %>%
  rename(
    person_id         = person_id,
    death_date        = death_date,
    cause_source_value = cause_source_value
  )

# ── 한림대 전체 대상자 (데이터 내 모든 person_id 사용) ─────────
hlym_end_vid <- bind_rows(
  hlym_measure_std %>% distinct(person_id),
  hlym_cond_std    %>% distinct(person_id),
  hlym_drug_std    %>% distinct(person_id)
) %>% distinct(person_id)

# ── 각 테이블 대상자 필터링 ───────────────────────────────────
hlym_measure_f <- inner_join(hlym_measure_std, hlym_end_vid, by = "person_id") %>%
  select(person_id, measurement_concept_id, measurement_date, value_as_number, unit_concept_id)

hlym_drug_f <- inner_join(hlym_drug_std, hlym_end_vid, by = "person_id")

hlym_cond_f <- inner_join(hlym_cond_std, hlym_end_vid, by = "person_id")

# ── 고혈압 최초 발생일 ────────────────────────────────────────
hlym_hyper_cond <- hlym_cond_f %>%
  filter(condition_concept_id %in% hyper_cond_codes) %>%
  arrange(person_id, condition_start_date) %>%
  distinct(person_id, .keep_all = TRUE) %>%
  rename(end_date = condition_start_date) %>%
  select(person_id, end_date)

hlym_hyper_drug <- hlym_drug_f %>%
  filter(drug_concept_id %in% hyper_drug_codes) %>%
  arrange(person_id, drug_exposure_start_date) %>%
  distinct(person_id, .keep_all = TRUE) %>%
  rename(end_date = drug_exposure_start_date) %>%
  select(person_id, end_date)

hlym_hyper <- bind_rows(hlym_hyper_cond, hlym_hyper_drug) %>%
  arrange(person_id, end_date) %>%
  distinct(person_id, .keep_all = TRUE)

# ── measure 가로 변환 + 고혈압 레이블 부착 ────────────────────
hlym_measure_wide <- hlym_measure_f %>%
  arrange(person_id, measurement_date) %>%
  pivot_wider(
    id_cols    = c(person_id, measurement_date),
    names_from = measurement_concept_id,
    values_from = c(value_as_number, unit_concept_id),
    names_glue = "{measurement_concept_id}_{.value}",
    names_sort = FALSE,
    values_fn  = list(value_as_number = ~ .x[1], unit_concept_id = ~ .x[1])
  )

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
  select(-last_visit) %>%
  select(-contains(c("unit", "end_date", "chk"))) %>%
  mutate(site = "hallym")   # 병원 구분 컬럼

cat("한림대 전처리 완료 | rows:", nrow(hlym_final), "\n")


# ============================================================
# 3. 공통 measurement_concept_id 추출
# ============================================================

# 각 병원에서 사용된 measurement_concept_id 목록 추출
#   - 가로변환 후 컬럼명 패턴: "{concept_id}_value_as_number"
extract_concept_ids <- function(df) {
  cols <- names(df)
  value_cols <- cols[grepl("_value_as_number$", cols)]
  as.integer(sub("_value_as_number$", "", value_cols))
}

kw_concept_ids   <- extract_concept_ids(kw_final)
hlym_concept_ids <- extract_concept_ids(hlym_final)

common_ids <- intersect(kw_concept_ids, hlym_concept_ids)

cat("강원대 고유 검사 수:", length(kw_concept_ids), "\n")
cat("한림대 고유 검사 수:", length(hlym_concept_ids), "\n")
cat("공통 검사 수        :", length(common_ids), "\n")
cat("공통 concept_id     :", paste(common_ids, collapse = ", "), "\n")

# 공통 검사에 해당하는 컬럼만 유지 (value_as_number 컬럼만; unit 제외)
common_value_cols <- paste0(common_ids, "_value_as_number")
keep_cols         <- c("person_id", "measurement_date", "hyper", "site",
                       intersect(common_value_cols, names(kw_final)),
                       intersect(common_value_cols, names(hlym_final))) %>% unique()

# ── 강원대: 공통 컬럼만 선택 ──────────────────────────────────
kw_common <- kw_final %>%
  select(any_of(keep_cols))

# ── 한림대: 공통 컬럼만 선택 ──────────────────────────────────
hlym_common <- hlym_final %>%
  select(any_of(keep_cols))


# ============================================================
# 4. 두 병원 데이터 통합
# ============================================================

# person_id 가 병원 간 중복될 수 있으므로 site 접두어로 구분
combined <- bind_rows(
  kw_common   %>% mutate(person_id = paste0("kw_",   person_id)),
  hlym_common %>% mutate(person_id = paste0("hlym_", person_id))
)

cat("통합 데이터 rows:", nrow(combined),
    "| 강원대:", nrow(kw_common),
    "| 한림대:", nrow(hlym_common), "\n")


# ============================================================
# 5. 저장
# ============================================================

out_dir <- "/home/byeonggeun/save/검진"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 강원대 단독
fwrite(kw_common,   file = file.path(out_dir, "kangwon_train_data_20260414.csv"),  bom = TRUE)

# 한림대 단독
fwrite(hlym_common, file = file.path(out_dir, "hallym_train_data_20260414.csv"),   bom = TRUE)

# 통합 (공통 검사만)
fwrite(combined,    file = file.path(out_dir, "combined_train_data_20260414.csv"), bom = TRUE)

cat("CSV 저장 완료\n")


# ============================================================
# 6. 결측치 요약 및 시각화 (통합 데이터 기준)
# ============================================================
library(visdat); library(ggplot2); library(ragg)

missing_summary <- combined %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(percent_missing = (n_missing / nrow(combined)) * 100)

write.csv(missing_summary, file.path(out_dir, "missing_summary_combined.csv"), row.names = FALSE)
print(missing_summary)

# ── 개별 병원 결측치 시각화 ───────────────────────────────────
save_vis_miss <- function(df, label, out_dir) {
  p <- vis_miss(df, warn_large_data = FALSE) +
    theme(axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5)) +
    labs(title    = paste("Missing Value Heatmap -", label),
         subtitle = paste("Total Rows:", nrow(df)))

  ggsave(
    filename = file.path(out_dir, paste0("missing_heatmap_", label, ".png")),
    plot     = p,
    width    = 8, height = 12, dpi = 300,
    device   = ragg::agg_png
  )
  cat(label, "heatmap 저장 완료\n")
}

save_vis_miss(kw_common,   "kangwon",  out_dir)
save_vis_miss(hlym_common, "hallym",   out_dir)
save_vis_miss(combined,    "combined", out_dir)

cat("모든 작업 완료!\n")
