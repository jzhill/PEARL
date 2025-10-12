# 06.14_tpt_side_effects_breakdown.R
# Disaggregated side effects by group and item + TOTAL row

library(tidyverse)
library(flextable)
library(here)
library(stringr)

# ---- Pretty names ------------------------------------------------------------
pretty_item <- function(code) {
  recode(code,
         fatigue  = "Fatigue",
         loa      = "Loss of appetite",
         nv       = "Nausea/vomiting",
         pain     = "Abdominal pain",
         rash     = "Rash",
         jaundice = "Jaundice",
         flu      = "Flu-like symptoms",
         syncope  = "Syncope/presyncope",
         stpain   = "Stomach problems",
         cough    = "Cough",
         headache = "Headache",
         dizzy    = "Dizziness",
         regurg   = "Regurgitation",
         const    = "Constipation",
         diar     = "Diarrhoea",
         jtpain   = "Joint pain",
         palp     = "Palpitations",
         sob      = "Shortness of breath",
         sweat    = "Sweating/feeling hot",
         tired    = "Feeling tired",
         weak     = "Feeling weak",
         .default = str_replace_all(str_to_sentence(code), "_", " ")
  )
}

# ---- Setup -------------------------------------------------------------------
tp      <- c("1m","3m","4m","ae")
tp_labs <- c("1m" = "1 month", "3m" = "3 month", "4m" = "4 month", "ae" = "AE form")

groups  <- tribble(
  ~key,       ~label,
  "dili_sx",  "Drug-induced liver injury (DILI)",
  "rhs_sx",   "Rifamycin hypersensitivity (RHS)",
  "csx",      "Common side effects (CSX)"
)

has_col  <- function(df, col) col %in% names(df)
pick_col <- function(df, candidates) {
  found <- candidates[candidates %in% names(df)]
  if (length(found)) found[1] else NA_character_
}
count_true <- function(x) sum(x %in% c(TRUE, 1, "1", "TRUE", "True"), na.rm = TRUE)

# ---- GROUP TOTAL ROWS (use existing *_any_true) ------------------------------
group_rows <- map_dfr(1:nrow(groups), function(i) {
  gkey   <- groups$key[i]
  glabel <- groups$label[i]
  
  any_cols <- map_chr(tp, \(t) pick_col(
    treatment_data,
    c(paste0("tpt_", t, "_", gkey, "_any_true"),
      paste0(t, "_", gkey, "_any_true"))
  ))
  names(any_cols) <- tp
  
  counts_tp <- map_int(tp, \(t) {
    col <- any_cols[[t]]
    if (is.na(col)) 0L else count_true(treatment_data[[col]])
  })
  names(counts_tp) <- tp
  
  overall_any_col <- pick_col(
    treatment_data,
    c(paste0("tpt_", gkey, "_ever_any_true"),
      paste0(gkey, "_ever_any_true"),
      paste0("tpt_", gkey, "_any_true"),
      paste0(gkey, "_any_true"))
  )
  ever_count <- if (!is.na(overall_any_col)) {
    count_true(treatment_data[[overall_any_col]])
  } else {
    present <- any_cols[!is.na(any_cols)]
    if (length(present) == 0) 0L else {
      mat <- map(present, ~ treatment_data[[.x]] %in% c(TRUE, 1, "1", "TRUE", "True")) %>%
        bind_cols() %>% as.matrix()
      sum(rowSums(mat, na.rm = TRUE) > 0, na.rm = TRUE)
    }
  }
  
  tibble(
    Group = glabel,
    Item  = "Any",
    `1m`  = counts_tp["1m"],
    `3m`  = counts_tp["3m"],
    `4m`  = counts_tp["4m"],
    ae    = counts_tp["ae"],
    ever  = ever_count
  )
})

# ---- ITEM SUBROWS ------------------------------------------------------------
detect_items_for_group <- function(gkey) {
  pat  <- paste0("^tpt_(", paste(tp, collapse="|"), ")_", gkey, "_([a-z0-9_]+)$")
  vars <- grep(pat, names(treatment_data), value = TRUE)
  if (!length(vars)) return(character(0))
  codes <- unique(sub(pat, "\\2", vars))
  codes[!codes %in% c("none","any_true")]
}

item_rows <- map_dfr(1:nrow(groups), function(i) {
  gkey   <- groups$key[i]
  glabel <- groups$label[i]
  items  <- detect_items_for_group(gkey)
  if (!length(items)) return(tibble(Group = glabel, Item = character(0), `1m`=integer(), `3m`=integer(), `4m`=integer(), ae=integer(), ever=integer()))
  
  map_dfr(items, function(code) {
    tp_counts <- map_int(tp, function(t) {
      col <- paste0("tpt_", t, "_", gkey, "_", code)
      if (!has_col(treatment_data, col)) 0L else count_true(treatment_data[[col]])
    })
    names(tp_counts) <- tp
    
    cols <- paste0("tpt_", tp, "_", gkey, "_", code)
    cols <- cols[cols %in% names(treatment_data)]
    ever_count <- if (!length(cols)) 0L else {
      mat <- map(cols, ~ treatment_data[[.x]] %in% c(TRUE, 1, "1", "TRUE", "True")) %>%
        bind_cols() %>% as.matrix()
      sum(rowSums(mat, na.rm = TRUE) > 0, na.rm = TRUE)
    }
    
    tibble(
      Group = glabel,
      Item  = pretty_item(code),
      `1m`  = tp_counts["1m"],
      `3m`  = tp_counts["3m"],
      `4m`  = tp_counts["4m"],
      ae    = tp_counts["ae"],
      ever  = ever_count
    )
  })
})

sx_long <- bind_rows(group_rows, item_rows) %>%
  mutate(
    Group = factor(Group, levels = groups$label),
    Item  = as.character(Item)
  ) %>%
  arrange(Group, desc(Item == "Any"), Item)

# ---- TOTAL "ANY SIDE EFFECT" ROW (all groups combined) ----------------------
# Prefer overall timepoint flags if present; else OR across group *_any_true
overall_tp_counts <- map_int(tp, function(t) {
  # overall any_true candidates for this timepoint
  overall_col <- pick_col(
    treatment_data,
    c(paste0("tpt_", t, "_any_true"),
      paste0(t, "_any_true"))
  )
  if (!is.na(overall_col)) {
    return(count_true(treatment_data[[overall_col]]))
  } else {
    # OR across groups' any_true columns for this tp
    grp_cols <- c(paste0("tpt_", t, "_", groups$key, "_any_true"),
                  paste0(t, "_", groups$key, "_any_true")) %>%
      intersect(names(treatment_data))
    if (!length(grp_cols)) return(0L)
    mat <- map(grp_cols, ~ treatment_data[[.x]] %in% c(TRUE, 1, "1", "TRUE", "True")) %>%
      bind_cols() %>% as.matrix()
    return(sum(rowSums(mat, na.rm = TRUE) > 0, na.rm = TRUE))
  }
})
names(overall_tp_counts) <- tp

overall_ever <- {
  ever_col <- pick_col(
    treatment_data,
    c("sx_any_ever", "ae_any_true")  # your project now uses sx_any_ever
  )
  if (!is.na(ever_col)) {
    count_true(treatment_data[[ever_col]])
  } else {
    # compute fallback OR across timepoint overall flags (if present)
    tp_cols <- c(paste0("tpt_", tp, "_any_true"), paste0(tp, "_any_true")) %>%
      intersect(names(treatment_data))
    if (!length(tp_cols)) 0L else {
      mat <- map(tp_cols, ~ treatment_data[[.x]] %in% c(TRUE, 1, "1", "TRUE", "True")) %>%
        bind_cols() %>% as.matrix()
      sum(rowSums(mat, na.rm = TRUE) > 0, na.rm = TRUE)
    }
  }
}

total_row <- tibble(
  Group = "Total",
  Item  = "Any side effect",
  `1m`  = overall_tp_counts["1m"],
  `3m`  = overall_tp_counts["3m"],
  `4m`  = overall_tp_counts["4m"],
  ae    = overall_tp_counts["ae"],
  ever  = overall_ever
)

sx_long <- bind_rows(sx_long, total_row)

# ---- Relabel timepoint columns for presentation -----------------------------
sx_long_out <- sx_long %>%
  rename(
    `1 month` = `1m`,
    `3 month` = `3m`,
    `4 month` = `4m`,
    `AE form` = `ae`,
    `Ever`    = `ever`
  )

# ---- Flextable --------------------------------------------------------------
table_06.14 <- sx_long_out %>%
  flextable(col_keys = c("Group","Item","1 month","3 month","4 month","AE form","Ever")) %>%
  theme_vanilla() %>%
  bg(part = "all", bg = "white") %>%
  bold(part = "header", bold = TRUE) %>%
  merge_v(j = "Group") %>%
  valign(j = "Group", valign = "top") %>%
  align(j = c("1 month","3 month","4 month","AE form","Ever"), align = "center", part = "all") %>%
  colformat_int(j = c("1 month","3 month","4 month","AE form","Ever"), big.mark = ",") %>%
  bold(i = ~ Group == "Total" & Item == "Any side effect") %>%  # highlight total row
  autofit()

table_06.14

# ---- Save flextable (exact block) ------------------------------------------
# Save flextable
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(here("figures"), paste0("Outputs_", current_date))
output_filename <- paste0("table_06.14_", current_date, ".png")

save_as_image(table_06.14, path = file.path(output_dir, output_filename))
