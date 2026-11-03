#======================================================================#
# Purpose : Summarize verified interfamily diagnostics for sample decisions
# Name    : Saani Rawat
# Created : 10/31/2026
# Log     : 10/31/2026: add aggregate tables and standalone audit figure.
#           11/01/2026: correct conditional aggregation; verify subtotals.
# Inputs  : 15_interfamily_flag_audit.R aggregate CSVs
# Outputs : National/California summaries and PNG; no analysis changes.
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
audit_dir <- path(tables_out_dir, "interfamily_audit_2026-10-31")
con <- open_corelogic_duckdb(memory_limit="1GB",threads=1)
panel_file <- sql_quote_path(path(data_dir,"events.parquet"))
expected <- dbGetQuery(con,glue("SELECT state,count(*) AS n FROM read_parquet('{panel_file}')
  WHERE sale_year BETWEEN 2007 AND 2023 GROUP BY state"))
dbDisconnect(con,shutdown=TRUE)
# Combine exactly the geographic partitions present in the current panel,
# including its small territory/military-code residue. Do not silently drop it.
labels <- c("class_profile","document_profile","category_price","flag_features",
            "ownership_codes","year_class","broad_buckets","candidate_counts","reconciliation")
for (label in labels) {
  files <- path(audit_dir,paste0(label,"_",expected$state,".csv"))
  stopifnot(all(file_exists(files)))
  pieces <- lapply(files,function(f) read_csv(f,na=character(),show_col_types=FALSE))
  nonempty <- Filter(function(x) nrow(x)>0,pieces)
  result <- if (length(nonempty)) bind_rows(nonempty) else pieces[[1]]
  write_csv(result,path(audit_dir,paste0(label,"_combined.csv")))
}
# The diagnostic run initially carried an unused numeric interpretation of
# partial_interest_indicator. Source inspection showed letter codes (Y etc.).
# Remove that unused metric from every aggregate; it never enters a screen.
profile_files <- dir_ls(audit_dir,regexp="(class|document)_profile_.*[.]csv$")
for (f in profile_files) {
  p <- read_csv(f,na=character(),show_col_types=FALSE)
  if ("partial_flag" %in% names(p)) write_csv(select(p,-partial_flag),f)
}
read_audit <- function(label) {
  read_csv(path(audit_dir,paste0(label,"_combined.csv")),
           na=character(),show_col_types=FALSE)
}
save_audit <- function(x,label) write_csv(x,path(audit_dir,paste0(label,".csv")))

# Verifying coverage and reconciliation ----
reconciliation <- read_audit("reconciliation")
stopifnot(nrow(reconciliation)==nrow(expected),
          setequal(reconciliation$state,expected$state),
          all(reconciliation$unmatched_key==0),
          all(reconciliation$class_flag_price_mismatch==0),
          all(reconciliation$reconstructed_n==reconciliation$existing_n))
profiles <- read_audit("class_profile")
stopifnot(sum(profiles$n)==sum(reconciliation$existing_n),sum(profiles$n)==sum(expected$n))
flag_total <- sum(profiles$n[profiles$interfam==1])
broad_classes <- c("family_person","family_other","family_estate")
broad_total <- sum(profiles$n[profiles$class %in% broad_classes])
market_total <- sum(profiles$n[profiles$class=="market_sale"])

# Summarizing current classes and candidate screens ----
for (region in c("national","california")) {
  p <- if (region=="national") profiles else filter(profiles,state=="CA")
  summary <- p %>% group_by(class,interfam) %>%
    summarize(across(where(is.numeric),sum),.groups="drop") %>%
    mutate(share_all=n/sum(n),share_flagged=if_else(interfam==1,n/sum(n[interfam==1]),NA_real_)) %>%
    arrange(desc(n))
  save_audit(summary,paste0(region,"_class_summary"))
}
buckets <- read_audit("broad_buckets") %>% group_by(class,audit_bucket) %>%
  summarize(n=sum(n),.groups="drop") %>%
  group_by(class) %>% mutate(share_class=n/sum(n)) %>% ungroup()
stopifnot(sum(buckets$n)==broad_total)
save_audit(buckets,"national_broad_buckets")
candidates <- read_audit("candidate_counts")
candidate_summary <- candidates %>% group_by(definition) %>%
  summarize(n_ca=sum(n[state=="CA"]),n=sum(n),.groups="drop") %>%
  mutate(share_all_flagged=n/flag_total,relative_current_broad=n/broad_total,
         per_100_market=100*n/market_total) %>% arrange(desc(n))
save_audit(candidate_summary,"national_candidate_summary")
candidate_n <- setNames(candidate_summary$n,candidate_summary$definition)
stopifnot(candidate_n[["current_broad"]]==broad_total,
          candidate_n[["all_flagged"]]==flag_total,
          candidate_n[["broad_screened_complete"]]==sum(buckets$n[buckets$audit_bucket=="6_named_person_candidate"]),
          candidate_n[["broad_screened_complete"]]==sum(candidate_n[c("surname_screened_complete",
            "other_screened_complete","estate_screened_complete")]),
          candidate_n[["surname_screened_complete"]]<=candidate_n[["surname_screened"]],
          candidate_n[["surname_screened"]]<=candidate_n[["current_same_surname"]])

# Testing simple explanations of the vendor flag ----
features <- read_audit("flag_features")
signals <- c("same_surname","same_person","trust_kw","entity_any",
             "any_name_overlap","complete_person_fields")
signal_summary <- bind_rows(lapply(signals,function(signal) {
  features %>% group_by(present=.data[[signal]]) %>%
    summarize(n_flagged=sum(n[interfam==1]),n=sum(n),.groups="drop") %>%
    mutate(signal=signal,flag_rate=n_flagged/n,share_all_flagged=n_flagged/flag_total)
}))
save_audit(signal_summary,"national_flag_signal_rates")
category <- read_audit("category_price") %>% group_by(pcat,price_group,interfam) %>%
  summarize(n=sum(n),.groups="drop")
save_audit(category,"national_category_price")

# Preserving raw deed codes and uncertainty in the internal crosswalk ----
docs <- read_audit("document_profile")
meanings <- read_csv(here("shared_utils","R","reference","sale_document_type_meanings.csv"),
                     na=character(),show_col_types=FALSE)
doc_summary <- docs %>% group_by(doc) %>%
  summarize(n_flagged=sum(n[interfam==1]),
            flagged_current_broad=sum(n[class %in% broad_classes]),
            flagged_trust=sum(n[class=="family_trust"]),
            flagged_retitle=sum(n[class=="family_retitle"]),n=sum(n),.groups="drop") %>%
  mutate(flag_rate=n_flagged/n,share_flagged=n_flagged/flag_total) %>%
  left_join(meanings,by=c("doc"="code")) %>% arrange(desc(n_flagged))
save_audit(doc_summary,"national_document_summary")
ca_docs <- docs %>% filter(state=="CA") %>% group_by(doc) %>%
  summarize(n_flagged=sum(n[interfam==1]),
            flagged_current_broad=sum(n[class %in% broad_classes]),
            flagged_trust=sum(n[class=="family_trust"]),
            flagged_retitle=sum(n[class=="family_retitle"]),n=sum(n),.groups="drop") %>%
  mutate(flag_rate=n_flagged/n) %>% left_join(meanings,by=c("doc"="code")) %>%
  arrange(desc(n_flagged))
save_audit(ca_docs,"california_document_summary")
state_summary <- profiles %>% group_by(state) %>%
  summarize(n_flagged=sum(n[interfam==1]),
    n_broad=sum(n[class %in% broad_classes]),
    n_person=sum(n[class=="family_person"]),
    n_trust=sum(n[class=="family_trust"]),n_retitle=sum(n[class=="family_retitle"]),
    n_other=sum(n[class=="family_other"]),n=sum(n),.groups="drop") %>%
  mutate(flag_rate=n_flagged/n,trust_share_flagged=n_trust/n_flagged,
         retitle_share_flagged=n_retitle/n_flagged,broad_share_flagged=n_broad/n_flagged)
save_audit(state_summary,"state_composition")
annual <- candidates %>% group_by(sale_year,definition) %>%
  summarize(n_ca=sum(n[state=="CA"]),n=sum(n),.groups="drop")
save_audit(annual,"annual_candidates")
stopifnot(!anyNA(candidate_summary$n_ca),!anyNA(signal_summary$n_flagged),
          !anyNA(doc_summary$n_flagged),sum(doc_summary$n_flagged)==flag_total,
          sum(state_summary$n_flagged)==flag_total,
          all(signal_summary$flag_rate>=0 & signal_summary$flag_rate<=1))

# Rendering a figure for inspecting the measurement choices ----
bucket_labels <- c("1_entity_signal"="Company / institutional signal",
  "2_trust_signal"="Additional trust-name signal",
  "3_continuing_named_party"="Continuing named party",
  "4_reo_signal"="REO signal",
  "5_incomplete_person_fields"="Incomplete primary person fields",
  "6_named_person_candidate"="Named-person candidate")
plot_data <- buckets %>% filter(class %in% c("family_person","family_other")) %>%
  mutate(audit_bucket=factor(audit_bucket,levels=names(bucket_labels),labels=bucket_labels),
    class=recode(class,family_person="Current same-surname class",
                 family_other="Current other-interfamily class"),
    count_label=if_else(n<10000,scales::comma(n),sprintf("%.2fM",n/1e6)),
    percent_label=if_else(share_class<.001,"<0.1%",sprintf("%.1f%%",100*share_class)),
    bar_label=paste0(count_label," (",percent_label,")"))
fig <- ggplot(plot_data,aes(x=n/1e6,y=audit_bucket,fill=audit_bucket))+
  geom_col(width=.7)+
  geom_text(aes(label=bar_label),
            hjust=-.05,size=3.2)+
  facet_wrap(~class,scales="free_x",ncol=1)+
  scale_x_continuous(expand=expansion(mult=c(0,.28)))+
  scale_y_discrete(limits=rev(levels(plot_data$audit_bucket)))+
  scale_fill_manual(values=c("#b45b50","#c8a14c","#8f78a8","#a2a2a2","#6d91ac","#397b69"))+
  labs(title="What remains inside the paper's family-transfer classes?",
       subtitle="Residential parcel-date events, 2007-2023; mutually exclusive diagnostic screens",
       x="Millions of events",y=NULL,
       caption="Priority: entity, trust, continuing party, REO, incomplete names, remaining candidate.\nSignals are not validated kinship labels; continuing parties can transfer partial interests.")+
  theme_paper(base_size=11)+theme(legend.position="none",panel.grid.major.y=element_blank())
figure_path <- path(figures_out_dir,"interfamily_audit_2026-10-31.png")
ggsave(figure_path,fig,width=11,height=8,dpi=180,bg="white")
writeLines(c(paste("PASS:",nrow(expected),"geographic partitions reconciled to audit-run panel keys, classes, flag, and price."),
             "Source reconciliation performed 2026-10-31; summaries finalized 2026-11-01.",
             "Later panel rebuild: state counts checked here; main class totals checked separately. Auxiliary tie values can differ.",
             paste("All residential events:",sum(profiles$n)),
             paste("Interfamily flagged:",flag_total),
             paste("Current broad family:",broad_total),
             paste("Current category-A market:",market_total),
             "PASS: mutually exclusive broad buckets sum to current broad family total.",
             paste("Figure:",figure_path)),path(audit_dir,"verification.txt"))
print(candidate_summary,n=20)
print(signal_summary,n=20)
