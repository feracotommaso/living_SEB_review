# Shared, compact article-level coding specification. Legacy 31 analytical
# fields remain unchanged; expanded fields live in records and review exports.
review_coding_fields <- function() c('main_objective','sample_context','seb_measurement',
 'study_contribution','constructs_analyzed')

review_coding_options <- function() list(
 sample_context=c('Preschool','Primary school','Secondary school','Higher education',
  'Workplace','General population','Other'),
 seb_measurement=c('Self-report — ability','Self-report — frequency/typical behavior',
  'Other-report — ability','Other-report — frequency/typical behavior','Behavioral/performance task','Other'),
 study_contribution=c('Measure development/validation','Associations/prediction',
  'Development/change','Intervention evaluation','Evidence synthesis','Theory/conceptual contribution'),
 study_design=c(cross='Cross-sectional',long='Longitudinal',exp='Experimental/intervention',
  esm='Diary/ESM/EMA',review='Review',meta='Meta-analysis',theory='Theoretical/conceptual',other='Other'))

review_coding_tokens <- function(x) {
 if(is.null(x)||!length(x)||all(is.na(x)))return(character())
 trimws(unlist(strsplit(as.character(x[!is.na(x)]),';',fixed=TRUE)))
}

review_validate_design <- function(x) {
 v<-review_coding_tokens(x);v<-v[nzchar(v)]
 if(anyDuplicated(v)||any(!v%in%names(review_coding_options()$study_design)))
  stop('Invalid study_design category or duplicate: ',paste(v,collapse='; '),call.=FALSE)
 invisible(TRUE)
}

review_validate_article <- function(records,variables) {
 missing<-c('Not reported','Not applicable','Not yet assessed')
 opts<-review_coding_options()
 for(f in review_coding_fields()) {
  if(!f%in%names(records))stop('Missing expanded coding field: ',f)
  for(id in unique(records$paper_id)) {
   values<-records[[f]][records$paper_id==id]
   if(length(unique(ifelse(is.na(values),'',values)))>1L)
    stop('Article-level coding differs across samples: ',id,' / ',f)
  }
  for(value in records[[f]]) {
   if(is.na(value)||!nzchar(value)||value%in%missing)next
   if(f=='main_objective')next
   v<-review_coding_tokens(value)
   if(anyDuplicated(v)||any(!nzchar(v))||any(v%in%missing))stop('Duplicate or mixed missing-value coding: ',f)
   allowed<-if(f=='constructs_analyzed')variables$column_name else opts[[f]]
   if(any(!v%in%allowed))stop('Unregistered ',f,': ',paste(setdiff(v,allowed),collapse='; '))
  }
 }
 invisible(TRUE)
}

review_coding_dictionary <- function(legacy) {
 fields<-review_coding_fields()
 definitions<-c('One short sentence stating the main research question.',
  'Contexts of the original samples; no new sample in a synthesis is Not applicable.',
  'How SEB skills are measured; do not infer from methods used for other constructs.',
  'Substantive scientific contributions, allowing multiple categories.',
  'Distinct base constructs explicitly analyzed, including quantitative syntheses; not a list of matrix suffix variants.')
 extra<-legacy[rep(1,length(fields)),,drop=FALSE]
 extra$dictionary_version<-'1.1.0';extra$order<-as.character(nrow(legacy)+seq_along(fields))
 extra$field<-fields;extra$level<-'paper';extra$type<-c('string',rep('string_list',4))
 extra$definition<-definitions;extra$allowed_values<-c('Short free text',
  vapply(review_coding_options()[c('sample_context','seb_measurement','study_contribution')],paste,collapse='; ',FUN.VALUE=character(1)),
  'Base codes in matrix_codebook, deduplicated; semicolon separated')
 extra$applies_to<-'all_retained_papers';extra$missing_rule<-'Blank = not yet assessed; distinguish Not reported and Not applicable.'
 extra$source<-'Full article, with page evidence';extra$coding_rule<-'Article-level value repeated consistently across its sample rows.';extra$rule_status<-'agreed'
 legacy$allowed_values[legacy$field=='study_design']<-paste(names(review_coding_options()$study_design),collapse='; ')
 legacy$type[legacy$field=='study_design']<-'string_list'
 rbind(legacy,extra)
}
