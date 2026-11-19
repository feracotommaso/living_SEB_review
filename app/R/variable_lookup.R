# Shared variable lookup for standalone Shiny and the local coding workflow.
# Definitions and aliases describe constructs; they never recode correlations.
resolve_variable_lookup <- function(requested, variables, suffixes) {
  base <- variables[!is.na(variables$column_name) & nzchar(variables$column_name),,drop=FALSE]
  if(anyNA(requested)||any(!nzchar(requested))||anyDuplicated(requested))stop('Requested variable names must be present and unique.')
  rows <- lapply(requested,function(name) {
    j <- match(name,base$column_name);suffix <- ''
    if(is.na(j)) {
      hits <- which(vapply(suffixes$suffix,function(s)endsWith(name,s)&&substr(name,1,nchar(name)-nchar(s))%in%base$column_name,logical(1)))
      if(length(hits)!=1L)stop('Unregistered or ambiguous variable: ',name)
      suffix <- suffixes$suffix[hits];j <- match(substr(name,1,nchar(name)-nchar(suffix)),base$column_name)
    }
    row <- base[j,,drop=FALSE];row$base_name <- row$column_name;row$column_name <- name;row$suffix <- suffix
    if(nzchar(suffix))row$label <- paste0(row$label,' (',suffixes$label[match(suffix,suffixes$suffix)],')')
    rownames(row)<-NULL;row
  })
  if(!length(rows)){x<-base[FALSE,,drop=FALSE];x$base_name<-character();x$suffix<-character();return(x)}
  do.call(rbind,rows)
}

variable_menu_lookup <- function(variables, effect_names, suffixes) {
  base <- variables$column_name[!is.na(variables$column_name)&nzchar(variables$column_name)]
  variants <- unique(unlist(lapply(suffixes$suffix,function(s)paste0(base,s))))
  variants <- setdiff(variants,base)
  # Pair names are canonical IDs joined by '_'; IDs may themselves contain '_'.
  present <- variants[vapply(variants,function(v)any(startsWith(effect_names,paste0(v,'_'))|endsWith(effect_names,paste0('_',v))),logical(1))]
  resolve_variable_lookup(c(base,present),variables,suffixes)
}

variable_search_choices <- function(variables) {
  # Shiny selectize searches names; submitted values remain exact canonical IDs.
  label <- variables$label
  if('synonyms'%in%names(variables)) {
    aliases <- ifelse(is.na(variables$synonyms),'',variables$synonyms)
    label <- ifelse(nzchar(aliases),paste0(label,' [',aliases,']'),label)
  }
  stats::setNames(variables$column_name,label)
}
