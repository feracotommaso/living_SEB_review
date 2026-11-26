# 0068 Table 2, PDF p.6: observed Pearson correlations, visually checked.
# Source values are a publication reference, not replacement effects.
source_map<-c(SMD='selfmanagement',IND='innovation',COD='cooperation',
 SED='socialengagement',ERD='emotionalresilience',achievement='academicachievement',
 Efficacy='academicselfefficacy',adapt='careeradaptability')
table2_variables<-c('SMD','IND','SED','COD','ERD','concern','control','curiosity',
 'confidence','achievement','Efficacy')
table2_lower<-list(numeric(),.54,c(.55,.52),c(.61,.51,.63),c(.50,.47,.43,.43),
 c(.54,.46,.43,.40,.28),c(.54,.45,.55,.41,.36,.64),
 c(.49,.50,.40,.43,.28,.64,.73),c(.59,.51,.47,.44,.38,.65,.72,.73),
 c(.37,.32,.23,.24,.14,.21,.24,.24,.29),
 c(.59,.45,.41,.39,.30,.38,.42,.40,.49,.69))
