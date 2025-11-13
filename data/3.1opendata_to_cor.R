#### --------------------------------------------------- 0013 --------------------------------------------------- ####

rm(list=ls())
d0013 <- readr::read_csv("data/3.meta_data/open_data/d0013.csv")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Reverse neuroticism
d0013$BFI_EmotionalStability <- (-1)*d0013$BFI_EmotionalStability

# Scores are already calculated. Just change the column names
names(d0013)[names(d0013) == "BFI_OpenMindedness"] <- "openness"
names(d0013)[names(d0013) == "BFI_Conscientiousness"] <- "conscientiousness"
names(d0013)[names(d0013) == "BFI_Extraversion"] <- "extraversion"
names(d0013)[names(d0013) == "BFI_Agreeableness"] <- "agreeableness"
names(d0013)[names(d0013) == "BFI_EmotionalStability"] <- "neuroticism"

names(d0013)[names(d0013) == "BESSI_SelfManagement"] <- "selfmanagement"
names(d0013)[names(d0013) == "BESSI_Cooperation"] <- "cooperation"
names(d0013)[names(d0013) == "BESSI_SocialEngagement"] <- "socialengagement"
names(d0013)[names(d0013) == "BESSI_EmotionalResilience"] <- "emotionalresilience"
names(d0013)[names(d0013) == "BESSI_Innovation"] <- "innovation"

names(d0013)[names(d0013) == "AcademicEngagement"] <- "academicengagement"
names(d0013)[names(d0013) == "SchoolGrades"] <- "academicachievement"
names(d0013)[names(d0013) == "MotherRelationshipQuality"] <- "motherrelationship"
names(d0013)[names(d0013) == "FatherRelationshipQuality"] <- "fatherrelationship"
names(d0013)[names(d0013) == "FriendshipQuality"] <- "friendshipquality"
names(d0013)[names(d0013) == "PeerAcceptance"] <- "peeracceptance"
names(d0013)[names(d0013) == "SocialResponsibilityValues"] <- "socialresponsibility"
names(d0013)[names(d0013) == "CivicSkills"] <- "civicskills"
names(d0013)[names(d0013) == "Anxiety"] <- "anxiety"
names(d0013)[names(d0013) == "Depression"] <- "depression"
names(d0013)[names(d0013) == "LifeSatisfaction"] <- "satisfactionwithlife"

d0013a <- d0013[, colnames(d0013) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0013a, use = "pairwise.complete")),"data/3.meta_data/matrices/0013a.xlsx")

# Individual data with age
d0013 <- d0013[, colnames(d0013) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0013),"data/3.meta_data/open_data/individual_data/0013a.xlsx")

#### --------------------------------------------------- 0018 --------------------------------------------------- ####

rm(list=ls())
d0018 <- readr::read_csv("data/3.meta_data/open_data/d0018.csv")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

#Self-Management skill facets
d0018$goalregulation<-rowMeans(d0018[,c("bessi_24", "bessi_56", "bessi_88", "bessi_120", "bessi_152", "bessi_184")], na.rm=TRUE) #Goal Regulation
d0018$taskmanagement<-rowMeans(d0018[,c("bessi_12", "bessi_44", "bessi_76", "bessi_108", "bessi_140", "bessi_172")], na.rm=TRUE) #Task Management
d0018$decisionmakingskill<-rowMeans(d0018[,c("bessi_27", "bessi_59", "bessi_91", "bessi_123", "bessi_155", "bessi_187")], na.rm=TRUE) #Decision-Making Skill
d0018$detailmanagement<-rowMeans(d0018[,c("bessi_15", "bessi_47", "bessi_79", "bessi_111", "bessi_143", "bessi_175")], na.rm=TRUE) #Detail Management
d0018$capacityforconsistency<-rowMeans(d0018[,c("bessi_9", "bessi_41", "bessi_73", "bessi_105", "bessi_137", "bessi_169")], na.rm=TRUE) #Capacity for Consistency
d0018$organizationalskill<-rowMeans(d0018[,c("bessi_6", "bessi_38", "bessi_70", "bessi_102", "bessi_134", "bessi_166")], na.rm=TRUE) #Organizational Skill
d0018$timemanagement<-rowMeans(d0018[,c("bessi_3", "bessi_35", "bessi_67", "bessi_99", "bessi_131", "bessi_163")], na.rm=TRUE) #Time Management
d0018$responsibilitymanagement<-rowMeans(d0018[,c("bessi_21", "bessi_53", "bessi_85", "bessi_117", "bessi_149", "bessi_181")], na.rm=TRUE) #Responsibility Management #21->193 #149->194
d0018$rulefollowingskill<-rowMeans(d0018[,c("bessi_18", "bessi_50", "bessi_82", "bessi_114", "bessi_146", "bessi_178")], na.rm=TRUE) #Rule Following Skill

#Innovation skill facets
d0018$abstractthinkingskill<-rowMeans(d0018[,c("bessi_4", "bessi_36", "bessi_68", "bessi_100", "bessi_132", "bessi_164")], na.rm=TRUE) #Abstract Thinking Skill #4->200
d0018$creativeskill<-rowMeans(d0018[,c("bessi_16", "bessi_48", "bessi_80", "bessi_112", "bessi_144", "bessi_176")], na.rm=TRUE) #Creative Skill
d0018$informationprocessingskill<-rowMeans(d0018[,c("bessi_22", "bessi_54", "bessi_86", "bessi_118", "bessi_150", "bessi_182")], na.rm=TRUE) #Information Processing Skill #86->202
d0018$culturalcompetence<-rowMeans(d0018[,c("bessi_32", "bessi_64", "bessi_96", "bessi_128", "bessi_160", "bessi_192")], na.rm=TRUE) #Cultural Competence #96->201
d0018$artisticskill<-rowMeans(d0018[,c("bessi_28", "bessi_60", "bessi_92", "bessi_124", "bessi_156", "bessi_188")], na.rm=TRUE) #Artistic Skill

#Cooperation skill facets
d0018$perspectivetakingskill<-rowMeans(d0018[,c("bessi_2", "bessi_34", "bessi_66", "bessi_98", "bessi_130", "bessi_162")], na.rm=TRUE) #Perspective Taking skill #34->196
d0018$capacityforsocialwarmth<-rowMeans(d0018[,c("bessi_14", "bessi_46", "bessi_78", "bessi_110", "bessi_142", "bessi_174")], na.rm=TRUE) #Capacity for Social Warmth
d0018$teamworkskill<-rowMeans(d0018[,c("bessi_23", "bessi_55", "bessi_87", "bessi_119", "bessi_151", "bessi_183")], na.rm=TRUE) #Teamwork Skill
d0018$ethicalcompetence<-rowMeans(d0018[,c("bessi_29", "bessi_61", "bessi_93", "bessi_125", "bessi_157", "bessi_189")], na.rm=TRUE) #Ethical Competence
d0018$capacityfortrust<-rowMeans(d0018[,c("bessi_8", "bessi_40", "bessi_72", "bessi_104", "bessi_136", "bessi_168")], na.rm=TRUE) #Capacity for Trust #8-197

#Social Engagement skill facets
d0018$leadershipskill<-rowMeans(d0018[,c("bessi_1", "bessi_33", "bessi_65", "bessi_97", "bessi_129", "bessi_161")], na.rm=TRUE) #Leadership Skill #1->195
d0018$expressiveskill<-rowMeans(d0018[,c("bessi_17", "bessi_49", "bessi_81", "bessi_113", "bessi_145", "bessi_177")], na.rm=TRUE) #Expressive Skill
d0018$conversationalskill<-rowMeans(d0018[,c("bessi_25", "bessi_57", "bessi_89", "bessi_121", "bessi_153", "bessi_185")], na.rm=TRUE) #Conversational Skill
d0018$persuasiveskill<-rowMeans(d0018[,c("bessi_13", "bessi_45", "bessi_77", "bessi_109", "bessi_141", "bessi_173")], na.rm=TRUE) #Persuasive Skill
d0018$energyregulation<-rowMeans(d0018[,c("bessi_7", "bessi_39", "bessi_71", "bessi_103", "bessi_135", "bessi_167")], na.rm=TRUE) #Energy Regulation

#Emotional Resilience skill facets
d0018$stressregulation<-rowMeans(d0018[,c("bessi_5", "bessi_37", "bessi_69", "bessi_101", "bessi_133", "bessi_165")], na.rm=TRUE) #Stress Regulation #5->198
d0018$capacityforoptimism<-rowMeans(d0018[,c("bessi_11", "bessi_43", "bessi_75", "bessi_107", "bessi_139", "bessi_171")], na.rm=TRUE) #Capacity for Optimism
d0018$confidenceregulation<-rowMeans(d0018[,c("bessi_26", "bessi_58", "bessi_90", "bessi_122", "bessi_154", "bessi_186")], na.rm=TRUE) #Confidence Regulation #122->199
d0018$impulseregulation<-rowMeans(d0018[,c("bessi_30", "bessi_62", "bessi_94", "bessi_126", "bessi_158", "bessi_190")], na.rm=TRUE) #Impulse Regulation
d0018$angermanagement<-rowMeans(d0018[,c("bessi_20", "bessi_52", "bessi_84", "bessi_116", "bessi_148", "bessi_180")], na.rm=TRUE) #Anger Management

#Compound skill facets
d0018$selfreflectionskill<-rowMeans(d0018[,c("bessi_10", "bessi_42", "bessi_74", "bessi_106", "bessi_138", "bessi_170")], na.rm=TRUE) #Self-Reflection Skill
d0018$capacityforindependence<-rowMeans(d0018[,c("bessi_31", "bessi_63", "bessi_95", "bessi_127", "bessi_159", "bessi_191")], na.rm=TRUE) #Capacity for Independence
d0018$adaptability<-rowMeans(d0018[,c("bessi_19", "bessi_51", "bessi_83", "bessi_115", "bessi_147", "bessi_179")], na.rm=TRUE) #Adaptability

#Five BESSI skill domains 
d0018$selfmanagement<-rowMeans(d0018[,c("goalregulation", "goalregulation", "taskmanagement", "taskmanagement", "decisionmakingskill", "decisionmakingskill", "detailmanagement", "detailmanagement",
                                        "capacityforconsistency", "capacityforconsistency", "organizationalskill", "organizationalskill", "timemanagement", "timemanagement", 
                                        "responsibilitymanagement", "responsibilitymanagement", "rulefollowingskill", "rulefollowingskill",
                                        "informationprocessingskill", "ethicalcompetence", "energyregulation", "impulseregulation")], na.rm=TRUE) #Self Management skill

d0018$innovation<-rowMeans(d0018[,c("abstractthinkingskill", "abstractthinkingskill", "creativeskill", "creativeskill", "culturalcompetence", "culturalcompetence",
                                    "artisticskill", "artisticskill", "informationprocessingskill")], na.rm=TRUE) #Innovation skill

d0018$cooperation<-rowMeans(d0018[,c("perspectivetakingskill", "perspectivetakingskill", "capacityforsocialwarmth", "capacityforsocialwarmth", "teamworkskill", "teamworkskill",
                                     "capacityfortrust", "capacityfortrust", "ethicalcompetence")], na.rm=TRUE) #Cooperation skill

d0018$socialengagement<-rowMeans(d0018[,c("leadershipskill", "leadershipskill", "expressiveskill", "expressiveskill", "conversationalskill", "conversationalskill",
                                          "persuasiveskill", "persuasiveskill", "energyregulation")], na.rm=TRUE) #Social Engagement skill

d0018$emotionalresilience<-rowMeans(d0018[,c("stressregulation", "stressregulation", "capacityforoptimism", "capacityforoptimism", "confidenceregulation", "confidenceregulation",
                                             "angermanagement", "angermanagement", "impulseregulation")], na.rm=TRUE) #Emotional Resilience skill

#Life satisfaction
d0018$satisfactionwithlife<-rowMeans(d0018[,c("SWLS_1", "SWLS_2", "SWLS_3", "SWLS_4", "SWLS_5")], na.rm = T)

# Achievement
d0018$math <- d0018$math
d0018$italian <- d0018$ita
d0018$academicachievement <- rowMeans(d0018[,c("math","italian")])

# Sex
d0018$sex <- d0018$sex - 1 # Males to 0, females to 1
d0018$age <- d0018$birth

d0018a <- d0018[d0018$dsa == "dsa", colnames(d0018) %in% admcol$column_name]
d0018b <- d0018[d0018$dsa == "typical", colnames(d0018) %in% admcol$column_name]

writexl::write_xlsx(data.frame(cor(d0018a, use = "pairwise.complete")),"data/3.meta_data/matrices/0018a.xlsx")
writexl::write_xlsx(data.frame(cor(d0018b, use = "pairwise.complete")),"data/3.meta_data/matrices/0018b.xlsx")

# Individual data with age
d0018a2 <- d0018[d0018$dsa == "dsa" , colnames(d0018) %in% c(admcol$column_name,"age")]
d0018b2 <- d0018[d0018$dsa == "typical", colnames(d0018) %in% c(admcol$column_name,"age")]

writexl::write_xlsx(data.frame(d0018a2),"data/3.meta_data/open_data/individual_data/0018a.xlsx")
writexl::write_xlsx(data.frame(d0018b2),"data/3.meta_data/open_data/individual_data/0018b.xlsx")

#### --------------------------------------------------- 0026 --------------------------------------------------- ####
rm(list=ls())
d0026 <- readr::read_csv("data/3.meta_data/open_data/d0026.csv")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Transform scores
d0026$sex <- ifelse(d0026$sex == 0, 1, ifelse(d0026$sex == 1, 0, d0026$sex)) # Males should be 0

# Scores are already calculated. Just change the column names
names(d0026)[names(d0026) == "Open_M"] <- "openness"
names(d0026)[names(d0026) == "Cons_M"] <- "conscientiousness"
names(d0026)[names(d0026) == "Extra_M"] <- "extraversion"
names(d0026)[names(d0026) == "Agree_M"] <- "agreeableness"
names(d0026)[names(d0026) == "Neu_M"] <- "neuroticism"

names(d0026)[names(d0026) == "SMang_M"] <- "selfmanagement"
names(d0026)[names(d0026) == "Coop_M"] <- "cooperation"
names(d0026)[names(d0026) == "SEnga_M"] <- "socialengagement"
names(d0026)[names(d0026) == "ERes_M"] <- "emotionalresilience"
names(d0026)[names(d0026) == "Innov_M"] <- "innovation"

names(d0026)[names(d0026) == "CWB_M"] <- "counterproductiveworkbehavior"
names(d0026)[names(d0026) == "FWG_M"] <- "coworkerrelationship"
names(d0026)[names(d0026) == "OCB_M"] <- "organizationalcitizenshipbehavior"
names(d0026)[names(d0026) == "JSB"] <- "jobsatisfaction"

# Additional scores are available, such as deal and sales centers (but very specific)
d0026a <- d0026[, colnames(d0026) %in% admcol$column_name]

writexl::write_xlsx(data.frame(cor(d0026a, use = "pairwise.complete")),"data/3.meta_data/matrices/0026a.xlsx")


# Individual data with age
d0026 <- d0026[, colnames(d0026) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0026),"data/3.meta_data/open_data/individual_data/0026a.xlsx")

#### --------------------------------------------------- 0030a --------------------------------------------------- ####
rm(list=ls())
d0030a <- readr::read_csv2("data/3.meta_data/open_data/d0030a.csv")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Transform scores
d0030a$sex <- d0030a$sex - 1
d0030a$age <- 2022 - d0030a$birth

#Self-Management skill facets
d0030a$goalregulation<-rowMeans(d0030a[,c("bessi_24", "bessi_56", "bessi_88", "bessi_120", "bessi_152", "bessi_184")], na.rm=TRUE) #Goal Regulation
d0030a$taskmanagement<-rowMeans(d0030a[,c("bessi_12", "bessi_44", "bessi_76", "bessi_108", "bessi_140", "bessi_172")], na.rm=TRUE) #Task Management
d0030a$decisionmakingskill<-rowMeans(d0030a[,c("bessi_27", "bessi_59", "bessi_91", "bessi_123", "bessi_155", "bessi_187")], na.rm=TRUE) #Decision-Making Skill
d0030a$detailmanagement<-rowMeans(d0030a[,c("bessi_15", "bessi_47", "bessi_79", "bessi_111", "bessi_143", "bessi_175")], na.rm=TRUE) #Detail Management
d0030a$capacityforconsistency<-rowMeans(d0030a[,c("bessi_9", "bessi_41", "bessi_73", "bessi_105", "bessi_137", "bessi_169")], na.rm=TRUE) #Capacity for Consistency
d0030a$organizationalskill<-rowMeans(d0030a[,c("bessi_6", "bessi_38", "bessi_70", "bessi_102", "bessi_134", "bessi_166")], na.rm=TRUE) #Organizational Skill
d0030a$timemanagement<-rowMeans(d0030a[,c("bessi_3", "bessi_35", "bessi_67", "bessi_99", "bessi_131", "bessi_163")], na.rm=TRUE) #Time Management
d0030a$responsibilitymanagement<-rowMeans(d0030a[,c("bessi_21", "bessi_53", "bessi_85", "bessi_117", "bessi_149", "bessi_181")], na.rm=TRUE) #Responsibility Management #21->193 #149->194
d0030a$rulefollowingskill<-rowMeans(d0030a[,c("bessi_18", "bessi_50", "bessi_82", "bessi_114", "bessi_146", "bessi_178")], na.rm=TRUE) #Rule Following Skill

#Innovation skill facets
d0030a$abstractthinkingskill<-rowMeans(d0030a[,c("bessi_4", "bessi_36", "bessi_68", "bessi_100", "bessi_132", "bessi_164")], na.rm=TRUE) #Abstract Thinking Skill #4->200
d0030a$creativeskill<-rowMeans(d0030a[,c("bessi_16", "bessi_48", "bessi_80", "bessi_112", "bessi_144", "bessi_176")], na.rm=TRUE) #Creative Skill
d0030a$informationprocessingskill<-rowMeans(d0030a[,c("bessi_22", "bessi_54", "bessi_86", "bessi_118", "bessi_150", "bessi_182")], na.rm=TRUE) #Information Processing Skill #86->202
d0030a$culturalcompetence<-rowMeans(d0030a[,c("bessi_32", "bessi_64", "bessi_96", "bessi_128", "bessi_160", "bessi_192")], na.rm=TRUE) #Cultural Competence #96->201
d0030a$artisticskill<-rowMeans(d0030a[,c("bessi_28", "bessi_60", "bessi_92", "bessi_124", "bessi_156", "bessi_188")], na.rm=TRUE) #Artistic Skill

#Cooperation skill facets
d0030a$perspectivetakingskill<-rowMeans(d0030a[,c("bessi_2", "bessi_34", "bessi_66", "bessi_98", "bessi_130", "bessi_162")], na.rm=TRUE) #Perspective Taking skill #34->196
d0030a$capacityforsocialwarmth<-rowMeans(d0030a[,c("bessi_14", "bessi_46", "bessi_78", "bessi_110", "bessi_142", "bessi_174")], na.rm=TRUE) #Capacity for Social Warmth
d0030a$teamworkskill<-rowMeans(d0030a[,c("bessi_23", "bessi_55", "bessi_87", "bessi_119", "bessi_151", "bessi_183")], na.rm=TRUE) #Teamwork Skill
d0030a$ethicalcompetence<-rowMeans(d0030a[,c("bessi_29", "bessi_61", "bessi_93", "bessi_125", "bessi_157", "bessi_189")], na.rm=TRUE) #Ethical Competence
d0030a$capacityfortrust<-rowMeans(d0030a[,c("bessi_8", "bessi_40", "bessi_72", "bessi_104", "bessi_136", "bessi_168")], na.rm=TRUE) #Capacity for Trust #8-197

#Social Engagement skill facets
d0030a$leadershipskill<-rowMeans(d0030a[,c("bessi_1", "bessi_33", "bessi_65", "bessi_97", "bessi_129", "bessi_161")], na.rm=TRUE) #Leadership Skill #1->195
d0030a$expressiveskill<-rowMeans(d0030a[,c("bessi_17", "bessi_49", "bessi_81", "bessi_113", "bessi_145", "bessi_177")], na.rm=TRUE) #Expressive Skill
d0030a$conversationalskill<-rowMeans(d0030a[,c("bessi_25", "bessi_57", "bessi_89", "bessi_121", "bessi_153", "bessi_185")], na.rm=TRUE) #Conversational Skill
d0030a$persuasiveskill<-rowMeans(d0030a[,c("bessi_13", "bessi_45", "bessi_77", "bessi_109", "bessi_141", "bessi_173")], na.rm=TRUE) #Persuasive Skill
d0030a$energyregulation<-rowMeans(d0030a[,c("bessi_7", "bessi_39", "bessi_71", "bessi_103", "bessi_135", "bessi_167")], na.rm=TRUE) #Energy Regulation

#Emotional Resilience skill facets
d0030a$stressregulation<-rowMeans(d0030a[,c("bessi_5", "bessi_37", "bessi_69", "bessi_101", "bessi_133", "bessi_165")], na.rm=TRUE) #Stress Regulation #5->198
d0030a$capacityforoptimism<-rowMeans(d0030a[,c("bessi_11", "bessi_43", "bessi_75", "bessi_107", "bessi_139", "bessi_171")], na.rm=TRUE) #Capacity for Optimism
d0030a$confidenceregulation<-rowMeans(d0030a[,c("bessi_26", "bessi_58", "bessi_90", "bessi_122", "bessi_154", "bessi_186")], na.rm=TRUE) #Confidence Regulation #122->199
d0030a$impulseregulation<-rowMeans(d0030a[,c("bessi_30", "bessi_62", "bessi_94", "bessi_126", "bessi_158", "bessi_190")], na.rm=TRUE) #Impulse Regulation
d0030a$angermanagement<-rowMeans(d0030a[,c("bessi_20", "bessi_52", "bessi_84", "bessi_116", "bessi_148", "bessi_180")], na.rm=TRUE) #Anger Management

#Compound skill facets
d0030a$selfreflectionskill<-rowMeans(d0030a[,c("bessi_10", "bessi_42", "bessi_74", "bessi_106", "bessi_138", "bessi_170")], na.rm=TRUE) #Self-Reflection Skill
d0030a$capacityforindependence<-rowMeans(d0030a[,c("bessi_31", "bessi_63", "bessi_95", "bessi_127", "bessi_159", "bessi_191")], na.rm=TRUE) #Capacity for Independence
d0030a$adaptability<-rowMeans(d0030a[,c("bessi_19", "bessi_51", "bessi_83", "bessi_115", "bessi_147", "bessi_179")], na.rm=TRUE) #Adaptability

#Five BESSI skill domains 
d0030a$selfmanagement<-rowMeans(d0030a[,c("goalregulation", "goalregulation", "taskmanagement", "taskmanagement", "decisionmakingskill", "decisionmakingskill", "detailmanagement", "detailmanagement",
                                        "capacityforconsistency", "capacityforconsistency", "organizationalskill", "organizationalskill", "timemanagement", "timemanagement", 
                                        "responsibilitymanagement", "responsibilitymanagement", "rulefollowingskill", "rulefollowingskill",
                                        "informationprocessingskill", "ethicalcompetence", "energyregulation", "impulseregulation")], na.rm=TRUE) #Self Management skill

d0030a$innovation<-rowMeans(d0030a[,c("abstractthinkingskill", "abstractthinkingskill", "creativeskill", "creativeskill", "culturalcompetence", "culturalcompetence",
                                    "artisticskill", "artisticskill", "informationprocessingskill")], na.rm=TRUE) #Innovation skill

d0030a$cooperation<-rowMeans(d0030a[,c("perspectivetakingskill", "perspectivetakingskill", "capacityforsocialwarmth", "capacityforsocialwarmth", "teamworkskill", "teamworkskill",
                                     "capacityfortrust", "capacityfortrust", "ethicalcompetence")], na.rm=TRUE) #Cooperation skill

d0030a$socialengagement<-rowMeans(d0030a[,c("leadershipskill", "leadershipskill", "expressiveskill", "expressiveskill", "conversationalskill", "conversationalskill",
                                          "persuasiveskill", "persuasiveskill", "energyregulation")], na.rm=TRUE) #Social Engagement skill

d0030a$emotionalresilience<-rowMeans(d0030a[,c("stressregulation", "stressregulation", "capacityforoptimism", "capacityforoptimism", "confidenceregulation", "confidenceregulation",
                                             "angermanagement", "angermanagement", "impulseregulation")], na.rm=TRUE) #Emotional Resilience skill

d0030a2 <- d0030a
d0030a <- d0030a[, colnames(d0030a) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0030a, use = "pairwise.complete")),"data/3.meta_data/matrices/0030a.xlsx")

# Individual data with age
d0030a2 <- d0030a2[, colnames(d0030a2) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0030a2),"data/3.meta_data/open_data/individual_data/0030a.xlsx")

#### --------------------------------------------------- 0030b --------------------------------------------------- ####
rm(list=ls())
dd <- readr::read_csv("data/3.meta_data/open_data/d0030b.csv")
d0030b <- readr::read_csv("data/3.meta_data/open_data/d0030b.csv")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Transform scores
d0030b$sex <- d0030b$sex - 1

# SMD
names(d0030b)[names(d0030b) == "SMD_GR"] <- "goalregulation"
names(d0030b)[names(d0030b) == "SMD_TaM"] <- "taskmanagement"
names(d0030b)[names(d0030b) == "SMD_DMS"] <- "decisionmakingskill"
names(d0030b)[names(d0030b) == "SMD_DM"] <- "detailmanagement"
names(d0030b)[names(d0030b) == "SMD_CC"] <- "capacityforconsistency"
names(d0030b)[names(d0030b) == "SMD_OS"] <- "organizationalskill"
names(d0030b)[names(d0030b) == "SMD_TiM"] <- "timemanagement"
names(d0030b)[names(d0030b) == "SMD_RM"] <- "responsibilitymanagement"
names(d0030b)[names(d0030b) == "SMD_RFS"] <- "rulefollowingskill"
names(d0030b)[names(d0030b) == "SMD"] <- "selfmanagement"

# IND
names(d0030b)[names(d0030b) == "IND_ATS"] <- "abstractthinkingskill"
names(d0030b)[names(d0030b) == "IND_CS"] <- "creativeskill"
names(d0030b)[names(d0030b) == "IND_IPS"] <- "informationprocessingskill"
names(d0030b)[names(d0030b) == "IND_CC"] <- "culturalcompetence"
names(d0030b)[names(d0030b) == "IND_AS"] <- "artisticskill"
names(d0030b)[names(d0030b) == "IND"] <- "innovation"

# COD
names(d0030b)[names(d0030b) == "COD_PTS"] <- "perspectivetakingskill"
names(d0030b)[names(d0030b) == "COD_CSW"] <- "capacityforsocialwarmth"
names(d0030b)[names(d0030b) == "COD_TS"] <- "teamworkskill"
names(d0030b)[names(d0030b) == "COD_EC"] <- "ethicalcompetence"
names(d0030b)[names(d0030b) == "COD_CT"] <- "capacityfortrust"
names(d0030b)[names(d0030b) == "COD"] <- "cooperation"

# SED
names(d0030b)[names(d0030b) == "SED_LS"] <- "leadershipskill"
names(d0030b)[names(d0030b) == "SED_ES"] <- "expressiveskill"
names(d0030b)[names(d0030b) == "SED_CS"] <- "conversationalskill"
names(d0030b)[names(d0030b) == "SED_PS"] <- "persuasiveskill"
names(d0030b)[names(d0030b) == "SED_ER"] <- "energyregulation"
names(d0030b)[names(d0030b) == "SED"] <- "socialengagement"

# ESD
names(d0030b)[names(d0030b) == "ESD_SR"] <- "stressregulation"
names(d0030b)[names(d0030b) == "ESD_CO"] <- "capacityforoptimism"
names(d0030b)[names(d0030b) == "ESD_CR"] <- "confidenceregulation"
names(d0030b)[names(d0030b) == "ESD_IR"] <- "impulseregulation"
names(d0030b)[names(d0030b) == "ESD_AM"] <- "angermanagement"
names(d0030b)[names(d0030b) == "ESD"] <- "emotionalresilience"

# Interstitial
names(d0030b)[names(d0030b) == "SRS"] <- "selfreflectionskill"
names(d0030b)[names(d0030b) == "CI"] <- "capacityforindependence"
names(d0030b)[names(d0030b) == "AD"] <- "adaptability"

# Other
names(d0030b)[names(d0030b) == "BF_C"] <- "conscientiousness"
names(d0030b)[names(d0030b) == "BF_O"] <- "openness"
names(d0030b)[names(d0030b) == "BF_A"] <- "agreeableness"
names(d0030b)[names(d0030b) == "BF_E"] <- "extraversion"
names(d0030b)[names(d0030b) == "BF_N"] <- "neuroticism"

names(d0030b)[names(d0030b) == "GSES"] <- "selfefficacy"
names(d0030b)[names(d0030b) == "GPS"] <- "procrastination"
names(d0030b)[names(d0030b) == "suppression"] <- "suppressionstrat"
names(d0030b)[names(d0030b) == "reappraisal"] <- "reappraisalstrat"

# Select and save
d0030b2 <- d0030b
d0030b <- d0030b[, colnames(d0030b) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0030b, use = "pairwise.complete")),"data/3.meta_data/matrices/0030b.xlsx")

# Individual data with age
d0030b2 <- d0030b2[, colnames(d0030b2) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0030b2),"data/3.meta_data/open_data/individual_data/0030b.xlsx")

#### --------------------------------------------------- 0034 --------------------------------------------------- ####
rm(list=ls())
d0034 <- haven::read_sav("data/3.meta_data/open_data/d0034t.sav")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Transform scores
d0034$sex <- ifelse(d0034$sexo == 2, 0, d0034$sexo)
d0034$age <- d0034$edad
names(d0034)[names(d0034) == "estudios"] <- "educationlevel"

#Seb
names(d0034)[names(d0034) == "DOMINIO_SelfManagement"] <- "selfmanagement"
names(d0034)[names(d0034) == "DOMINIO_SocialEngagement"] <- "socialengagement"
names(d0034)[names(d0034) == "DOMINIO_Cooperation"] <- "cooperation"
names(d0034)[names(d0034) == "DOMINIO_EmotionalResilience"] <- "emotionalresilience"
names(d0034)[names(d0034) == "DOMINIO_Innovation"] <- "innovation"
# Facets
# SMD
names(d0034)[names(d0034) == "GR"] <- "goalregulation"
names(d0034)[names(d0034) == "TaskM"] <- "taskmanagement"
names(d0034)[names(d0034) == "DecM"] <- "decisionmakingskill"
names(d0034)[names(d0034) == "DM"] <- "detailmanagement"
names(d0034)[names(d0034) == "CC"] <- "capacityforconsistency"
names(d0034)[names(d0034) == "ORR"] <- "organizationalskill"
names(d0034)[names(d0034) == "TM"] <- "timemanagement"
names(d0034)[names(d0034) == "RM"] <- "responsibilitymanagement"
names(d0034)[names(d0034) == "RF"] <- "rulefollowingskill"
# IND
names(d0034)[names(d0034) == "ABS"] <- "abstractthinkingskill"
names(d0034)[names(d0034) == "CREA"] <- "creativeskill"
names(d0034)[names(d0034) == "IP"] <- "informationprocessingskill"
names(d0034)[names(d0034) == "CU"] <- "culturalcompetence"
names(d0034)[names(d0034) == "ART"] <- "artisticskill"
# COD
names(d0034)[names(d0034) == "PT"] <- "perspectivetakingskill"
names(d0034)[names(d0034) == "CSW"] <- "capacityforsocialwarmth"
names(d0034)[names(d0034) == "TW"] <- "teamworkskill"
names(d0034)[names(d0034) == "EC"] <- "ethicalcompetence"
names(d0034)[names(d0034) == "CT"] <- "capacityfortrust"
# SED
names(d0034)[names(d0034) == "LS"] <- "leadershipskill"
names(d0034)[names(d0034) == "Ex"] <- "expressiveskill"
names(d0034)[names(d0034) == "CV"] <- "conversationalskill"
names(d0034)[names(d0034) == "PS"] <- "persuasiveskill"
names(d0034)[names(d0034) == "ER"] <- "energyregulation"
# ESD
names(d0034)[names(d0034) == "SR"] <- "stressregulation"
names(d0034)[names(d0034) == "COP"] <- "capacityforoptimism"
names(d0034)[names(d0034) == "CR"] <- "confidenceregulation"
names(d0034)[names(d0034) == "IR"] <- "impulseregulation"
names(d0034)[names(d0034) == "AM"] <- "angermanagement"
# Interstitial
names(d0034)[names(d0034) == "REF"] <- "selfreflectionskill"
names(d0034)[names(d0034) == "IND"] <- "capacityforindependence"
names(d0034)[names(d0034) == "ADA"] <- "adaptability"

# Rescale to mean item values
d0034[,which(colnames(d0034)=="goalregulation"):
        which(colnames(d0034)=="adaptability")] <-
  d0034[,which(colnames(d0034)=="goalregulation"):
          which(colnames(d0034)=="adaptability")]/6
d0034$selfmanagement <- d0034$selfmanagement / 54
d0034$cooperation <- d0034$cooperation / 30
d0034$emotionalresilience <- d0034$emotionalresilience / 30
d0034$innovation <- d0034$innovation / 30
d0034$socialengagement <- d0034$socialengagement / 30

# Traits
names(d0034)[names(d0034) == "Conscientiousness"] <- "conscientiousness"
names(d0034)[names(d0034) == "OpenMindedness"] <- "openness"
names(d0034)[names(d0034) == "Agreeableness"] <- "agreeableness"
names(d0034)[names(d0034) == "Extraversion"] <- "extraversion"
names(d0034)[names(d0034) == "Negative_Emotionality"] <- "neuroticism"
# d0043$neuroticism <- (-1)*d0043$neuroticism
# O
names(d0034)[names(d0034) == "Curiosity"] <- "bf_curiosity"
names(d0034)[names(d0034) == "AestheticSensitivity"] <- "bf_aesthetic"
names(d0034)[names(d0034) == "Creativity"] <- "bf_creativity"
# C
names(d0034)[names(d0034) == "Organization"] <- "bf_organization"
names(d0034)[names(d0034) == "Productiveness"] <- "bf_productiveness"
names(d0034)[names(d0034) == "Responsability"] <- "bf_responsibility"
# E
names(d0034)[names(d0034) == "Sociabilidad"] <- "bf_sociability"
names(d0034)[names(d0034) == "Assertiviness"] <- "bf_assertiveness"
names(d0034)[names(d0034) == "Energy"] <- "bf_energy"
# A
names(d0034)[names(d0034) == "Compassion"] <- "bf_compassion"
names(d0034)[names(d0034) == "Respectfulness"] <- "bf_respectfulness"
names(d0034)[names(d0034) == "Trust"] <- "bf_trust"
# N
names(d0034)[names(d0034) == "Anxiety"] <- "bf_anxiety"
names(d0034)[names(d0034) == "Depression"] <- "bf_depression"
names(d0034)[names(d0034) == "Volatility"] <- "bf_emotionalvolatility"

#Casel
names(d0034)[names(d0034) == "SelfmanagementMotivation"] <- "secaSelfmanagement"
names(d0034)[names(d0034) == "SocialawarenessProsocialbehavior"] <- "secaSocialawareness"
names(d0034)[names(d0034) == "Selfawareness"] <- "secaSelfawareness"
names(d0034)[names(d0034) == "DecisionMaking"] <- "secaDecisionmaking"

#Mindset
names(d0034)[names(d0034) == "MindsetGrowth"] <- "growthmindset"
names(d0034)[names(d0034) == "MindsetFixed"] <- "fixedmindset"

names(d0034)[names(d0034) == "Grit"] <- "grit"
names(d0034)[names(d0034) == "Curiosity_Test"] <- "epistemiccuriosity"

names(d0034)[names(d0034) == "PersonalidadEmprendedora"] <- "enterprisingpersonality"

# Internet
names(d0034)[names(d0034) == "ComparaciónSocial_Redes"] <- "socialnetworkaddictiveconsequences"
names(d0034)[names(d0034) == "ConsecuenciasAdictivas_Redes"] <- "socialnetworknegativesocialcomparison"

# Select and save
d0034a <- d0034[, colnames(d0034) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0034a, use = "pairwise.complete")),"data/3.meta_data/matrices/0034a.xlsx")

# Individual data with age
d0034 <- d0034[, colnames(d0034) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0034),"data/3.meta_data/open_data/individual_data/0034a.xlsx")

#### --------------------------------------------------- 0035 --------------------------------------------------- ####
rm(list=ls())
d0035 <- readr::read_csv("data/3.meta_data/open_data/d0035.csv")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

names(d0035)[names(d0035) == "AVGP"] <- "goalregulation"
names(d0035)[names(d0035) == "AVLD"] <- "leadershipskill"
names(d0035)[names(d0035) == "AVCL"] <- "teamworkskill"
names(d0035)[names(d0035) == "AVIC"] <- "abstractthinkingskill"
names(d0035)[names(d0035) == "AVSR"] <- "stressregulation"

names(d0035)[names(d0035) == "bfi_C"] <- "conscientiousness"
names(d0035)[names(d0035) == "bfi_O"] <- "openness"
names(d0035)[names(d0035) == "bfi_A"] <- "agreeableness"
names(d0035)[names(d0035) == "bfi_E"] <- "extraversion"
names(d0035)[names(d0035) == "bfi_ES"] <- "neuroticism"

names(d0035)[names(d0035) == "RPTD_STDSCN_C"] <- "standardizedacademicachievement"

# Transform emotional stability to neuroticism
d0035$neuroticism <- d0035$neuroticism*(-1)

# Select and save
d0035a <- d0035[, colnames(d0035) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0035a, use = "pairwise.complete")),"data/3.meta_data/matrices/0035a.xlsx")

# Individual data with age
d0035 <- d0035[, colnames(d0035) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0035),"data/3.meta_data/open_data/individual_data/0035a.xlsx")

#### --------------------------------------------------- 0043 --------------------------------------------------- ####
rm(list=ls())
d0043 <- haven::read_sav("data/3.meta_data/open_data/d0043.sav")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Transform scores
d0043$sex <- ifelse(d0043$Gender == -1, 0, d0043$Gender)

#ses
names(d0043)[names(d0043) == "ParentsEducation"] <- "ParentEducation"
#Seb
names(d0043)[names(d0043) == "Skill_SelfManagement"] <- "selfmanagement"
names(d0043)[names(d0043) == "Skill_SocialEngagement"] <- "socialengagement"
names(d0043)[names(d0043) == "Skill_Cooperation"] <- "cooperation"
names(d0043)[names(d0043) == "Skill_EmotionalResilience"] <- "emotionalresilience"
names(d0043)[names(d0043) == "Skill_Innovation"] <- "innovation"
#Traits
names(d0043)[names(d0043) == "Trait_SelfManagement"] <- "conscientiousness"
names(d0043)[names(d0043) == "Trait_Innovation"] <- "openness"
names(d0043)[names(d0043) == "Trait_Cooperation"] <- "agreeableness"
names(d0043)[names(d0043) == "Trait_SocialEngagement"] <- "extraversion"
names(d0043)[names(d0043) == "Trait_EmotionalResilience"] <- "neuroticism"
d0043$neuroticism <- (-1)*d0043$neuroticism
#Mosaic
names(d0043)[names(d0043) == "Mosaic_SustainingEffort"] <- "mosaicSustainingEffort"
names(d0043)[names(d0043) == "Mosaic_SocialConnection"] <- "mosaicSocialConnection"
names(d0043)[names(d0043) == "Mosaic_GettingAlongWithOthers"] <- "mosaicGettingAlong"
names(d0043)[names(d0043) == "Mosaic_MaintainingComposure"] <- "mosaicComposure"
names(d0043)[names(d0043) == "Mosaic_KeepingAnOpenMind"] <- "mosaicOpenMind"
#School
names(d0043)[names(d0043) == "ActTestScore"] <- "standardizedacademicachievement"
names(d0043)[names(d0043) == "OverallGPA"] <- "academicachievement"
names(d0043)[names(d0043) == "SchoolAttendance"] <- "schoolattendance"
names(d0043)[names(d0043) == "EducationalAspirations"] <- "educationalaspiration"


# Select and save
d0043a <- d0043[, colnames(d0043) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0043a, use = "pairwise.complete")),"data/3.meta_data/matrices/0043a.xlsx")

# Individual data with age
d0043 <- d0043[, colnames(d0043) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0043),"data/3.meta_data/open_data/individual_data/0043a.xlsx")

#### --------------------------------------------------- 0045 --------------------------------------------------- ####
rm(list=ls())
d0045 <- readxl::read_excel("data/3.meta_data/open_data/d0045.xlsx")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Transform scores
d0045$sex <- ifelse(d0045$gender == 2, NA, ifelse(d0045$gender==0,1,0))
d0045$volunteering <- d0045$hour_group

#Self-Management skill facets
d0045$taskmanagement<-rowMeans(d0045[,c("bessi_12", "bessi_44", "bessi_76", "bessi_108", "bessi_140", "bessi_172")], na.rm=TRUE) #Task Management
#Social Engagement skill facets
d0045$leadershipskill<-rowMeans(d0045[,c("bessi_1", "bessi_33", "bessi_65", "bessi_97", "bessi_129", "bessi_161")], na.rm=TRUE) #Leadership Skill #1->195
#Innovation skill facets
d0045$creativeskill<-rowMeans(d0045[,c("bessi_16", "bessi_48", "bessi_80", "bessi_112", "bessi_144", "bessi_176")], na.rm=TRUE) #Creative Skill
d0045$abstractthinkingskill<-rowMeans(d0045[,c("bessi_4", "bessi_36", "bessi_68", "bessi_100", "bessi_132", "bessi_164")], na.rm=TRUE) #Abstract Thinking Skill #4->200
d0045$culturalcompetence<-rowMeans(d0045[,c("bessi_32", "bessi_64", "bessi_96", "bessi_128", "bessi_160", "bessi_192")], na.rm=TRUE) #Cultural Competence #96->201
#Cooperation skill facets
d0045$perspectivetakingskill<-rowMeans(d0045[,c("bessi_2", "bessi_34", "bessi_66", "bessi_98", "bessi_130", "bessi_162")], na.rm=TRUE) #Perspective Taking skill #34->196
#Emotional Resilience skill facets
d0045$stressregulation<-rowMeans(d0045[,c("bessi_5", "bessi_37", "bessi_69", "bessi_101", "bessi_133", "bessi_165")], na.rm=TRUE) #Stress Regulation #5->198

# Select and save
d0045a <- d0045[, colnames(d0045) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0045a, use = "pairwise.complete")),"data/3.meta_data/matrices/0045a.xlsx")

# Individual data with age
d0045 <- d0045[, colnames(d0045) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0045),"data/3.meta_data/open_data/individual_data/0045a.xlsx")

#### --------------------------------------------------- 0048 --------------------------------------------------- ####
rm(list=ls())
d0048 <- haven::read_sav("data/3.meta_data/open_data/d0048.sav")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Transform scores
d0048$sex <- ifelse(d0048$Gender == -1, 0, d0048$Gender)

# Transform colnames
names(d0048)[names(d0048) == "Age"] <- "age"

#Seb
names(d0048)[names(d0048) == "BESSI_SelfManagement"] <- "selfmanagement"
names(d0048)[names(d0048) == "BESSI_SocialEngagement"] <- "socialengagement"
names(d0048)[names(d0048) == "BESSI_Cooperation"] <- "cooperation"
names(d0048)[names(d0048) == "BESSI_EmotionalResilience"] <- "emotionalresilience"
names(d0048)[names(d0048) == "BESSI_Innovation"] <- "innovation"
#Traits
names(d0048)[names(d0048) == "BFI_Conscientiousness"] <- "conscientiousness"
names(d0048)[names(d0048) == "BFI_OpenMindedness"] <- "openness"
names(d0048)[names(d0048) == "BFI_Agreeableness"] <- "agreeableness"
names(d0048)[names(d0048) == "BFI_Extraversion"] <- "extraversion"
names(d0048)[names(d0048) == "BFI_NegativeEmotionality"] <- "neuroticism"
#Casel
names(d0048)[names(d0048) == "SECA_SelfManagement"] <- "secaSelfmanagement"
names(d0048)[names(d0048) == "SECA_RelationshipSkills"] <- "secaRelationships"
names(d0048)[names(d0048) == "SECA_SocialAwareness"] <- "secaSocialawareness"
names(d0048)[names(d0048) == "SECA_SelfAwareness"] <- "secaSelfawareness"
names(d0048)[names(d0048) == "SECA_ResponsibleDecisionMaking"] <- "secaDecisionmaking"
#School
names(d0048)[names(d0048) == "AcademicEngagement"] <- "academicengagement"
names(d0048)[names(d0048) == "SchoolGrades"] <- "academicachievement"
names(d0048)[names(d0048) == "SchoolAttendance"] <- "schoolattendance"
names(d0048)[names(d0048) == "SchoolDisciplineReferral"] <- "schooldiscipline"
#Creative
names(d0048)[names(d0048) == "RealisticInterests"] <- "realisticinterests"
names(d0048)[names(d0048) == "InvestigativeInterests"] <- "investigativeinterests"
names(d0048)[names(d0048) == "ArtisticInterests"] <- "artisticinterests"
names(d0048)[names(d0048) == "SocialInterests"] <- "socialinterests"
names(d0048)[names(d0048) == "EnterprisingInterests"] <- "enterprisinginterests"
names(d0048)[names(d0048) == "ConventionalInterests"] <- "conventionalinterests"
#Social
names(d0048)[names(d0048) == "PeerAcceptance"] <- "peeracceptance"
names(d0048)[names(d0048) == "FriendshipQuality"] <- "friendshipquality"
names(d0048)[names(d0048) == "MotherRelationshipQuality"] <- "motherrelationship"
names(d0048)[names(d0048) == "FatherRelationshipQuality"] <- "fatherrelationship"
names(d0048)[names(d0048) == "SocialResponsibilityValues"] <- "socialresponsibility"
names(d0048)[names(d0048) == "CivicSkills"] <- "civicskills"
names(d0048)[names(d0048) == "VotingIntention"] <- "votingintention"
names(d0048)[names(d0048) == "CivicOrganizationInvolvement"] <- "civicinvolvement"
names(d0048)[names(d0048) == "Activism"] <- "activism"
names(d0048)[names(d0048) == "InformalHelping"] <- "informalhelping"
names(d0048)[names(d0048) == "Volunteerism"] <- "volunteering"
#Physical
names(d0048)[names(d0048) == "PhysicalExercise"] <- "physicalexercise"
#Mental health
names(d0048)[names(d0048) == "Anxiety"] <- "anxiety"
names(d0048)[names(d0048) == "Depression"] <- "depression"
names(d0048)[names(d0048) == "LifeSatisfaction"] <- "satisfactionwithlife"

# Select and save
d0048a <- d0048[, colnames(d0048) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0048a, use = "pairwise.complete")),"data/3.meta_data/matrices/0048a.xlsx")

# Individual data with age
d0048 <- d0048[, colnames(d0048) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0048),"data/3.meta_data/open_data/individual_data/0048a.xlsx")

#### --------------------------------------------------- 0049 --------------------------------------------------- ####
rm(list=ls())
d0049 <- readxl::read_excel("data/3.meta_data/open_data/d0049.xlsx", na = "NA")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# SEX
d0049$sex <- d0049$Gender #Gender is already coded 0 = M; 1 = F
#Big Five Scores
#Recode reversed Items. Items that need to be reversed are those with "_R"
d0049$BFI01 <- 6 - d0049$BFI01_R
d0049$BFI03 <- 6 - d0049$BFI03_R
d0049$BFI07 <- 6 - d0049$BFI07_R
d0049$BFI08 <- 6 - d0049$BFI08_R
d0049$BFI10 <- 6 - d0049$BFI10_R
d0049$BFI14 <- 6 - d0049$BFI14_R
d0049$BFI17 <- 6 - d0049$BFI17_R
d0049$BFI19 <- 6 - d0049$BFI19_R
d0049$BFI20 <- 6 - d0049$BFI20_R
d0049$BFI21 <- 6 - d0049$BFI21_R
d0049$BFI24 <- 6 - d0049$BFI24_R
d0049$BFI26 <- 6 - d0049$BFI26_R
d0049$BFI27 <- 6 - d0049$BFI27_R
d0049$BFI28 <- 6 - d0049$BFI28_R
d0049$BFI30 <- 6 - d0049$BFI30_R

#Create Big Five Scales. E = Extraversion, A = Agreeableness, N = Negative emotionality
d0049$extraversion <- rowMeans (select(d0049,BFI01,BFI06,BFI11,BFI16,BFI21,BFI26))
d0049$agreeableness <- rowMeans (select(d0049,BFI02,BFI07,BFI12,BFI17,BFI22,BFI27))
d0049$neuroticism <- rowMeans (select(d0049,BFI04,BFI09,BFI14,BFI19,BFI24,BFI29))

#Build Big Five Facet Scores
#Extraversion: S = Sociability, A = Assertiveness, E = Energy level
d0049$bf_sociability <- rowMeans (select(d0049,BFI01,BFI16))
d0049$bf_assertiveness <- rowMeans (select(d0049,BFI06,BFI21))
d0049$bf_energy <- rowMeans (select(d0049,BFI11,BFI26))

#Agreeableness: C = Compassion, R = Respectfulness, T = Trust
d0049$bf_compassion <- rowMeans (select(d0049,BFI02,BFI17))
d0049$bf_respectfulness <- rowMeans (select(d0049,BFI07,BFI22))
d0049$bf_trust <- rowMeans (select(d0049,BFI12,BFI27))

#Negative emotionality: A = Anxiety, D = Depression, E = Emotional volatility
d0049$bf_anxiety <- rowMeans (select(d0049,BFI04,BFI19))
d0049$bf_depression <- rowMeans (select(d0049,BFI09,BFI24))
d0049$emotionalvolatility <- rowMeans (select(d0049,BFI14,BFI29))

#Create BESSI Scales
#Leadership skill
d0049$leadershipskill <- rowMeans (select(d0049,Lead1,Lead2,Lead3,Lead4,Lead5,Lead6))
#Persuasion skill with alternative translations
d0049$persuasiveskill <- rowMeans (select(d0049,Pers1,Pers2,Pers3,Pers4,Pers5,Pers6a))

#Perspective-taking skill
d0049$perspectivetakingskill <- rowMeans (select(d0049,Empa1,Empa2,Empa3,Empa4,Empa5,Empa6))
#Capacity for social warmth
d0049$capacityforsocialwarmth <- rowMeans (select(d0049,Soha1,Soha2,Soha3,Soha4,Soha5,Soha6))

#Stress regulation
d0049$stressregulation <- rowMeans (select(d0049,Stres1,Stres2,Stres3,Stres4,Stres5,Stres6))
#Anger management
d0049$angermanagement <- rowMeans (select(d0049,Angm1,Angm2,Angm3,Angm4,Angm5,Angm6))

#Agency skill
d0049$socialengagement <- rowMeans (select(d0049,leadershipskill,persuasiveskill))

#Communion skill
d0049$cooperation <- rowMeans (select(d0049,perspectivetakingskill,capacityforsocialwarmth))

#Resilience skill
d0049$emotionalresilience <- rowMeans (select(d0049,stressregulation,angermanagement))

#Observed skills - aggregate across raters
d0049$task_socialengagement <- rowMeans (select(d0049,Agency_Observed_R1,Agency_Observed_R2),na.rm =T)
d0049$task_cooperation <- rowMeans (select(d0049,Communion_Observed_R1,Communion_Observed_R2),na.rm =T)
d0049$task_emotionalresilience <- rowMeans (select(d0049,Resilience_Observed_R1,Resilience_Observed_R2,Resilience_Observed_R3),na.rm =T)

# Select and save
d0049a <- d0049[, colnames(d0049) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0049a, use = "pairwise.complete")),"data/3.meta_data/matrices/0049a.xlsx")

# Individual data with age
d0049 <- d0049[, colnames(d0049) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0049),"data/3.meta_data/open_data/individual_data/0049a.xlsx")

#### --------------------------------------------------- 0050a --------------------------------------------------- ####
rm(list=ls())
load("data/3.meta_data/open_data/d0050.Rdata")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Skill domains
d0050a <- adults_t4 %>% 
  rowwise() %>% 
  mutate(
    selfmanagement = mean(c_across(c("tafo_prm", "time_prm", "deta_prm", "orga_prm", "resp_prm",
                                     "cons_prm", "goal_prm", "rule_prm", "deci_prm"))),
    socialengagement= mean(c_across(c("lead_prm", "pers_prm", "conv_prm", "expr_prm"))),
    emotionalresilience = mean(c_across(c("stre_prm", "opti_prm", "angm_prm", "scon_prm"))),
    cooperation = mean(c_across(c("team_prm", "trus_prm", "empa_prm", "soha_prm"))),
    innovation = mean(c_across(c("crea_prm", "curi_prm", "arti_prm", "cult_prm")))
  ) %>% 
  ungroup()

# Sex
d0050a$sex <- ifelse(d0050a$sex == 1, 0,
                     ifelse(d0050a$sex == 2, 1, NA)) # 0 = M; 1=F

# SMD
names(d0050a)[names(d0050a) == "goal_prm"] <- "goalregulation"
names(d0050a)[names(d0050a) == "tafo_prm"] <- "taskmanagement"
names(d0050a)[names(d0050a) == "deci_prm"] <- "decisionmakingskill"
names(d0050a)[names(d0050a) == "deta_prm"] <- "detailmanagement"
names(d0050a)[names(d0050a) == "cons_prm"] <- "capacityforconsistency"
names(d0050a)[names(d0050a) == "orga_prm"] <- "organizationalskill"
names(d0050a)[names(d0050a) == "time_prm"] <- "timemanagement"
names(d0050a)[names(d0050a) == "resp_prm"] <- "responsibilitymanagement"
names(d0050a)[names(d0050a) == "rule_prm"] <- "rulefollowingskill"

# IND
names(d0050a)[names(d0050a) == "curi_prm"] <- "abstractthinkingskill"
names(d0050a)[names(d0050a) == "crea_prm"] <- "creativeskill"
names(d0050a)[names(d0050a) == "info_prm"] <- "informationprocessingskill"
names(d0050a)[names(d0050a) == "cult_prm"] <- "culturalcompetence"
names(d0050a)[names(d0050a) == "arti_prm"] <- "artisticskill"

# COD
names(d0050a)[names(d0050a) == "empa_prm"] <- "perspectivetakingskill"
names(d0050a)[names(d0050a) == "soha_prm"] <- "capacityforsocialwarmth"
names(d0050a)[names(d0050a) == "team_prm"] <- "teamworkskill"
names(d0050a)[names(d0050a) == "inte_prm"] <- "ethicalcompetence"
names(d0050a)[names(d0050a) == "trus_prm"] <- "capacityfortrust"

# SED
names(d0050a)[names(d0050a) == "lead_prm"] <- "leadershipskill"
names(d0050a)[names(d0050a) == "expr_prm"] <- "expressiveskill"
names(d0050a)[names(d0050a) == "conv_prm"] <- "conversationalskill"
names(d0050a)[names(d0050a) == "pers_prm"] <- "persuasiveskill"
names(d0050a)[names(d0050a) == "ener_prm"] <- "energyregulation"

# ESD
names(d0050a)[names(d0050a) == "stre_prm"] <- "stressregulation"
names(d0050a)[names(d0050a) == "opti_prm"] <- "capacityforoptimism"
names(d0050a)[names(d0050a) == "scon_prm"] <- "confidenceregulation"
names(d0050a)[names(d0050a) == "impu_prm"] <- "impulseregulation"
names(d0050a)[names(d0050a) == "angm_prm"] <- "angermanagement"

# Interstitial
names(d0050a)[names(d0050a) == "refl_prm"] <- "selfreflectionskill"
names(d0050a)[names(d0050a) == "inde_prm"] <- "capacityforindependence"
names(d0050a)[names(d0050a) == "adap_prm"] <- "adaptability"

# Select and save
d0050a2 <- d0050a
d0050a <- d0050a[, colnames(d0050a) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0050a, use = "pairwise.complete")),"data/3.meta_data/matrices/0050a.xlsx")

# Individual data with age
d0050a2 <- d0050a2[, colnames(d0050a2) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0050a2),"data/3.meta_data/open_data/individual_data/0050a.xlsx")

#### --------------------------------------------------- 0050b --------------------------------------------------- ####
rm(list=ls())
load("data/3.meta_data/open_data/d0050.Rdata")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Skill domains
d0050b <- adults_teens_t0_t1_t2 %>% 
  rowwise() %>% 
  mutate(
    selfmanagement = mean(c_across(c("tafo_prm", "time_prm", "deta_prm", "orga_prm", "resp_prm",
                                      "cons_prm", "goal_prm", "rule_prm", "deci_prm"))),
    socialengagement= mean(c_across(c("lead_prm", "pers_prm", "conv_prm", "expr_prm"))),
    emotionalresilience = mean(c_across(c("stre_prm", "opti_prm", "angm_prm", "scon_prm"))),
    cooperation = mean(c_across(c("team_prm", "trus_prm", "empa_prm", "soha_prm"))),
    innovation = mean(c_across(c("crea_prm", "curi_prm", "arti_prm", "cult_prm")))
  ) %>% 
  ungroup()

# AGE
d0050b$age <- rowMeans(subset(d0050b, select = c(age.x,age.y)),na.rm=TRUE) 

# SEX
d0050b$sex <- rowMeans(subset(d0050b, select = c(sex.x,sex.y)),na.rm=TRUE)  
d0050b$sex <- ifelse(d0050b$sex == 1, 0,
                ifelse(d0050b$sex == 2, 1, NA)) # 0 = M; 1=F

# Intelligence
d0050b$intelligencecrystallized <- scale(d0050b$IQGC)
d0050b$intelligencefluid <- scale(d0050b$IQGF)
d0050b$intelligence <- rowMeans(cbind(scale(d0050b$intelligencecrystallized),
                                  scale(d0050b$intelligencefluid)),
                            na.rm=TRUE)

# SMD
names(d0050b)[names(d0050b) == "goal_prm"] <- "goalregulation"
names(d0050b)[names(d0050b) == "tafo_prm"] <- "taskmanagement"
names(d0050b)[names(d0050b) == "deci_prm"] <- "decisionmakingskill"
names(d0050b)[names(d0050b) == "deta_prm"] <- "detailmanagement"
names(d0050b)[names(d0050b) == "cons_prm"] <- "capacityforconsistency"
names(d0050b)[names(d0050b) == "orga_prm"] <- "organizationalskill"
names(d0050b)[names(d0050b) == "time_prm"] <- "timemanagement"
names(d0050b)[names(d0050b) == "resp_prm"] <- "responsibilitymanagement"
names(d0050b)[names(d0050b) == "rule_prm"] <- "rulefollowingskill"

# IND
names(d0050b)[names(d0050b) == "curi_prm"] <- "abstractthinkingskill"
names(d0050b)[names(d0050b) == "crea_prm"] <- "creativeskill"
names(d0050b)[names(d0050b) == "info_prm"] <- "informationprocessingskill"
names(d0050b)[names(d0050b) == "cult_prm"] <- "culturalcompetence"
names(d0050b)[names(d0050b) == "arti_prm"] <- "artisticskill"

# COD
names(d0050b)[names(d0050b) == "empa_prm"] <- "perspectivetakingskill"
names(d0050b)[names(d0050b) == "soha_prm"] <- "capacityforsocialwarmth"
names(d0050b)[names(d0050b) == "team_prm"] <- "teamworkskill"
names(d0050b)[names(d0050b) == "inte_prm"] <- "ethicalcompetence"
names(d0050b)[names(d0050b) == "trus_prm"] <- "capacityfortrust"

# SED
names(d0050b)[names(d0050b) == "lead_prm"] <- "leadershipskill"
names(d0050b)[names(d0050b) == "expr_prm"] <- "expressiveskill"
names(d0050b)[names(d0050b) == "conv_prm"] <- "conversationalskill"
names(d0050b)[names(d0050b) == "pers_prm"] <- "persuasiveskill"
names(d0050b)[names(d0050b) == "ener_prm"] <- "energyregulation"

# ESD
names(d0050b)[names(d0050b) == "stre_prm"] <- "stressregulation"
names(d0050b)[names(d0050b) == "opti_prm"] <- "capacityforoptimism"
names(d0050b)[names(d0050b) == "scon_prm"] <- "confidenceregulation"
names(d0050b)[names(d0050b) == "impu_prm"] <- "impulseregulation"
names(d0050b)[names(d0050b) == "angm_prm"] <- "angermanagement"

# Interstitial
names(d0050b)[names(d0050b) == "refl_prm"] <- "selfreflectionskill"
names(d0050b)[names(d0050b) == "inde_prm"] <- "capacityforindependence"
names(d0050b)[names(d0050b) == "adap_prm"] <- "adaptability"

# BF
d0050b$b5_emos_prm <- (-1)*d0050b$b5_emos_prm
names(d0050b)[names(d0050b) == "b5_cons_prm"] <- "conscientiousness"
names(d0050b)[names(d0050b) == "b5_open_prm"] <- "openness"
names(d0050b)[names(d0050b) == "b5_agre_prm"] <- "agreeableness"
names(d0050b)[names(d0050b) == "b5_extr_prm"] <- "extraversion"
names(d0050b)[names(d0050b) == "b5_emos_prm"] <- "neuroticism"

# O
names(d0050b)[names(d0050b) == "b5_open_int_prm"] <- "bf_curiosity"
names(d0050b)[names(d0050b) == "b5_open_aes_prm"] <- "bf_aesthetic"
names(d0050b)[names(d0050b) == "b5_open_cre_prm"] <- "bf_creativity"
# C
names(d0050b)[names(d0050b) == "b5_cons_org_prm"] <- "bf_organization"
names(d0050b)[names(d0050b) == "b5_cons_pro_prm"] <- "bf_productiveness"
names(d0050b)[names(d0050b) == "b5_cons_res_prm"] <- "bf_responsibility"
# E
names(d0050b)[names(d0050b) == "b5_extr_soc_prm"] <- "bf_sociability"
names(d0050b)[names(d0050b) == "b5_extr_ass_prm"] <- "bf_assertiveness"
names(d0050b)[names(d0050b) == "b5_extr_ene_prm"] <- "bf_energy"
# A
names(d0050b)[names(d0050b) == "b5_agre_com_prm"] <- "bf_compassion"
names(d0050b)[names(d0050b) == "b5_agre_res_prm"] <- "bf_respectfulness"
names(d0050b)[names(d0050b) == "b5_agre_tru_prm"] <- "bf_trust"
# N
names(d0050b)[names(d0050b) == "b5_emos_anx_prm"] <- "bf_anxiety"
d0050b$bf_anxiety <- (-1)*d0050b$bf_anxiety
names(d0050b)[names(d0050b) == "b5_emos_dep_prm"] <- "bf_depression"
d0050b$bf_depression <- (-1)*d0050b$bf_depression
names(d0050b)[names(d0050b) == "b5_emos_vol_prm"] <- "bf_emotionalvolatility"
d0050b$bf_emotionalvolatility <- (-1)*d0050b$bf_emotionalvolatility

# Select and save
d0050b2 <- d0050b
d0050b <- d0050b[, colnames(d0050b) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0050b, use = "pairwise.complete")),"data/3.meta_data/matrices/0050b.xlsx")

# Individual data with age
d0050b2 <- d0050b2[, colnames(d0050b2) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0050b2),"data/3.meta_data/open_data/individual_data/0050b.xlsx")

#### --------------------------------------------------- 0053c --------------------------------------------------- ####
rm(list=ls())
d0053c <- readxl::read_excel("data/3.meta_data/open_data/d0053c.xlsx")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Transform scores
table(d0053c$Gender)

d0053c$sex <- ifelse(d0053c$Gender == "Female", 1, 0)

# Transform colnames
names(d0053c)[names(d0053c) == "Age"] <- "age"

#Seb
# SMD
names(d0053c)[names(d0053c) == "BESSI_SM_GoalRegulation"] <- "goalregulation"
names(d0053c)[names(d0053c) == "BESSI_SM_TaskManagement"] <- "taskmanagement"
names(d0053c)[names(d0053c) == "BESSI_SM_DecisionMakingSkill"] <- "decisionmakingskill"
names(d0053c)[names(d0053c) == "BESSI_SM_DetailManagement"] <- "detailmanagement"
names(d0053c)[names(d0053c) == "BESSI_SM_CapacityForConsistency"] <- "capacityforconsistency"
names(d0053c)[names(d0053c) == "BESSI_SM_OrganizationalSkill"] <- "organizationalskill"
names(d0053c)[names(d0053c) == "BESSI_SM_TimeManagement"] <- "timemanagement"
names(d0053c)[names(d0053c) == "BESSI_SM_ResponsibilityManagement"] <- "responsibilitymanagement"
names(d0053c)[names(d0053c) == "BESSI_SM_RuleFollowingSkill"] <- "rulefollowingskill"
names(d0053c)[names(d0053c) == "BESSI_SelfManagementSkills"] <- "selfmanagement"

# IND
names(d0053c)[names(d0053c) == "BESSI_IN_AbstractThinkingSkill"] <- "abstractthinkingskill"
names(d0053c)[names(d0053c) == "BESSI_IN_CreativeSkill"] <- "creativeskill"
names(d0053c)[names(d0053c) == "BESSI_INSM_InformationProcessingSkill"] <- "informationprocessingskill"
names(d0053c)[names(d0053c) == "BESSI_IN_CulturalCompetence"] <- "culturalcompetence"
names(d0053c)[names(d0053c) == "BESSI_IN_ArtisticSkill"] <- "artisticskill"
names(d0053c)[names(d0053c) == "BESSI_InnovationSkills"] <- "innovation"

# COD
names(d0053c)[names(d0053c) == "BESSI_CO_PerspectiveTakingSkill"] <- "perspectivetakingskill"
names(d0053c)[names(d0053c) == "BESSI_CO_CapacityForSocialWarmth"] <- "capacityforsocialwarmth"
names(d0053c)[names(d0053c) == "BESSI_CO_TeamworkSkill"] <- "teamworkskill"
names(d0053c)[names(d0053c) == "BESSI_COSM_EthicalCompetence"] <- "ethicalcompetence"
names(d0053c)[names(d0053c) == "BESSI_CO_CapacityForTrust"] <- "capacityfortrust"
names(d0053c)[names(d0053c) == "BESSI_CooperationSkills"] <- "cooperation"

# SED
names(d0053c)[names(d0053c) == "BESSI_SE_LeadershipSkill"] <- "leadershipskill"
names(d0053c)[names(d0053c) == "BESSI_SE_ExpressiveSkill"] <- "expressiveskill"
names(d0053c)[names(d0053c) == "BESSI_SE_ConversationalSkill"] <- "conversationalskill"
names(d0053c)[names(d0053c) == "BESSI_SE_PersuasiveSkill"] <- "persuasiveskill"
names(d0053c)[names(d0053c) == "BESSI_SESM_EnergyRegulation"] <- "energyregulation"
names(d0053c)[names(d0053c) == "BESSI_SocialEngagementSkills"] <- "socialengagement"

# ESD
names(d0053c)[names(d0053c) == "BESSI_ER_StressRegulation"] <- "stressregulation"
names(d0053c)[names(d0053c) == "BESSI_ER_CapacityForOptimism"] <- "capacityforoptimism"
names(d0053c)[names(d0053c) == "BESSI_ER_ConfidenceRegulation"] <- "confidenceregulation"
names(d0053c)[names(d0053c) == "BESSI_ERSM_ImpulseRegulation"] <- "impulseregulation"
names(d0053c)[names(d0053c) == "BESSI_ER_AngerManagement"] <- "angermanagement"
names(d0053c)[names(d0053c) == "BESSI_EmotionalResilienceSkills"] <- "emotionalresilience"

# Interstitial
names(d0053c)[names(d0053c) == "BESSI_XX_SelfReflectionSkill"] <- "selfreflectionskill"
names(d0053c)[names(d0053c) == "BESSI_XX_CapacityForIndependence"] <- "capacityforindependence"
names(d0053c)[names(d0053c) == "BESSI_XX_Adaptability"] <- "adaptability"

# Select and save
d0053c2<-d0053c
d0053c <- d0053c[, colnames(d0053c) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0053c, use = "pairwise.complete")),"data/3.meta_data/matrices/0053c.xlsx")

# Individual data with age
d0053c2 <- d0053c2[, colnames(d0053c2) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0053c2),"data/3.meta_data/open_data/individual_data/0053c.xlsx")

#### --------------------------------------------------- 0053d --------------------------------------------------- ####
rm(list=ls())
d0053d <- readxl::read_excel("data/3.meta_data/open_data/d0053d.xlsx")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Transform scores
d0053d$sex <- ifelse(d0053d$Gender == "Female", 1, 0)

# Transform colnames
names(d0053d)[names(d0053d) == "Age"] <- "age"

#Seb
# SMD
names(d0053d)[names(d0053d) == "BESSI_SM_GoalRegulation"] <- "goalregulation"
names(d0053d)[names(d0053d) == "BESSI_SM_TaskManagement"] <- "taskmanagement"
names(d0053d)[names(d0053d) == "BESSI_SM_DecisionMakingSkill"] <- "decisionmakingskill"
names(d0053d)[names(d0053d) == "BESSI_SM_DetailManagement"] <- "detailmanagement"
names(d0053d)[names(d0053d) == "BESSI_SM_CapacityForConsistency"] <- "capacityforconsistency"
names(d0053d)[names(d0053d) == "BESSI_SM_OrganizationalSkill"] <- "organizationalskill"
names(d0053d)[names(d0053d) == "BESSI_SM_TimeManagement"] <- "timemanagement"
names(d0053d)[names(d0053d) == "BESSI_SM_ResponsibilityManagement"] <- "responsibilitymanagement"
names(d0053d)[names(d0053d) == "BESSI_SM_RuleFollowingSkill"] <- "rulefollowingskill"
names(d0053d)[names(d0053d) == "BESSI_SelfManagementSkills"] <- "selfmanagement"

# IND
names(d0053d)[names(d0053d) == "BESSI_IN_AbstractThinkingSkill"] <- "abstractthinkingskill"
names(d0053d)[names(d0053d) == "BESSI_IN_CreativeSkill"] <- "creativeskill"
names(d0053d)[names(d0053d) == "BESSI_INSM_InformationProcessingSkill"] <- "informationprocessingskill"
names(d0053d)[names(d0053d) == "BESSI_IN_CulturalCompetence"] <- "culturalcompetence"
names(d0053d)[names(d0053d) == "BESSI_IN_ArtisticSkill"] <- "artisticskill"
names(d0053d)[names(d0053d) == "BESSI_InnovationSkills"] <- "innovation"

# COD
names(d0053d)[names(d0053d) == "BESSI_CO_PerspectiveTakingSkill"] <- "perspectivetakingskill"
names(d0053d)[names(d0053d) == "BESSI_CO_CapacityForSocialWarmth"] <- "capacityforsocialwarmth"
names(d0053d)[names(d0053d) == "BESSI_CO_TeamworkSkill"] <- "teamworkskill"
names(d0053d)[names(d0053d) == "BESSI_COSM_EthicalCompetence"] <- "ethicalcompetence"
names(d0053d)[names(d0053d) == "BESSI_CO_CapacityForTrust"] <- "capacityfortrust"
names(d0053d)[names(d0053d) == "BESSI_CooperationSkills"] <- "cooperation"

# SED
names(d0053d)[names(d0053d) == "BESSI_SE_LeadershipSkill"] <- "leadershipskill"
names(d0053d)[names(d0053d) == "BESSI_SE_ExpressiveSkill"] <- "expressiveskill"
names(d0053d)[names(d0053d) == "BESSI_SE_ConversationalSkill"] <- "conversationalskill"
names(d0053d)[names(d0053d) == "BESSI_SE_PersuasiveSkill"] <- "persuasiveskill"
names(d0053d)[names(d0053d) == "BESSI_SESM_EnergyRegulation"] <- "energyregulation"
names(d0053d)[names(d0053d) == "BESSI_SocialEngagementSkills"] <- "socialengagement"

# ESD
names(d0053d)[names(d0053d) == "BESSI_ER_StressRegulation"] <- "stressregulation"
names(d0053d)[names(d0053d) == "BESSI_ER_CapacityForOptimism"] <- "capacityforoptimism"
names(d0053d)[names(d0053d) == "BESSI_ER_ConfidenceRegulation"] <- "confidenceregulation"
names(d0053d)[names(d0053d) == "BESSI_ERSM_ImpulseRegulation"] <- "impulseregulation"
names(d0053d)[names(d0053d) == "BESSI_ER_AngerManagement"] <- "angermanagement"
names(d0053d)[names(d0053d) == "BESSI_EmotionalResilienceSkills"] <- "emotionalresilience"

# Interstitial
names(d0053d)[names(d0053d) == "BESSI_XX_SelfReflectionSkill"] <- "selfreflectionskill"
names(d0053d)[names(d0053d) == "BESSI_XX_CapacityForIndependence"] <- "capacityforindependence"
names(d0053d)[names(d0053d) == "BESSI_XX_Adaptability"] <- "adaptability"

# Select and save
d0053d2<-d0053d
d0053d <- d0053d[, colnames(d0053d) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0053d, use = "pairwise.complete")),"data/3.meta_data/matrices/0053d.xlsx")

# Individual data with age
d0053d2 <- d0053d2[, colnames(d0053d2) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0053d2),"data/3.meta_data/open_data/individual_data/0053d.xlsx")

#### --------------------------------------------------- 0053e --------------------------------------------------- ####
rm(list=ls())
d0053e <- readxl::read_excel("data/3.meta_data/open_data/d0053e.xlsx")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Transform scores
d0053e$sex <- ifelse(d0053e$Gender == 3, NA, d0053e$Gender)
d0053e$sex <- d0053e$sex -1 #males to 0, females to 1

# Transform colnames
names(d0053e)[names(d0053e) == "Age"] <- "age"

#Seb
# SMD
names(d0053e)[names(d0053e) == "BESSI_SM_GoalRegulation"] <- "goalregulation"
names(d0053e)[names(d0053e) == "BESSI_SM_TaskManagement"] <- "taskmanagement"
names(d0053e)[names(d0053e) == "BESSI_SM_DecisionMakingSkill"] <- "decisionmakingskill"
names(d0053e)[names(d0053e) == "BESSI_SM_DetailManagement"] <- "detailmanagement"
names(d0053e)[names(d0053e) == "BESSI_SM_CapacityForConsistency"] <- "capacityforconsistency"
names(d0053e)[names(d0053e) == "BESSI_SM_OrganizationalSkill"] <- "organizationalskill"
names(d0053e)[names(d0053e) == "BESSI_SM_TimeManagement"] <- "timemanagement"
names(d0053e)[names(d0053e) == "BESSI_SM_ResponsibilityManagement"] <- "responsibilitymanagement"
names(d0053e)[names(d0053e) == "BESSI_SM_RuleFollowingSkill"] <- "rulefollowingskill"
names(d0053e)[names(d0053e) == "BESSI_SelfManagementSkills"] <- "selfmanagement"

# IND
names(d0053e)[names(d0053e) == "BESSI_IN_AbstractThinkingSkill"] <- "abstractthinkingskill"
names(d0053e)[names(d0053e) == "BESSI_IN_CreativeSkill"] <- "creativeskill"
names(d0053e)[names(d0053e) == "BESSI_INSM_InformationProcessingSkill"] <- "informationprocessingskill"
names(d0053e)[names(d0053e) == "BESSI_IN_CulturalCompetence"] <- "culturalcompetence"
names(d0053e)[names(d0053e) == "BESSI_IN_ArtisticSkill"] <- "artisticskill"
names(d0053e)[names(d0053e) == "BESSI_InnovationSkills"] <- "innovation"

# COD
names(d0053e)[names(d0053e) == "BESSI_CO_PerspectiveTakingSkill"] <- "perspectivetakingskill"
names(d0053e)[names(d0053e) == "BESSI_CO_CapacityForSocialWarmth"] <- "capacityforsocialwarmth"
names(d0053e)[names(d0053e) == "BESSI_CO_TeamworkSkill"] <- "teamworkskill"
names(d0053e)[names(d0053e) == "BESSI_COSM_EthicalCompetence"] <- "ethicalcompetence"
names(d0053e)[names(d0053e) == "BESSI_CO_CapacityForTrust"] <- "capacityfortrust"
names(d0053e)[names(d0053e) == "BESSI_CooperationSkills"] <- "cooperation"

# SED
names(d0053e)[names(d0053e) == "BESSI_SE_LeadershipSkill"] <- "leadershipskill"
names(d0053e)[names(d0053e) == "BESSI_SE_ExpressiveSkill"] <- "expressiveskill"
names(d0053e)[names(d0053e) == "BESSI_SE_ConversationalSkill"] <- "conversationalskill"
names(d0053e)[names(d0053e) == "BESSI_SE_PersuasiveSkill"] <- "persuasiveskill"
names(d0053e)[names(d0053e) == "BESSI_SESM_EnergyRegulation"] <- "energyregulation"
names(d0053e)[names(d0053e) == "BESSI_SocialEngagementSkills"] <- "socialengagement"

# ESD
names(d0053e)[names(d0053e) == "BESSI_ER_StressRegulation"] <- "stressregulation"
names(d0053e)[names(d0053e) == "BESSI_ER_CapacityForOptimism"] <- "capacityforoptimism"
names(d0053e)[names(d0053e) == "BESSI_ER_ConfidenceRegulation"] <- "confidenceregulation"
names(d0053e)[names(d0053e) == "BESSI_ERSM_ImpulseRegulation"] <- "impulseregulation"
names(d0053e)[names(d0053e) == "BESSI_ER_AngerManagement"] <- "angermanagement"
names(d0053e)[names(d0053e) == "BESSI_EmotionalResilienceSkills"] <- "emotionalresilience"

# Interstitial
names(d0053e)[names(d0053e) == "BESSI_XX_SelfReflectionSkill"] <- "selfreflectionskill"
names(d0053e)[names(d0053e) == "BESSI_XX_CapacityForIndependence"] <- "capacityforindependence"
names(d0053e)[names(d0053e) == "BESSI_XX_Adaptability"] <- "adaptability"

# BIG FIVE
# O
names(d0053e)[names(d0053e) == "BFI2_OpenMindedness"] <- "openness"
names(d0053e)[names(d0053e) == "BFI2_O_IntellectualCuriosity"] <- "bf_curiosity"
names(d0053e)[names(d0053e) == "BFI2_O_AestheticSensitivity"] <- "bf_aesthetic"
names(d0053e)[names(d0053e) == "BFI2_O_CreativeImagination"] <- "bf_creativity"
# C
names(d0053e)[names(d0053e) == "BFI2_Conscientiousness"] <- "conscientiousness"
names(d0053e)[names(d0053e) == "BFI2_C_Organization"] <- "bf_organization"
names(d0053e)[names(d0053e) == "BFI2_C_Productiveness"] <- "bf_productiveness"
names(d0053e)[names(d0053e) == "BFI2_C_Responsibility"] <- "bf_responsibility"
# E
names(d0053e)[names(d0053e) == "BFI2_Extraversion"] <- "extraversion"
names(d0053e)[names(d0053e) == "BFI2_E_Sociability"] <- "bf_sociability"
names(d0053e)[names(d0053e) == "BFI2_E_Assertiveness"] <- "bf_assertiveness"
names(d0053e)[names(d0053e) == "BFI2_E_EnergyLevel"] <- "bf_energy"
# A
names(d0053e)[names(d0053e) == "BFI2_Agreeableness"] <- "agreeableness"
names(d0053e)[names(d0053e) == "BFI2_A_Compassion"] <- "bf_compassion"
names(d0053e)[names(d0053e) == "BFI2_A_Respectfulness"] <- "bf_respectfulness"
names(d0053e)[names(d0053e) == "BFI2_A_Trust"] <- "bf_trust"
# N
names(d0053e)[names(d0053e) == "BFI2_NegativeEmotionality"] <- "neuroticism"
names(d0053e)[names(d0053e) == "BFI2_N_Anxiety"] <- "bf_anxiety"
names(d0053e)[names(d0053e) == "BFI2_N_Depression"] <- "bf_depression"
names(d0053e)[names(d0053e) == "BFI2_N_EmotionalVolatility"] <- "bf_emotionalvolatility"

# CHARACTER STRENGTHS
names(d0053e)[names(d0053e) == "TTC_InterpersonalStrengths"] <- "cs_interpersonal"
names(d0053e)[names(d0053e) == "TTC_IntellectualStrengths"] <- "cs_intellectual"
names(d0053e)[names(d0053e) == "TTC_IntrapersonalStrengths"] <- "cs_intrapersonal"
names(d0053e)[names(d0053e) == "TTC_Inter_InterpersonalControl"] <- "cs_interpersonalcontrol"
names(d0053e)[names(d0053e) == "TTC_Inter_SocialIntelligence"] <- "cs_socialintelligence"
names(d0053e)[names(d0053e) == "TTC_Inter_Gratitude"] <- "cs_gratitude"
names(d0053e)[names(d0053e) == "TTC_Intel_Zest"] <- "cs_zest"
names(d0053e)[names(d0053e) == "TTC_Intel_Curiosity"] <- "cs_curiosity"
names(d0053e)[names(d0053e) == "TTC_Intra_AcademicControl"] <- "cs_academiccontrol"
names(d0053e)[names(d0053e) == "TTC_Intra_Grit"] <- "cs_perseverance"

# POSITIVE YOUTH DEVELOPMENT
names(d0053e)[names(d0053e) == "PYD_Competence"] <- "pyd_competence"
names(d0053e)[names(d0053e) == "PYD_Confidence"] <- "pyd_confidence"
names(d0053e)[names(d0053e) == "PYD_Character"] <- "pyd_character"
names(d0053e)[names(d0053e) == "PYD_Caring"] <- "pyd_caring"
names(d0053e)[names(d0053e) == "PYD_Connection"] <- "pyd_connection"

#Casel
names(d0053e)[names(d0053e) == "SECA_SelfManagement"] <- "secaSelfmanagement"
names(d0053e)[names(d0053e) == "SECA_RelationshipSkills"] <- "secaRelationships"
names(d0053e)[names(d0053e) == "SECA_SocialAwareness"] <- "secaSocialawareness"
names(d0053e)[names(d0053e) == "SECA_SelfAwareness"] <- "secaSelfawareness"
names(d0053e)[names(d0053e) == "SECA_ResponsibleDecisionMaking"] <- "secaDecisionmaking"

#SEL
names(d0053e)[names(d0053e) == "SELS_SelfManagement"] <- "sel_selfmanagement"
names(d0053e)[names(d0053e) == "SELS_GrowthMindset"] <- "sel_mindset"
names(d0053e)[names(d0053e) == "SELS_SelfEfficacy"] <- "sel_selfefficacy"
names(d0053e)[names(d0053e) == "SELS_SocialAwareness"] <- "selsocial"

names(d0053e)
# Select and save
d0053e2<-d0053e
d0053e <- d0053e[, colnames(d0053e) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0053e, use = "pairwise.complete")),"data/3.meta_data/matrices/0053e.xlsx")

# Individual data with age
d0053e2 <- d0053e2[, colnames(d0053e2) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0053e2),"data/3.meta_data/open_data/individual_data/0053e.xlsx")

#### --------------------------------------------------- 0053f --------------------------------------------------- ####
rm(list=ls())
d0053f <- readxl::read_excel("data/3.meta_data/open_data/d0053f.xlsx")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

names(d0053f)

# Transform scores
d0053f$sex <- ifelse(d0053f$Gender == 0, NA, ifelse(d0053f$Gender == -1, 0, d0053f$Gender)) # Males to 0, Females to 1
# Transform colnames
names(d0053f)[names(d0053f) == "Age"] <- "age"

#Seb
# SMD
names(d0053f)[names(d0053f) == "BESSI_SM_GoalRegulation"] <- "goalregulation"
names(d0053f)[names(d0053f) == "BESSI_SM_TaskManagement"] <- "taskmanagement"
names(d0053f)[names(d0053f) == "BESSI_SM_DecisionMakingSkill"] <- "decisionmakingskill"
names(d0053f)[names(d0053f) == "BESSI_SM_DetailManagement"] <- "detailmanagement"
names(d0053f)[names(d0053f) == "BESSI_SM_CapacityForConsistency"] <- "capacityforconsistency"
names(d0053f)[names(d0053f) == "BESSI_SM_OrganizationalSkill"] <- "organizationalskill"
names(d0053f)[names(d0053f) == "BESSI_SM_TimeManagement"] <- "timemanagement"
names(d0053f)[names(d0053f) == "BESSI_SM_ResponsibilityManagement"] <- "responsibilitymanagement"
names(d0053f)[names(d0053f) == "BESSI_SM_RuleFollowingSkill"] <- "rulefollowingskill"
names(d0053f)[names(d0053f) == "BESSI_SelfManagementSkills"] <- "selfmanagement"
# IND
names(d0053f)[names(d0053f) == "BESSI_IN_AbstractThinkingSkill"] <- "abstractthinkingskill"
names(d0053f)[names(d0053f) == "BESSI_IN_CreativeSkill"] <- "creativeskill"
names(d0053f)[names(d0053f) == "BESSI_INSM_InformationProcessingSkill"] <- "informationprocessingskill"
names(d0053f)[names(d0053f) == "BESSI_IN_CulturalCompetence"] <- "culturalcompetence"
names(d0053f)[names(d0053f) == "BESSI_IN_ArtisticSkill"] <- "artisticskill"
names(d0053f)[names(d0053f) == "BESSI_InnovationSkills"] <- "innovation"
# COD
names(d0053f)[names(d0053f) == "BESSI_CO_PerspectiveTakingSkill"] <- "perspectivetakingskill"
names(d0053f)[names(d0053f) == "BESSI_CO_CapacityForSocialWarmth"] <- "capacityforsocialwarmth"
names(d0053f)[names(d0053f) == "BESSI_CO_TeamworkSkill"] <- "teamworkskill"
names(d0053f)[names(d0053f) == "BESSI_COSM_EthicalCompetence"] <- "ethicalcompetence"
names(d0053f)[names(d0053f) == "BESSI_CO_CapacityForTrust"] <- "capacityfortrust"
names(d0053f)[names(d0053f) == "BESSI_CooperationSkills"] <- "cooperation"
# SED
names(d0053f)[names(d0053f) == "BESSI_SE_LeadershipSkill"] <- "leadershipskill"
names(d0053f)[names(d0053f) == "BESSI_SE_ExpressiveSkill"] <- "expressiveskill"
names(d0053f)[names(d0053f) == "BESSI_SE_ConversationalSkill"] <- "conversationalskill"
names(d0053f)[names(d0053f) == "BESSI_SE_PersuasiveSkill"] <- "persuasiveskill"
names(d0053f)[names(d0053f) == "BESSI_SESM_EnergyRegulation"] <- "energyregulation"
names(d0053f)[names(d0053f) == "BESSI_SocialEngagementSkills"] <- "socialengagement"
# ESD
names(d0053f)[names(d0053f) == "BESSI_ER_StressRegulation"] <- "stressregulation"
names(d0053f)[names(d0053f) == "BESSI_ER_CapacityForOptimism"] <- "capacityforoptimism"
names(d0053f)[names(d0053f) == "BESSI_ER_ConfidenceRegulation"] <- "confidenceregulation"
names(d0053f)[names(d0053f) == "BESSI_ERSM_ImpulseRegulation"] <- "impulseregulation"
names(d0053f)[names(d0053f) == "BESSI_ER_AngerManagement"] <- "angermanagement"
names(d0053f)[names(d0053f) == "BESSI_EmotionalResilienceSkills"] <- "emotionalresilience"
# Interstitial
names(d0053f)[names(d0053f) == "BESSI_XX_SelfReflectionSkill"] <- "selfreflectionskill"
names(d0053f)[names(d0053f) == "BESSI_XX_CapacityForIndependence"] <- "capacityforindependence"
names(d0053f)[names(d0053f) == "BESSI_XX_Adaptability"] <- "adaptability"

# School
d0053f$academicachievement <- rowMeans(select(d0053f, GPA_Winter, GPA_Spring), na.rm = TRUE)
names(d0053f)[names(d0053f) == "AE_Overall"] <- "academicengagement"
# Social
names(d0053f)[names(d0053f) == "Volunteerism"] <- "volunteering"
names(d0053f)[names(d0053f) == "PeerAcceptance"] <- "peeracceptance"
names(d0053f)[names(d0053f) == "FQS_Overall"] <- "friendshipquality"
names(d0053f)[names(d0053f) == "RomanticSatisfaction"] <- "romanticrelationship"
names(d0053f)[names(d0053f) == "MotherSatisfaction"] <- "motherrelationship"
names(d0053f)[names(d0053f) == "FatherSatisfaction"] <- "fatherrelationship"
# Physical
names(d0053f)[names(d0053f) == "Exercise"] <- "physicalexercise"
# Mental health
names(d0053f)[names(d0053f) == "LifeSatisfaction"] <- "satisfactionwithlife"
#Creative
names(d0053f)[names(d0053f) == "MIP_Realistic"] <- "realisticinterests"
names(d0053f)[names(d0053f) == "MIP_Investigative"] <- "investigativeinterests"
names(d0053f)[names(d0053f) == "MIP_Artistic"] <- "artisticinterests"
names(d0053f)[names(d0053f) == "MIP_Social"] <- "socialinterests"
names(d0053f)[names(d0053f) == "MIP_Enterprising"] <- "enterprisinginterests"
names(d0053f)[names(d0053f) == "MIP_Conventional"] <- "conventionalinterests"


# Select and save
d0053f2<-d0053f
d0053f <- d0053f[, colnames(d0053f) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0053f, use = "pairwise.complete")),"data/3.meta_data/matrices/0053f.xlsx")

# Individual data with age
d0053f2 <- d0053f2[, colnames(d0053f2) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0053f2),"data/3.meta_data/open_data/individual_data/0053f.xlsx")

#### --------------------------------------------------- 0053g --------------------------------------------------- ####
rm(list=ls())
d0053g <- readxl::read_excel("data/3.meta_data/open_data/d0053g.xlsx")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

# Transform scores
table(d0053g$TargetGender)

d0053g$sex <- ifelse(d0053g$TargetGender == 2, 0, d0053g$TargetGender) # Male = 0, Female = 1

# Transform colnames
names(d0053g)[names(d0053g) == "TargetAge"] <- "age"

#Seb
# SMD
names(d0053g)[names(d0053g) == "BESSI_SM_GoalRegulation"] <- "goalregulation"
names(d0053g)[names(d0053g) == "BESSI_SM_TaskManagement"] <- "taskmanagement"
names(d0053g)[names(d0053g) == "BESSI_SM_DecisionMakingSkill"] <- "decisionmakingskill"
names(d0053g)[names(d0053g) == "BESSI_SM_DetailManagement"] <- "detailmanagement"
names(d0053g)[names(d0053g) == "BESSI_SM_CapacityForConsistency"] <- "capacityforconsistency"
names(d0053g)[names(d0053g) == "BESSI_SM_OrganizationalSkill"] <- "organizationalskill"
names(d0053g)[names(d0053g) == "BESSI_SM_TimeManagement"] <- "timemanagement"
names(d0053g)[names(d0053g) == "BESSI_SM_ResponsibilityManagement"] <- "responsibilitymanagement"
names(d0053g)[names(d0053g) == "BESSI_SM_RuleFollowingSkill"] <- "rulefollowingskill"
names(d0053g)[names(d0053g) == "BESSI_SelfManagementSkills"] <- "selfmanagement"

# IND
names(d0053g)[names(d0053g) == "BESSI_IN_AbstractThinkingSkill"] <- "abstractthinkingskill"
names(d0053g)[names(d0053g) == "BESSI_IN_CreativeSkill"] <- "creativeskill"
names(d0053g)[names(d0053g) == "BESSI_INSM_InformationProcessingSkill"] <- "informationprocessingskill"
names(d0053g)[names(d0053g) == "BESSI_IN_CulturalCompetence"] <- "culturalcompetence"
names(d0053g)[names(d0053g) == "BESSI_IN_ArtisticSkill"] <- "artisticskill"
names(d0053g)[names(d0053g) == "BESSI_InnovationSkills"] <- "innovation"

# COD
names(d0053g)[names(d0053g) == "BESSI_CO_PerspectiveTakingSkill"] <- "perspectivetakingskill"
names(d0053g)[names(d0053g) == "BESSI_CO_CapacityForSocialWarmth"] <- "capacityforsocialwarmth"
names(d0053g)[names(d0053g) == "BESSI_CO_TeamworkSkill"] <- "teamworkskill"
names(d0053g)[names(d0053g) == "BESSI_COSM_EthicalCompetence"] <- "ethicalcompetence"
names(d0053g)[names(d0053g) == "BESSI_CO_CapacityForTrust"] <- "capacityfortrust"
names(d0053g)[names(d0053g) == "BESSI_CooperationSkills"] <- "cooperation"

# SED
names(d0053g)[names(d0053g) == "BESSI_SE_LeadershipSkill"] <- "leadershipskill"
names(d0053g)[names(d0053g) == "BESSI_SE_ExpressiveSkill"] <- "expressiveskill"
names(d0053g)[names(d0053g) == "BESSI_SE_ConversationalSkill"] <- "conversationalskill"
names(d0053g)[names(d0053g) == "BESSI_SE_PersuasiveSkill"] <- "persuasiveskill"
names(d0053g)[names(d0053g) == "BESSI_SESM_EnergyRegulation"] <- "energyregulation"
names(d0053g)[names(d0053g) == "BESSI_SocialEngagementSkills"] <- "socialengagement"

# ESD
names(d0053g)[names(d0053g) == "BESSI_ER_StressRegulation"] <- "stressregulation"
names(d0053g)[names(d0053g) == "BESSI_ER_CapacityForOptimism"] <- "capacityforoptimism"
names(d0053g)[names(d0053g) == "BESSI_ER_ConfidenceRegulation"] <- "confidenceregulation"
names(d0053g)[names(d0053g) == "BESSI_ERSM_ImpulseRegulation"] <- "impulseregulation"
names(d0053g)[names(d0053g) == "BESSI_ER_AngerManagement"] <- "angermanagement"
names(d0053g)[names(d0053g) == "BESSI_EmotionalResilienceSkills"] <- "emotionalresilience"

# Interstitial
names(d0053g)[names(d0053g) == "BESSI_XX_SelfReflectionSkill"] <- "selfreflectionskill"
names(d0053g)[names(d0053g) == "BESSI_XX_CapacityForIndependence"] <- "capacityforindependence"
names(d0053g)[names(d0053g) == "BESSI_XX_Adaptability"] <- "adaptability"

# Select and save
d0053g2<-d0053g
d0053g <- d0053g[, colnames(d0053g) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0053g, use = "pairwise.complete")),"data/3.meta_data/matrices/0053g.xlsx")

# Individual data with age
d0053g2 <- d0053g2[, colnames(d0053g2) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0053g2),"data/3.meta_data/open_data/individual_data/0053g.xlsx")

#### --------------------------------------------------- 0068 --------------------------------------------------- ####
rm(list=ls())
d0068 <- readxl::read_excel("data/3.meta_data/open_data/d0068.xlsx")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

names(d0068)

# Transform scores
table(d0068$Gender)

d0068$sex <- ifelse(d0068$Gender == 1, 0, 
                    ifelse(d0068$Gender == 2, 1, NA)) # Males to 0, Females to 1
d0068$academicachievement 
# Transform colnames
names(d0068)[names(d0068) == "Age"] <- "age"

#Seb
# SMD
names(d0068)[names(d0068) == "SMD"] <- "selfmanagement"
# IND
names(d0068)[names(d0068) == "IND"] <- "innovation"
# COD
names(d0068)[names(d0068) == "COD"] <- "cooperation"
# SED
names(d0068)[names(d0068) == "SED"] <- "socialengagement"
# ESD
names(d0068)[names(d0068) == "ERD"] <- "emotionalresilience"

# School
names(d0068)[names(d0068) == "achievement"] <- "academicachievement"
names(d0068)[names(d0068) == "Efficacy"] <- "academicselfefficacy"

# Job
names(d0068)[names(d0068) == "adapt"] <- "careeradaptability"

# Select and save
d0068a <- d0068[, colnames(d0068) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0068a, use = "pairwise.complete")),"data/3.meta_data/matrices/0068a.xlsx")

# Individual data with age
d0068 <- d0068[, colnames(d0068) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0068),"data/3.meta_data/open_data/individual_data/0068a.xlsx")

#### --------------------------------------------------- 0070 --------------------------------------------------- ####
rm(list=ls())
d0070 <- readxl::read_excel("data/3.meta_data/open_data/d0070.xlsx")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

names(d0070)

# Transform scores
d0070$sex <- ifelse(d0070$sex == 1, 0, 
                    ifelse(d0070$sex == 2, 1, NA)) # Males to 0, Females to 1

# Environmental scores
d0070$natureconnectedness <- rowMeans(subset(d0070, select = c("CNS_1", "CNS_2", "CNS_3",
                                           "CNS_4", "CNS_5", "CNS_6", 
                                           "CNS_7", "CNS_8", "CNS_9",
                                           "CNS_10", "CNS_11", "CNS_13")))
d0070$sustainabilitycompetence <- rowMeans(d0070[, which(colnames(d0070)=="SPACS_1"):
                            which(colnames(d0070)=="SPACS_12")])

# Five BESSI skill domains 
d0070$selfmanagement<-rowMeans(d0070[,c("bessi_1",  "bessi_6", "bessi_11", "bessi_16",
                     "bessi_21", "bessi_26", "bessi_31", "bessi_36",
                     "bessi_41")], na.rm=TRUE) #Self Management skill

d0070$innovation<-rowMeans(d0070[,c("bessi_5", "bessi_10", "bessi_15", "bessi_20",
                     "bessi_25", "bessi_30", "bessi_35", "bessi_40",
                     "bessi_45")], na.rm=TRUE) #Innovation skill

d0070$cooperation<-rowMeans(d0070[,c("bessi_3", "bessi_8", "bessi_13", "bessi_18",
                     "bessi_23", "bessi_28", "bessi_33", "bessi_38",
                     "bessi_43")], na.rm=TRUE) #Cooperation skill

d0070$socialengagement<-rowMeans(d0070[,c("bessi_2", "bessi_7", "bessi_12", "bessi_17",
                     "bessi_22", "bessi_27", "bessi_32", "bessi_37",
                     "bessi_42")], na.rm=TRUE) #Social Engagement skill

d0070$emotionalresilience<-rowMeans(d0070[,c("bessi_4", "bessi_9", "bessi_14", "bessi_19",
                     "bessi_24", "bessi_29", "bessi_34", "bessi_39",
                     "bessi_44")], na.rm=TRUE) #Emotional Resilience skill


# Select and save
d0070a <- d0070[, colnames(d0070) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0070a, use = "pairwise.complete")),"data/3.meta_data/matrices/0070a.xlsx")

# Individual data with age
d0070 <- d0070[, colnames(d0070) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0070),"data/3.meta_data/open_data/individual_data/0070a.xlsx")

#### --------------------------------------------------- 0071 --------------------------------------------------- ####
rm(list=ls())
d0071 <- readxl::read_excel("data/3.meta_data/open_data/d0071.xlsx")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

names(d0071)

# Transform scores
d0071$sex <- ifelse(d0071$Gender == "Male", 0, 
                    ifelse(d0071$Gender == "Female", 1, NA)) # Males to 0, Females to 1
d0071$age <- d0071$Age

# Five BESSI skill domains 
d0071$selfmanagement<-rowMeans(d0071[,c("bessi_1",  "bessi_6", "bessi_11", "bessi_16",
                                        "bessi_21", "bessi_26", "bessi_31", "bessi_36",
                                        "bessi_41")], na.rm=TRUE) #Self Management skill

d0071$innovation<-rowMeans(d0071[,c("bessi_5", "bessi_10", "bessi_15", "bessi_20",
                                    "bessi_25", "bessi_30", "bessi_35", "bessi_40",
                                    "bessi_45")], na.rm=TRUE) #Innovation skill

d0071$cooperation<-rowMeans(d0071[,c("bessi_3", "bessi_8", "bessi_13", "bessi_18",
                                     "bessi_23", "bessi_28", "bessi_33", "bessi_38",
                                     "bessi_43")], na.rm=TRUE) #Cooperation skill

d0071$socialengagement<-rowMeans(d0071[,c("bessi_2", "bessi_7", "bessi_12", "bessi_17",
                                          "bessi_22", "bessi_27", "bessi_32", "bessi_37",
                                          "bessi_42")], na.rm=TRUE) #Social Engagement skill

d0071$emotionalresilience<-rowMeans(d0071[,c("bessi_4", "bessi_9", "bessi_14", "bessi_19",
                                             "bessi_24", "bessi_29", "bessi_34", "bessi_39",
                                             "bessi_44")], na.rm=TRUE) #Emotional Resilience skill


# School factors
d0071$srlstrategies <- rowMeans(d0071[,c("qas_1","qas_10","qas_12","qas_15","qas_2",
                                             "qas_3","qas_7","qas_13","qas_4","qas_8",
                                             "qas_9","qas_14","qas_5","qas_6","qas_11","qas_16")])
d0071$masterygoals <- rowMeans(subset(d0071, select = c(qc_5,qc_6,qc_7,qc_8 )))
d0071$academicselfefficacy <- rowMeans(subset(d0071, select = c( qc_9,qc_10,qc_11,qc_12)))
d0071$positiveachievementemotions <- rowMeans(subset(d0071, select = c(qe_2,qe_4,qe_6,qe_8,qe_10,qe_12,qe_14)))
d0071$negativeachievementemotions <- rowMeans(subset(d0071, select = c(qe_1,qe_3,qe_5,qe_7,qe_9,qe_11,qe_13)))
d0071$academicachievement <- d0071$media

# Broad factors
d0071$growthmindset <- rowMeans(subset(d0071, select = c(qc_1,qc_2,qc_3,qc_4)))

d0071$satisfactionwithlife <- rowMeans(subset(d0071, select = c(swls_1,swls_2,swls_3,swls_4,swls_5)))
d0071$peeracceptance <- rowMeans(subset(d0071, select = c(acc_1,acc_2,acc_3,acc_4,acc_5,
                                                            acc_6,acc_7,acc_8,acc_9,acc_10,
                                                            acc_11,acc_12)))

# Select and save
d0071a <- d0071[, colnames(d0071) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0071a, use = "pairwise.complete")),"data/3.meta_data/matrices/0071a.xlsx")

# Individual data with age
d0071 <- d0071[, colnames(d0071) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0071),"data/3.meta_data/open_data/individual_data/0071a.xlsx")


#### --------------------------------------------------- 0080 --------------------------------------------------- ####
rm(list=ls())
d0080 <- readxl::read_excel("data/3.meta_data/open_data/d0080.xlsx")
admcol <- readxl::read_excel("data/matrix_codebook.xlsx")

names(d0080)

# Transform scores
d0080$sex <- ifelse(d0080$Sex == "1", 0, 
                    ifelse(d0080$Sex == "0", 1, NA)) # Males to 0, Females to 1

names(d0080)[names(d0080) == "Age"] <- "age"

# Five BESSI skill domains 
names(d0080)[names(d0080) == "BESSI_SM"] <- "selfmanagement"

names(d0080)[names(d0080) == "BESSI_IN"] <- "innovation"

names(d0080)[names(d0080) == "BESSI_CO"] <- "cooperation"

names(d0080)[names(d0080) == "BESSI_SE"] <- "socialengagement"

names(d0080)[names(d0080) == "BESSI_ER"] <- "emotionalresilience"

# School factors
names(d0080)[names(d0080) == "SRL_TOT"] <- "srlstrategies"

names(d0080)[names(d0080) == "ASE_TOT"] <- "academicselfefficacy"

names(d0080)[names(d0080) == "Standard_Score"] <- "academicachievement"

names(d0080)[names(d0080) == "AS_TOT"] <- "academicsatisfaction"

# Select and save
d0080a <- d0080[, colnames(d0080) %in% admcol$column_name]
writexl::write_xlsx(data.frame(cor(d0080a, use = "pairwise.complete")),"data/3.meta_data/matrices/0080a.xlsx")

# Individual data with age
d0080 <- d0080[, colnames(d0080) %in% c(admcol$column_name,"age")]
writexl::write_xlsx(data.frame(d0080),"data/3.meta_data/open_data/individual_data/0080a.xlsx")
