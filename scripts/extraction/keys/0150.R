# Standard BESSI-192: six items per facet, repeated in blocks of 32.
# These keys reproduce all 32 released facet scores exactly. No item reversals.
facet_start <- c(
  timemanagement=3, organizationalskill=6, capacityforconsistency=9,
  taskmanagement=12, detailmanagement=15, rulefollowingskill=18,
  responsibilitymanagement=21, goalregulation=24, decisionmakingskill=27,
  leadershipskill=1, persuasiveskill=13, expressiveskill=17,
  conversationalskill=25, energyregulation=7,
  perspectivetakingskill=2, capacityfortrust=8, capacityforsocialwarmth=14,
  teamworkskill=23, ethicalcompetence=29,
  stressregulation=5, capacityforoptimism=11, angermanagement=20,
  confidenceregulation=26, impulseregulation=30,
  abstractthinkingskill=4, creativeskill=16, artisticskill=28,
  culturalcompetence=32, informationprocessingskill=22,
  selfreflectionskill=10, adaptability=19, capacityforindependence=31)

source_facet_names <- c(
  "BESSI_SM_TimeManagement", "BESSI_SM_OrganizationalSkill", "BESSI_SM_CapacityForConsistency",
  "BESSI_SM_TaskManagement", "BESSI_SM_DetailManagement", "BESSI_SM_RuleFollowingSkill",
  "BESSI_SM_ResponsibilityManagement", "BESSI_SM_GoalRegulation", "BESSI_SM_DecisionMakingSkill",
  "BESSI_SE_LeadershipSkill", "BESSI_SE_PersuasiveSkill", "BESSI_SE_ExpressiveSkill",
  "BESSI_SE_ConversationalSkill", "BESSI_SESM_EnergyRegulation",
  "BESSI_CO_PerspectiveTakingSkill", "BESSI_CO_CapacityForTrust", "BESSI_CO_CapacityForSocialWarmth",
  "BESSI_CO_TeamworkSkill", "BESSI_COSM_EthicalCompetence",
  "BESSI_ER_StressRegulation", "BESSI_ER_CapacityForOptimism", "BESSI_ER_AngerManagement",
  "BESSI_ER_ConfidenceRegulation", "BESSI_ERSM_ImpulseRegulation",
  "BESSI_IN_AbstractThinkingSkill", "BESSI_IN_CreativeSkill", "BESSI_IN_ArtisticSkill",
  "BESSI_IN_CulturalCompetence", "BESSI_INSM_InformationProcessingSkill",
  "BESSI_XX_SelfReflectionSkill", "BESSI_XX_Adaptability", "BESSI_XX_CapacityForIndependence")

source_domain_names <- c(selfmanagement="BESSI_SelfManagementSkills",
  socialengagement="BESSI_SocialEngagementSkills", cooperation="BESSI_CooperationSkills",
  emotionalresilience="BESSI_EmotionalResilienceSkills", innovation="BESSI_InnovationSkills")

# Half weights retain the standard cross-loading facets; normalise by total weight.
domain_weights <- list(
  selfmanagement=c(timemanagement=1, organizationalskill=1, capacityforconsistency=1,
    taskmanagement=1, detailmanagement=1, rulefollowingskill=1, responsibilitymanagement=1,
    goalregulation=1, decisionmakingskill=1, energyregulation=.5, ethicalcompetence=.5,
    impulseregulation=.5, informationprocessingskill=.5),
  socialengagement=c(leadershipskill=1, persuasiveskill=1, expressiveskill=1,
    conversationalskill=1, energyregulation=.5),
  cooperation=c(perspectivetakingskill=1, capacityfortrust=1, capacityforsocialwarmth=1,
    teamworkskill=1, ethicalcompetence=.5),
  emotionalresilience=c(stressregulation=1, capacityforoptimism=1, angermanagement=1,
    confidenceregulation=1, impulseregulation=.5),
  innovation=c(abstractthinkingskill=1, creativeskill=1, artisticskill=1,
    culturalcompetence=1, informationprocessingskill=.5))
matrix_variables <- c(names(domain_weights), names(facet_start), "sex")
