# 0145 Study 2: named item/answer keys and output lists. No processing logic.
# Sources: OSF an64w Supplement_S2.2.qmd; main scoring is in articles/0145.R.

hexaco_reversed <- c("PH02_06", "PH03_04", "PH02_12", "PH02_36", "PH03_34", "PH02_42", "PH03_16", "PH03_40", "PH03_22", "PH03_46",
  "PH02_29", "PH03_27", "PH02_35", "PH03_09", "PH02_41", "PH03_39", "PH03_45", "PH03_02", "PH03_26", "PH02_10", "PH03_32", "PH02_16", "PH03_20", "PH03_44",
  "PH03_01", "PH03_25", "PH02_09", "PH02_15", "PH03_13", "PH03_37", "PH02_21", "PH03_43", "PH02_50", "PH03_24", "PH03_06", "PH03_30", "PH02_38", "PH02_20", "PH02_44", "PH03_42",
  "PH02_01", "PH02_25", "PH03_05", "PH03_29", "PH02_13", "PH03_35", "PH02_19", "PH03_41")

hexaco_facets <- list(
  sincerity = c("PH02_06_inv", "PH02_30", "PH03_04_inv", "PH03_28"),
  fairness = c("PH02_12_inv", "PH02_36_inv", "PH03_10", "PH03_34_inv"),
  greedavoidance = c("PH02_18", "PH02_42_inv", "PH03_16_inv", "PH03_40_inv"),
  modesty = c("PH02_24", "PH02_48", "PH03_22_inv", "PH03_46_inv"),
  fearfulness = c("PH02_05", "PH02_29_inv", "PH03_03", "PH03_27_inv"),
  anxiety = c("PH02_11", "PH02_35_inv", "PH03_09_inv", "PH03_33"),
  dependence = c("PH02_17", "PH02_41_inv", "PH03_15", "PH03_39_inv"),
  sentimentality = c("PH02_23", "PH02_47", "PH03_21", "PH03_45_inv"),
  socialselfesteem = c("PH02_04", "PH02_28", "PH03_02_inv", "PH03_26_inv"),
  socialboldness = c("PH02_10_inv", "PH02_34", "PH03_08", "PH03_32_inv"),
  sociability = c("PH02_16_inv", "PH02_40", "PH03_14", "PH03_38"),
  liveliness = c("PH02_22", "PH02_46", "PH03_20_inv", "PH03_44_inv"),
  forgiveness = c("PH02_03", "PH02_27", "PH03_01_inv", "PH03_25_inv"),
  gentleness = c("PH02_09_inv", "PH02_33", "PH03_07", "PH03_31"),
  flexibility = c("PH02_15_inv", "PH02_39", "PH03_13_inv", "PH03_37_inv"),
  patience = c("PH02_21_inv", "PH02_45", "PH03_19", "PH03_43_inv"),
  organisation = c("PH02_02", "PH02_26", "PH02_50_inv", "PH03_24_inv"),
  diligence = c("PH02_08", "PH02_32", "PH03_06_inv", "PH03_30_inv"),
  perfectionism = c("PH02_14", "PH02_38_inv", "PH03_12", "PH03_36"),
  prudence = c("PH02_20_inv", "PH02_44_inv", "PH03_18", "PH03_42_inv"),
  aestheticappreciation = c("PH02_01_inv", "PH02_25_inv", "PH02_49", "PH03_23"),
  inquisitiveness = c("PH02_07", "PH02_31", "PH03_05_inv", "PH03_29_inv"),
  creativity = c("PH02_13_inv", "PH02_37", "PH03_11", "PH03_35_inv"),
  unconventionality = c("PH02_19_inv", "PH02_43", "PH03_17", "PH03_41_inv"))

reasoning_key <- list(
  IA16="lullaby", IA29="spring", IA30="length", IA38="accurate", IA39="meal", IA40="inhabited", IA41="evergreen",
  E305="4", E323="10", E333="21", E405="18", E409="26", IF05="A", IF04="A", IF10="A", IF11="A",
  IL05="R", IL07="X", IL11="F", IL09="N", IM04="MR1_4.png", IM06="MR3_2.png", IM10="MR6_4.png", IM13="MR10_4.png",
  IN07_01="51", IN08_01="55", IN11_01="76", IN14_01="39", IN14_02="30",
  IP08="The drill is lowering.", IP06="When the drill is lowering, the oil will drip.", IP11="The yellow signal is not flashing.", IP13="The conveyor belt is running.",
  IS04="ADAFI_Item13_A2.png", IS05="ADAFI_Item20_A3.png", IS06="ADAFI_Item28_A3.png", IS07="ADAFI_Item31_A1.png", IS08="ADAFI_Item33_A1.png",
  IV03="Phillip is taller than Isaac", IV04="47", IV10="Thursday", IV07="It's impossible to tell")

reasoning_subtests <- list(
  ia=c("IA16","IA29","IA30","IA38","IA39","IA40","IA41"), ie=c("E305","E323","E333","E405","E409"),
  ifig=c("IF05","IF04","IF10","IF11"), il=c("IL05","IL07","IL11","IL09"), im=c("IM04","IM06","IM10","IM13"),
  inum=c("IN07_01","IN08_01","IN11_01","IN14"), ip=c("IP08","IP06","IP11","IP13"),
  iseq=c("IS04","IS05","IS06","IS07","IS08"), iv=c("IV03","IV04","IV10","IV07"))

tailor_key <- list(TK04=c(2,2,2,3,2,2,2,2,3,3), TK05=c(1,1,2,1,1,2,1,1,1,1),
  TK06=c(3,3,2,2,3,3,3,2,2,2), TK07=c(1,2,3,1,2,1,1,1,1,1),
  TK09=c(0,0,1,0,0,0,0,0,0,0,0), TK10=c(1,1,0,1,1,0,1,0,1,1,0),
  TK11=c(0,0,1,1,0,1,0,0,0,0,0,0,0), TK12=c(0,0,1,1,0,1,0,0,0,0,0,0,0))

bessi_variables <- c("selfmanagement", "socialengagement", "cooperation", "emotionalresilience", "innovation",
  "timemanagement", "organizationalskill", "capacityforconsistency", "taskmanagement", "detailmanagement", "rulefollowingskill", "responsibilitymanagement", "goalregulation", "decisionmakingskill",
  "energyregulation", "ethicalcompetence", "impulseregulation", "informationprocessingskill", "leadershipskill", "persuasiveskill", "expressiveskill", "conversationalskill", "perspectivetakingskill",
  "capacityfortrust", "capacityforsocialwarmth", "teamworkskill", "stressregulation", "capacityforoptimism", "angermanagement", "confidenceregulation", "abstractthinkingskill", "creativeskill", "artisticskill", "culturalcompetence", "selfreflectionskill", "adaptability", "capacityforindependence")
matrix_variables <- c(bessi_variables, paste0("hexaco_", c("honestyhumility", "emotionality", "extraversion", "agreeableness", "conscientiousness", "openness")),
  "intelligencefluid", "complexproblemsolvingknowledge", "complexproblemsolvingperformance", "socialdominanceorientation", "rightwingauthoritarianism", "conservatism", "economicconservatism", "socialconservatism", "politicalorientation",
  "proenvironmentalattitudes", "environmentalmotivation", "natureconnectedness", "proenvironmentalbehavior", "proenvironmentalbehaviorgoods", "proenvironmentalbehaviortransport", "proenvironmentalbehaviorengagement", "proenvironmentalbehaviorfood", "sex")
