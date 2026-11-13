# BESSI v1.0 scoring key, checked 2026-11-13 against the instrument authors:
# https://www.canpersonalitychange.com/uploads/1/3/1/0/131015187/bessi_scoring_1.pdf
# Page 1: six items per facet, 32 positions apart. Page 2: domain weights.
# Constants only; the article scripts specify scoring and missing-data rules.
bessi192_first_item <- c(
  leadershipskill=1, perspectivetakingskill=2, timemanagement=3,
  abstractthinkingskill=4, stressregulation=5, organizationalskill=6,
  energyregulation=7, capacityfortrust=8, capacityforconsistency=9,
  selfreflectionskill=10, capacityforoptimism=11, taskmanagement=12,
  persuasiveskill=13, capacityforsocialwarmth=14, detailmanagement=15,
  creativeskill=16, expressiveskill=17, rulefollowingskill=18, adaptability=19,
  angermanagement=20, responsibilitymanagement=21, informationprocessingskill=22,
  teamworkskill=23, goalregulation=24, conversationalskill=25,
  confidenceregulation=26, decisionmakingskill=27, artisticskill=28,
  ethicalcompetence=29, impulseregulation=30, capacityforindependence=31,
  culturalcompetence=32)

bessi192_domain_weights <- list(
  selfmanagement=c(timemanagement=1, organizationalskill=1, capacityforconsistency=1,
    taskmanagement=1, detailmanagement=1, rulefollowingskill=1, responsibilitymanagement=1,
    goalregulation=1, decisionmakingskill=1, energyregulation=.5,
    ethicalcompetence=.5, impulseregulation=.5, informationprocessingskill=.5),
  socialengagement=c(leadershipskill=1, persuasiveskill=1, expressiveskill=1,
    conversationalskill=1, energyregulation=.5),
  cooperation=c(perspectivetakingskill=1, capacityfortrust=1,
    capacityforsocialwarmth=1, teamworkskill=1, ethicalcompetence=.5),
  emotionalresilience=c(stressregulation=1, capacityforoptimism=1,
    angermanagement=1, confidenceregulation=1, impulseregulation=.5),
  innovation=c(abstractthinkingskill=1, creativeskill=1, artisticskill=1,
    culturalcompetence=1, informationprocessingskill=.5))
