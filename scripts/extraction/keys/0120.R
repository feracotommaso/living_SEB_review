# 0120: scoring and publication constants, without processing logic.
# Official BFI-2 key, checked online 2026-11-13 (second page):
# https://www.colby.edu/wp-content/uploads/2013/08/bfi2-form.pdf
# Negative numbers denote reverse-keyed items on the 1-5 response scale.
# The article script retains the explicit scoring steps.
bfi2_facet_items <- list(
  bf_sociability=c(1,-16,-31,46),
  bf_assertiveness=c(6,21,-36,-51),
  bf_energy=c(-11,-26,41,56),
  bf_compassion=c(2,-17,32,-47),
  bf_respectfulness=c(7,-22,-37,52),
  bf_trust=c(-12,27,-42,57),
  bf_organization=c(-3,18,33,-48),
  bf_productiveness=c(-8,-23,38,53),
  bf_responsibility=c(13,-28,43,-58),
  bf_anxiety=c(-4,19,34,-49),
  bf_depression=c(-9,-24,39,54),
  bf_emotionalvolatility=c(14,-29,-44,59),
  bf_curiosity=c(10,-25,40,-55),
  bf_aesthetic=c(-5,20,35,-50),
  bf_creativity=c(15,-30,-45,60))
bfi2_domain_facets <- list(
  extraversion=c("bf_sociability","bf_assertiveness","bf_energy"),
  agreeableness=c("bf_compassion","bf_respectfulness","bf_trust"),
  conscientiousness=c("bf_organization","bf_productiveness","bf_responsibility"),
  neuroticism=c("bf_anxiety","bf_depression","bf_emotionalvolatility"),
  openness=c("bf_curiosity","bf_aesthetic","bf_creativity"))

# Publication reference, independently read from the rendered 0120 PDF:
# Table 8, p.22, domain rows and seven correlates; Table 9, p.23, all 480
# facet correlations. Ignore significance stars; allow two-decimal rounding.
# These are published reference values, not individual observations.
published_table8 <- rbind(
  innovation=c(.78,.19,.31,.18,-.25,.18,.19),
  selfmanagement=c(.21,.85,.37,.24,-.51,.51,.52),
  socialengagement=c(.36,.42,.86,.33,-.49,.46,.49),
  cooperation=c(.23,.37,.50,.78,-.51,.42,.42),
  emotionalresilience=c(.18,.46,.33,.37,-.89,.54,.61))
colnames(published_table8) <- c("openness","conscientiousness","extraversion",
  "agreeableness","neuroticism","satisfactionwithlife","selfesteem")
published_table9 <- rbind(
  abstractthinkingskill=c(0.85,0.47,0.51,0.01,0.19,0.14,0.05,0.28,0.18,0.09,-0.02,0.07,-0.09,-0.04,-0.20),
  creativeskill=c(0.47,0.29,0.80,0.01,0.16,0.10,0.16,0.39,0.24,0.12,-0.04,0.08,-0.20,-0.17,-0.14),
  artisticskill=c(0.49,0.67,0.64,0.07,0.16,0.08,0.04,0.19,0.24,0.04,-0.09,0.12,-0.28,-0.10,-0.22),
  culturalcompetence=c(0.48,0.28,0.11,-0.07,0.04,0.08,0.10,0.17,0.11,0.30,0.16,0.22,0.04,-0.02,-0.08),
  informationprocessingskill=c(0.49,0.12,0.46,0.21,0.42,0.35,0.19,0.42,0.41,0.27,0.10,0.21,-0.27,-0.35,-0.29),
  timemanagement=c(0.15,0.10,0.19,0.58,0.71,0.58,0.07,0.34,0.34,0.11,0.21,-0.05,-0.16,-0.34,-0.31),
  organizationalskill=c(0.04,0.11,0.09,0.91,0.53,0.49,0.04,0.06,0.29,0.20,0.18,0.05,-0.23,-0.36,-0.31),
  capacityforconsistency=c(0.05,0.07,0.10,0.46,0.61,0.52,0.04,0.24,0.32,0.09,0.18,-0.07,-0.07,-0.30,-0.22),
  taskmanagement=c(0.15,-0.02,0.11,0.51,0.82,0.57,0.22,0.33,0.51,0.19,0.26,-0.05,-0.26,-0.51,-0.35),
  detailmanagement=c(0.29,0.15,0.20,0.42,0.43,0.58,0.07,0.28,0.29,0.29,0.24,-0.02,-0.07,-0.30,-0.30),
  rulefollowingskill=c(-0.10,-0.11,-0.08,0.55,0.50,0.67,0.05,0.11,0.22,0.26,0.50,-0.05,-0.12,-0.42,-0.30),
  responsibilitymanagement=c(0.12,0.09,0.14,0.44,0.69,0.71,0.13,0.38,0.45,0.40,0.33,0.06,-0.13,-0.45,-0.25),
  goalregulation=c(0.16,0.03,0.20,0.33,0.68,0.51,0.19,0.36,0.46,0.20,0.16,-0.12,-0.08,-0.44,-0.19),
  decisionmakingskill=c(0.20,0.14,0.11,0.38,0.46,0.50,-0.06,0.15,0.18,0.11,0.19,-0.02,-0.18,-0.32,-0.46),
  leadershipskill=c(0.31,0.15,0.37,0.08,0.32,0.31,0.48,0.76,0.53,0.29,0.09,0.12,-0.26,-0.38,-0.24),
  persuasiveskill=c(0.45,0.24,0.33,0.04,0.21,0.28,0.33,0.63,0.34,0.24,-0.17,0.21,-0.17,-0.28,-0.29),
  expressiveskill=c(0.38,0.09,0.26,0.14,0.29,0.24,0.59,0.55,0.53,0.34,0.05,0.25,-0.29,-0.38,-0.32),
  conversationalskill=c(0.18,-0.01,0.14,0.15,0.28,0.16,0.83,0.54,0.57,0.37,0.07,0.31,-0.26,-0.33,-0.20),
  energyregulation=c(0.20,0.21,0.25,0.50,0.75,0.47,0.31,0.45,0.72,0.29,0.20,0.12,-0.32,-0.63,-0.40),
  perspectivetakingskill=c(0.33,0.18,0.04,0.11,0.07,0.21,0.18,0.29,0.29,0.63,0.43,0.44,-0.14,-0.19,-0.22),
  capacityfortrust=c(0.25,0.15,0.07,0.14,0.01,0.08,0.23,0.11,0.22,0.52,0.25,0.82,-0.34,-0.23,-0.38),
  capacityforsocialwarmth=c(0.20,0.08,0.15,0.19,0.22,0.33,0.53,0.37,0.53,0.63,0.44,0.54,-0.34,-0.48,-0.36),
  teamworkskill=c(0.14,0.09,0.20,0.25,0.35,0.52,0.36,0.45,0.44,0.48,0.36,0.27,-0.17,-0.40,-0.31),
  ethicalcompetence=c(0.11,0.12,0.04,0.35,0.36,0.55,0.11,0.30,0.28,0.37,0.28,0.17,-0.31,-0.40,-0.42),
  stressregulation=c(0.13,0.12,0.24,0.29,0.23,0.20,0.18,0.20,0.20,0.11,0.16,0.27,-0.76,-0.54,-0.75),
  capacityforoptimism=c(0.07,0.06,0.14,0.35,0.32,0.26,0.35,0.28,0.52,0.39,0.34,0.39,-0.64,-0.81,-0.61),
  angermanagement=c(0.10,0.13,0.19,0.31,0.18,0.21,0.07,0.11,0.16,0.13,0.34,0.35,-0.63,-0.46,-0.86),
  confidenceregulation=c(0.10,0.06,0.11,0.34,0.43,0.37,0.25,0.32,0.49,0.25,0.19,0.19,-0.49,-0.78,-0.44),
  impulseregulation=c(0.09,0.15,0.09,0.40,0.41,0.36,-0.17,0.14,0.19,0.08,0.17,0.09,-0.36,-0.31,-0.48),
  selfreflectionskill=c(0.42,0.20,0.28,0.15,0.22,0.24,0.21,0.35,0.34,0.31,0.13,0.16,-0.22,-0.34,-0.29),
  adaptability=c(0.27,0.09,0.30,0.14,0.23,0.10,0.26,0.24,0.42,0.25,0.16,0.29,-0.50,-0.46,-0.45),
  capacityforindependence=c(0.27,0.12,0.20,0.21,0.37,0.36,0.11,0.36,0.30,0.21,0.20,0.03,-0.29,-0.50,-0.37))
colnames(published_table9) <- c(
  "bf_curiosity","bf_aesthetic","bf_creativity",
  "bf_organization","bf_productiveness","bf_responsibility",
  "bf_sociability","bf_assertiveness","bf_energy",
  "bf_compassion","bf_respectfulness","bf_trust",
  "bf_anxiety","bf_depression","bf_emotionalvolatility")
