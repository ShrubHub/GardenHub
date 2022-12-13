# Results models for growth differences comparing northern and southern willows
# in the garden to their source populations
# by Erica Zaja, created on 13/12/2022
# Last update: 13/12/2022

# growth: canopy height, shrub width, stem elongation, stem diameter over time 
# model structure: 
# CHANGE: lmer(growth_variable  ~ site*species +  (1|sample_year/field_sample_ID))
# N.B Site is Kluane or QHI