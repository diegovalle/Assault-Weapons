#Check for coding errors
with(hom.border, Firearm.Homicide <= Homicide)
with(hom.borderct, Firearm.Homicides <= Homicides)
with(hom, Murders.with.Firearm <= Murders)
with(hom.region, Firearm.Homicide <= Homicide)
with(sui, Firearm.Suicides < Suicides)
