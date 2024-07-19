require(tseries)
data("USMacroG")
pp.test(USMacroG[,9]) ### Unemployment
pp.test(USMacroG[-1,11]) ### Inflation


