# Radiomics signature calculation

mydata <- read.csv(file = "filename",header = TRUE)
mydata <- data.frame (mydata)

mydata$feature1 = (mydata$AP_wavelet.HHH_glszm_ZonePercentage-0.000599)/0.000412
mydata$feature2 = (mydata$AP_wavelet.HLL_glcm_MaximumProbability-0.395684)/0.064144
mydata$feature3 = (mydata$AP_wavelet.HLL_glcm_InverseVariance-0.299016)/0.038406
mydata$feature4 = (mydata$AP_wavelet.HLL_ngtdm_Busyness-110.3791)/259.8173
mydata$feature5 = (mydata$AP_wavelet.LLL_glcm_Correlation-0.658187)/0.100549
mydata$feature6 = (mydata$AP_wavelet.HHL_glszm_SmallAreaHighGrayLevelEmphasis-2.372169)/4.776257
mydata$feature7 = (mydata$AP_wavelet.HHL_glszm_LowGrayLevelZoneEmphasis-0.509824)/0.200607
mydata$feature8 = (mydata$AT_wavelet.HLL_glszm_ZonePercentage-0.003287)/0.003003
mydata$feature9 = (mydata$AT_wavelet.LHL_firstorder_Maximum-68.54686)/64.13939
mydata$feature10 = (mydata$AT_wavelet.LLL_glcm_Imc2-0.831809)/0.072855
mydata$feature11 = (mydata$AT_wavelet.LLL_glcm_Imc1+0.27008)/0.052094
mydata$feature12 = (mydata$AT_wavelet.HHL_firstorder_Energy-55517.95)/136095.3
mydata$feature13 = (mydata$AT_wavelet.HHL_glszm_SizeZoneNonUniformity-2.028439)/1.792333
mydata$feature14 = (mydata$VP_wavelet.LHH_glszm_LowGrayLevelZoneEmphasis-0.42574)/0.167492
mydata$feature15 = (mydata$VP_wavelet.LLH_glcm_Imc2-0.394896)/0.097376
mydata$feature16 = (mydata$VP_wavelet.HHL_glszm_GrayLevelNonUniformityNormalized-0.48828)/0.113749
mydata$feature17 = (mydata$VP_wavelet.HLH_glszm_SmallAreaEmphasis-0.413513)/0.169768
mydata$feature18 = (mydata$VT_original_gldm_DependenceNonUniformityNormalized-0.047918)/0.007642
mydata$feature19 = (mydata$VT_wavelet.LHL_glszm_LargeAreaLowGrayLevelEmphasis-104199.9)/191904.6
mydata$feature20 = (mydata$VT_wavelet.LHH_ngtdm_Contrast-0.047607)/0.041749

mydata$radscore = - 0.035*mydata$feature1 + 0.067*mydata$feature2 - 0.078*mydata$feature3 + 0.207*mydata$feature4 + 0.113*mydata$feature5 + 0.080*mydata$feature6 - 0.170*mydata$feature7 + 0.083*mydata$feature8 + 0.100*mydata$feature9 + 0.024*mydata$feature10 - 0.087*mydata$feature11 + 0.043*mydata$feature12 + 0.014*mydata$feature13 + 0.154*mydata$feature14 - 0.106*mydata$feature15 - 0.156*mydata$feature16 + 0.104*mydata$feature17 + 0.067*mydata$feature18 + 0.016*mydata$feature19 + 0.082*mydata$feature20

write.csv(mydata$radscore,file = "filename",quote=F)