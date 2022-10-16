# -----------------------------------------------------------------------------
# Praat script to extract values   
# Created by                       
# Joseph V. Casillas 10/11/2022
#                                  
# This file will:                  
#  - extract duration, f0, f1, f2 and intentsity     
#  - save output to ./data/raw/ dir     
# -----------------------------------------------------------------------------



#
# Set some parameters ---------------------------------------------------------
#

# Directory of sound and textgrid files:
sound_directory$ = "../../data/stimuli/sounds/"
textgrid_directory$ = "../../data/stimuli/textgrids/"


# Which tier do you want to analyze?
tier = 1
	
# Formant analysis parameters
time_step = 0.01
maximum_number_of_formants = 5
maximum_formant = 6000
window_length = 0.025
preemphasis_from = 50

# Pitch analysis parameters
pitch_time_step = 0.01
minimum_pitch = 50
maximum_pitch = 500

# -----------------------------------------------------------------------------




#
# Prepare loop ----------------------------------------------------------------
#

Create Strings as file list... dirFiles 'sound_directory$'/*.wav
select Strings dirFiles
numberOfFiles = Get number of strings
writeInfoLine: "filename,duration,	f0_00,f0_01,f0_02,f0_03,f0_04,f0_05,f0_06,
...f0_07,f0_08,f0_09,f0_10,f0_11,f0_12,f0_13,f0_14,f0_15,f0_16,f0_17,f0_18,
...f0_19,f0_20,f0_21,f0_22,f0_23,f0_24,f0_25,f0_26,f0_27,f0_28,f0_29,f0_30,
...f0_31,f0_32,f0_33,f0_34,f0_35,f0_36,f0_37,f0_38,f0_39,f0_40,f0_41,f0_42,
...f0_43,f0_44,f0_45,f0_46,f0_47,f0_48,f0_49,f0_50,f0_51,f0_52,f0_53,f0_54,
...f0_55,f0_56,f0_57,f0_58,f0_59,f0_60,f0_61,f0_62,f0_63,f0_64,f0_65,f0_66,
...f0_67,f0_68,f0_69,f0_70,f0_71,f0_72,f0_73,f0_74,f0_75,f0_76,f0_77,f0_78,
...f0_79,f0_80,f0_81,f0_82,f0_83,f0_84,f0_85,f0_86,f0_87,f0_88,f0_89,f0_90,
...f0_91,f0_92,f0_93,f0_94,f0_95,f0_96,f0_97,f0_98,f0_99,f0_100"

# -----------------------------------------------------------------------------



#
# Start loop ------------------------------------------------------------------
#

for file to numberOfFiles
	select Strings dirFiles
	fileName$ = Get string: file
	prefix$ = fileName$ - ".wav"
	Read from file... 'sound_directory$'/'prefix$'.wav
	Read from file... 'textgrid_directory$'/'prefix$'.TextGrid

	select Sound 'prefix$'
	To Pitch... pitch_time_step minimum_pitch maximum_pitch

	select TextGrid 'prefix$'
	numberOfIntervals = Get number of intervals... tier

	# duration:
	start = Get starting point: 1, 2
	end = Get end time of interval: 1, 2
	duration = end-start

	perc_01 = (duration * 0.01) + start
	perc_02 = (duration * 0.02) + start
	perc_03 = (duration * 0.03) + start
	perc_04 = (duration * 0.04) + start
	perc_05 = (duration * 0.05) + start
	perc_06 = (duration * 0.06) + start
	perc_07 = (duration * 0.07) + start
	perc_08 = (duration * 0.08) + start
	perc_09 = (duration * 0.09) + start
	perc_10 = (duration * 0.10) + start
	perc_11 = (duration * 0.11) + start
	perc_12 = (duration * 0.12) + start
	perc_13 = (duration * 0.13) + start
	perc_14 = (duration * 0.14) + start
	perc_15 = (duration * 0.15) + start
	perc_16 = (duration * 0.16) + start
	perc_17 = (duration * 0.17) + start
	perc_18 = (duration * 0.18) + start
	perc_19 = (duration * 0.19) + start
	perc_20 = (duration * 0.20) + start
	perc_21 = (duration * 0.21) + start
	perc_22 = (duration * 0.22) + start
	perc_23 = (duration * 0.23) + start
	perc_24 = (duration * 0.24) + start
	perc_25 = (duration * 0.25) + start
	perc_26 = (duration * 0.26) + start
	perc_27 = (duration * 0.27) + start
	perc_28 = (duration * 0.28) + start
	perc_29 = (duration * 0.29) + start
	perc_30 = (duration * 0.30) + start
	perc_31 = (duration * 0.31) + start
	perc_32 = (duration * 0.32) + start
	perc_33 = (duration * 0.33) + start
	perc_34 = (duration * 0.34) + start
	perc_35 = (duration * 0.35) + start
	perc_36 = (duration * 0.36) + start
	perc_37 = (duration * 0.37) + start
	perc_38 = (duration * 0.38) + start
	perc_39 = (duration * 0.39) + start
	perc_40 = (duration * 0.40) + start
	perc_41 = (duration * 0.41) + start
	perc_42 = (duration * 0.42) + start
	perc_43 = (duration * 0.43) + start
	perc_44 = (duration * 0.44) + start
	perc_45 = (duration * 0.45) + start
	perc_46 = (duration * 0.46) + start
	perc_47 = (duration * 0.47) + start
	perc_48 = (duration * 0.48) + start
	perc_49 = (duration * 0.49) + start
	perc_50 = (duration * 0.50) + start
	perc_51 = (duration * 0.51) + start
	perc_52 = (duration * 0.52) + start
	perc_53 = (duration * 0.53) + start
	perc_54 = (duration * 0.54) + start
	perc_55 = (duration * 0.55) + start
	perc_56 = (duration * 0.56) + start
	perc_57 = (duration * 0.57) + start
	perc_58 = (duration * 0.58) + start
	perc_59 = (duration * 0.59) + start
	perc_60 = (duration * 0.60) + start
	perc_61 = (duration * 0.61) + start
	perc_62 = (duration * 0.62) + start
	perc_63 = (duration * 0.63) + start
	perc_64 = (duration * 0.64) + start
	perc_65 = (duration * 0.65) + start
	perc_66 = (duration * 0.66) + start
	perc_67 = (duration * 0.67) + start
	perc_68 = (duration * 0.68) + start
	perc_69 = (duration * 0.69) + start
	perc_70 = (duration * 0.70) + start
	perc_71 = (duration * 0.71) + start
	perc_72 = (duration * 0.72) + start
	perc_73 = (duration * 0.73) + start
	perc_74 = (duration * 0.74) + start
	perc_75 = (duration * 0.75) + start
	perc_76 = (duration * 0.76) + start
	perc_77 = (duration * 0.77) + start
	perc_78 = (duration * 0.78) + start
	perc_79 = (duration * 0.79) + start
	perc_80 = (duration * 0.80) + start
	perc_81 = (duration * 0.81) + start
	perc_82 = (duration * 0.82) + start
	perc_83 = (duration * 0.83) + start
	perc_84 = (duration * 0.84) + start
	perc_85 = (duration * 0.85) + start
	perc_86 = (duration * 0.86) + start
	perc_87 = (duration * 0.87) + start
	perc_88 = (duration * 0.88) + start
	perc_89 = (duration * 0.89) + start
	perc_90 = (duration * 0.90) + start
	perc_91 = (duration * 0.91) + start
	perc_92 = (duration * 0.92) + start
	perc_93 = (duration * 0.93) + start
	perc_94 = (duration * 0.94) + start
	perc_95 = (duration * 0.95) + start
	perc_96 = (duration * 0.96) + start
	perc_97 = (duration * 0.97) + start
	perc_98 = (duration * 0.98) + start
	perc_99 = (duration * 0.99) + start

	# pitch:
	select Pitch 'prefix$'
	f0_00 = Get value at time... start Hertz Linear
	f0_01 = Get value at time... perc_01 Hertz Linear
	f0_02 = Get value at time... perc_02 Hertz Linear
	f0_03 = Get value at time... perc_03 Hertz Linear
	f0_04 = Get value at time... perc_04 Hertz Linear
	f0_05 = Get value at time... perc_05 Hertz Linear
	f0_06 = Get value at time... perc_06 Hertz Linear
	f0_07 = Get value at time... perc_07 Hertz Linear
	f0_08 = Get value at time... perc_08 Hertz Linear
	f0_09 = Get value at time... perc_09 Hertz Linear
	f0_10 = Get value at time... perc_10 Hertz Linear
	f0_11 = Get value at time... perc_11 Hertz Linear
	f0_12 = Get value at time... perc_12 Hertz Linear
	f0_13 = Get value at time... perc_13 Hertz Linear
	f0_14 = Get value at time... perc_14 Hertz Linear
	f0_15 = Get value at time... perc_15 Hertz Linear
	f0_16 = Get value at time... perc_16 Hertz Linear
	f0_17 = Get value at time... perc_17 Hertz Linear
	f0_18 = Get value at time... perc_18 Hertz Linear
	f0_19 = Get value at time... perc_19 Hertz Linear
	f0_20 = Get value at time... perc_20 Hertz Linear
	f0_21 = Get value at time... perc_21 Hertz Linear
	f0_22 = Get value at time... perc_22 Hertz Linear
	f0_23 = Get value at time... perc_23 Hertz Linear
	f0_24 = Get value at time... perc_24 Hertz Linear
	f0_25 = Get value at time... perc_25 Hertz Linear
	f0_26 = Get value at time... perc_26 Hertz Linear
	f0_27 = Get value at time... perc_27 Hertz Linear
	f0_28 = Get value at time... perc_28 Hertz Linear
	f0_29 = Get value at time... perc_29 Hertz Linear
	f0_30 = Get value at time... perc_30 Hertz Linear
	f0_31 = Get value at time... perc_31 Hertz Linear
	f0_32 = Get value at time... perc_32 Hertz Linear
	f0_33 = Get value at time... perc_33 Hertz Linear
	f0_34 = Get value at time... perc_34 Hertz Linear
	f0_35 = Get value at time... perc_35 Hertz Linear
	f0_36 = Get value at time... perc_36 Hertz Linear
	f0_37 = Get value at time... perc_37 Hertz Linear
	f0_38 = Get value at time... perc_38 Hertz Linear
	f0_39 = Get value at time... perc_39 Hertz Linear
	f0_40 = Get value at time... perc_40 Hertz Linear
	f0_41 = Get value at time... perc_41 Hertz Linear
	f0_42 = Get value at time... perc_42 Hertz Linear
	f0_43 = Get value at time... perc_43 Hertz Linear
	f0_44 = Get value at time... perc_44 Hertz Linear
	f0_45 = Get value at time... perc_45 Hertz Linear
	f0_46 = Get value at time... perc_46 Hertz Linear
	f0_47 = Get value at time... perc_47 Hertz Linear
	f0_48 = Get value at time... perc_48 Hertz Linear
	f0_49 = Get value at time... perc_49 Hertz Linear
	f0_50 = Get value at time... perc_50 Hertz Linear
	f0_51 = Get value at time... perc_51 Hertz Linear
	f0_52 = Get value at time... perc_52 Hertz Linear
	f0_53 = Get value at time... perc_53 Hertz Linear
	f0_54 = Get value at time... perc_54 Hertz Linear
	f0_55 = Get value at time... perc_55 Hertz Linear
	f0_56 = Get value at time... perc_56 Hertz Linear
	f0_57 = Get value at time... perc_57 Hertz Linear
	f0_58 = Get value at time... perc_58 Hertz Linear
	f0_59 = Get value at time... perc_59 Hertz Linear
	f0_60 = Get value at time... perc_60 Hertz Linear
	f0_61 = Get value at time... perc_61 Hertz Linear
	f0_62 = Get value at time... perc_62 Hertz Linear
	f0_63 = Get value at time... perc_63 Hertz Linear
	f0_64 = Get value at time... perc_64 Hertz Linear
	f0_65 = Get value at time... perc_65 Hertz Linear
	f0_66 = Get value at time... perc_66 Hertz Linear
	f0_67 = Get value at time... perc_67 Hertz Linear
	f0_68 = Get value at time... perc_68 Hertz Linear
	f0_69 = Get value at time... perc_69 Hertz Linear
	f0_70 = Get value at time... perc_70 Hertz Linear
	f0_71 = Get value at time... perc_71 Hertz Linear
	f0_72 = Get value at time... perc_72 Hertz Linear
	f0_73 = Get value at time... perc_73 Hertz Linear
	f0_74 = Get value at time... perc_74 Hertz Linear
	f0_75 = Get value at time... perc_75 Hertz Linear
	f0_76 = Get value at time... perc_76 Hertz Linear
	f0_77 = Get value at time... perc_77 Hertz Linear
	f0_78 = Get value at time... perc_78 Hertz Linear
	f0_79 = Get value at time... perc_79 Hertz Linear
	f0_80 = Get value at time... perc_80 Hertz Linear
	f0_81 = Get value at time... perc_81 Hertz Linear
	f0_82 = Get value at time... perc_82 Hertz Linear
	f0_83 = Get value at time... perc_83 Hertz Linear
	f0_84 = Get value at time... perc_84 Hertz Linear
	f0_85 = Get value at time... perc_85 Hertz Linear
	f0_86 = Get value at time... perc_86 Hertz Linear
	f0_87 = Get value at time... perc_87 Hertz Linear
	f0_88 = Get value at time... perc_88 Hertz Linear
	f0_89 = Get value at time... perc_89 Hertz Linear
	f0_90 = Get value at time... perc_90 Hertz Linear
	f0_91 = Get value at time... perc_91 Hertz Linear
	f0_92 = Get value at time... perc_92 Hertz Linear
	f0_93 = Get value at time... perc_93 Hertz Linear
	f0_94 = Get value at time... perc_94 Hertz Linear
	f0_95 = Get value at time... perc_95 Hertz Linear
	f0_96 = Get value at time... perc_96 Hertz Linear
	f0_97 = Get value at time... perc_97 Hertz Linear
	f0_98 = Get value at time... perc_98 Hertz Linear
	f0_99 = Get value at time... perc_99 Hertz Linear
	f0_100 = Get value at time... end Hertz Linear

	appendInfo: "'prefix$','duration',	'f0_00','f0_01',
	...'f0_02','f0_03','f0_04','f0_05','f0_06','f0_07',
	...'f0_08','f0_09','f0_10','f0_11','f0_12','f0_13',
	...'f0_14','f0_15','f0_16','f0_17','f0_18','f0_19',
	...'f0_20','f0_21','f0_22','f0_23','f0_24','f0_25',
	...'f0_26','f0_27','f0_28','f0_29','f0_30','f0_31',
	...'f0_32','f0_33','f0_34','f0_35','f0_36','f0_37',
	...'f0_38','f0_39','f0_40','f0_41','f0_42','f0_43',
	...'f0_44','f0_45','f0_46','f0_47','f0_48','f0_49',
	...'f0_50','f0_51','f0_52','f0_53','f0_54','f0_55',
	...'f0_56','f0_57','f0_58','f0_59','f0_60','f0_61',
	...'f0_62','f0_63','f0_64','f0_65','f0_66','f0_67',
	...'f0_68','f0_69','f0_70','f0_71','f0_72','f0_73',
	...'f0_74','f0_75','f0_76','f0_77','f0_78','f0_79',
	...'f0_80','f0_81','f0_82','f0_83','f0_84','f0_85',
	...'f0_86','f0_87','f0_88','f0_89','f0_90','f0_91',
	...'f0_92','f0_93','f0_94','f0_95','f0_96','f0_97',
	...'f0_98','f0_99','f0_100''newline$'"

endfor

# -----------------------------------------------------------------------------


# Clean up
select all
Remove








