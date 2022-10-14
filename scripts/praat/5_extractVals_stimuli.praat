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
file_directory$ = "../../exp/empathy_intonation_perc/stim/wavs/"

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
maximum_pitch = 400

# -----------------------------------------------------------------------------




#
# Prepare loop ----------------------------------------------------------------
#

Create Strings as file list... dirFiles 'file_directory$'/*.wav
select Strings dirFiles
numberOfFiles = Get number of strings
writeInfoLine: "filename,duration,f0_00,f0_05,f0_10,f0_15,f0_20,f0_25,f0_30,f0_35,
...f0_40,f0_45,f0_50,f0_55,f0_60,f0_65,f0_70,f0_75,f0_80,f0_85,f0_90,f0_95,
...f0_100,f1_00,f1_05,f1_10,f1_15,f1_20,f1_25,f1_30,f1_35,f1_40,f1_45,f1_50,
...f1_55,f1_60,f1_65,f1_70,f1_75,f1_80,f1_85,f1_90,f1_95,f1_100,f2_00,f2_05,
...f2_10,f2_15,f2_20,f2_25,f2_30,f2_35,f2_40,f2_45,f2_50,f2_55,f2_60,f2_65,
...f2_70,f2_75,f2_80,f2_85,f2_90,f2_95,f2_100,in_00,in_05,in_10,in_15,in_20,
...in_25,in_30,in_35,in_40,in_45,in_50,in_55,in_60,in_65,in_70,in_75,in_80,
...in_85,in_90,in_95,in_100"

# -----------------------------------------------------------------------------



#
# Start loop ------------------------------------------------------------------
#

for file to numberOfFiles
	select Strings dirFiles
	fileName$ = Get string: file
	prefix$ = fileName$ - ".wav"
	Read from file... 'file_directory$'/'prefix$'.wav
	Read from file... 'file_directory$'/'prefix$'.TextGrid

	select Sound 'prefix$'
	To Formant (burg)... time_step maximum_number_of_formants maximum_formant 
	...window_length preemphasis_from
	
	select Sound 'prefix$'
	To Pitch... pitch_time_step minimum_pitch maximum_pitch

	select Sound 'prefix$'
	To Intensity: 100, 0, "yes"

	select TextGrid 'prefix$'
	numberOfIntervals = Get number of intervals... tier

	# duration:
	start = Get starting point: 1, 2
	end = Get end time of interval: 1, 2
	duration = end-start

	perc_05 = (duration * 0.05) + start
	perc_10 = (duration * 0.10) + start
	perc_15 = (duration * 0.15) + start
	perc_20 = (duration * 0.20) + start
	perc_25 = (duration * 0.25) + start
	perc_30 = (duration * 0.30) + start
	perc_35 = (duration * 0.35) + start
	perc_40 = (duration * 0.40) + start
	perc_45 = (duration * 0.45) + start
	perc_50 = (duration * 0.50) + start
	perc_55 = (duration * 0.55) + start
	perc_60 = (duration * 0.60) + start
	perc_65 = (duration * 0.65) + start
	perc_70 = (duration * 0.70) + start
	perc_75 = (duration * 0.75) + start
	perc_80 = (duration * 0.80) + start
	perc_85 = (duration * 0.85) + start
	perc_90 = (duration * 0.90) + start
	perc_95 = (duration * 0.95) + start

	# formants:
	select Formant 'prefix$'
	f1_00 = Get value at time... 1 start Hertz Linear
	f2_00 = Get value at time... 2 start Hertz Linear
	f1_05 = Get value at time... 1 perc_05 Hertz Linear
	f2_05 = Get value at time... 2 perc_05 Hertz Linear
	f1_10 = Get value at time... 1 perc_10 Hertz Linear
	f2_10 = Get value at time... 2 perc_10 Hertz Linear
	f1_15 = Get value at time... 1 perc_15 Hertz Linear
	f2_15 = Get value at time... 2 perc_15 Hertz Linear
	f1_20 = Get value at time... 1 perc_20 Hertz Linear
	f2_20 = Get value at time... 2 perc_20 Hertz Linear
	f1_25 = Get value at time... 1 perc_25 Hertz Linear
	f2_25 = Get value at time... 2 perc_25 Hertz Linear
	f1_30 = Get value at time... 1 perc_30 Hertz Linear
	f2_30 = Get value at time... 2 perc_30 Hertz Linear
	f1_35 = Get value at time... 1 perc_35 Hertz Linear
	f2_35 = Get value at time... 2 perc_35 Hertz Linear
	f1_40 = Get value at time... 1 perc_40 Hertz Linear
	f2_40 = Get value at time... 2 perc_40 Hertz Linear
	f1_45 = Get value at time... 1 perc_45 Hertz Linear
	f2_45 = Get value at time... 2 perc_45 Hertz Linear
	f1_50 = Get value at time... 1 perc_50 Hertz Linear
	f2_50 = Get value at time... 2 perc_50 Hertz Linear
	f1_55 = Get value at time... 1 perc_55 Hertz Linear
	f2_55 = Get value at time... 2 perc_55 Hertz Linear
	f1_60 = Get value at time... 1 perc_60 Hertz Linear
	f2_60 = Get value at time... 2 perc_60 Hertz Linear
	f1_65 = Get value at time... 1 perc_65 Hertz Linear
	f2_65 = Get value at time... 2 perc_65 Hertz Linear
	f1_70 = Get value at time... 1 perc_70 Hertz Linear
	f2_70 = Get value at time... 2 perc_70 Hertz Linear
	f1_75 = Get value at time... 1 perc_75 Hertz Linear
	f2_75 = Get value at time... 2 perc_75 Hertz Linear
	f1_80 = Get value at time... 1 perc_80 Hertz Linear
	f2_80 = Get value at time... 2 perc_80 Hertz Linear
	f1_85 = Get value at time... 1 perc_85 Hertz Linear
	f2_85 = Get value at time... 2 perc_85 Hertz Linear
	f1_90 = Get value at time... 1 perc_90 Hertz Linear
	f2_90 = Get value at time... 2 perc_90 Hertz Linear
	f1_95 = Get value at time... 1 perc_95 Hertz Linear
	f2_95 = Get value at time... 2 perc_95 Hertz Linear
	f1_100 = Get value at time... 1 end Hertz Linear
	f2_100 = Get value at time... 2 end Hertz Linear

	# pitch:
	select Pitch 'prefix$'
	f0_00 = Get value at time... start Hertz Linear
	f0_05 = Get value at time... perc_05 Hertz Linear
	f0_10 = Get value at time... perc_10 Hertz Linear
	f0_15 = Get value at time... perc_15 Hertz Linear
	f0_20 = Get value at time... perc_20 Hertz Linear
	f0_25 = Get value at time... perc_25 Hertz Linear
	f0_30 = Get value at time... perc_30 Hertz Linear
	f0_35 = Get value at time... perc_35 Hertz Linear
	f0_40 = Get value at time... perc_40 Hertz Linear
	f0_45 = Get value at time... perc_45 Hertz Linear
	f0_50 = Get value at time... perc_50 Hertz Linear
	f0_55 = Get value at time... perc_55 Hertz Linear
	f0_60 = Get value at time... perc_60 Hertz Linear
	f0_65 = Get value at time... perc_65 Hertz Linear
	f0_70 = Get value at time... perc_70 Hertz Linear
	f0_75 = Get value at time... perc_75 Hertz Linear
	f0_80 = Get value at time... perc_80 Hertz Linear
	f0_85 = Get value at time... perc_85 Hertz Linear
	f0_90 = Get value at time... perc_90 Hertz Linear
	f0_95 = Get value at time... perc_95 Hertz Linear
	f0_100 = Get value at time... end Hertz Linear

	# intensity:
	select Intensity 'prefix$'
	in_00 = Get value at time... start Cubic
	in_05 = Get value at time... perc_05 Cubic
	in_10 = Get value at time... perc_10 Cubic
	in_15 = Get value at time... perc_15 Cubic
	in_20 = Get value at time... perc_20 Cubic
	in_25 = Get value at time... perc_25 Cubic
	in_30 = Get value at time... perc_30 Cubic
	in_35 = Get value at time... perc_35 Cubic
	in_40 = Get value at time... perc_40 Cubic
	in_45 = Get value at time... perc_45 Cubic
	in_50 = Get value at time... perc_50 Cubic
	in_55 = Get value at time... perc_55 Cubic
	in_60 = Get value at time... perc_60 Cubic
	in_65 = Get value at time... perc_65 Cubic
	in_70 = Get value at time... perc_70 Cubic
	in_75 = Get value at time... perc_75 Cubic
	in_80 = Get value at time... perc_80 Cubic
	in_85 = Get value at time... perc_85 Cubic
	in_90 = Get value at time... perc_90 Cubic
	in_95 = Get value at time... perc_95 Cubic
	in_100 = Get value at time... end Cubic

	appendInfo: "'prefix$','duration','f0_00','f0_05','f0_10','f0_15',
	...'f0_20','f0_25','f0_30','f0_35','f0_40','f0_45','f0_50','f0_55',
	...'f0_60','f0_65','f0_70','f0_75','f0_80','f0_85','f0_90','f0_95',
	...'f0_100','f1_00','f1_05','f1_10','f1_15','f1_20','f1_25','f1_30',
	...'f1_35','f1_40','f1_45','f1_50','f1_55','f1_60','f1_65','f1_70',
	...'f1_75','f1_80','f1_85','f1_90','f1_95','f1_100','f2_00','f2_05',
	...'f2_10','f2_15','f2_20','f2_25','f2_30','f2_35','f2_40','f2_45',
	...'f2_50','f2_55','f2_60','f2_65','f2_70','f2_75','f2_80','f2_85',
	...'f2_90','f2_95','f2_100','in_00','in_05','in_10','in_15','in_20',
	...'in_25','in_30','in_35','in_40','in_45','in_50','in_55','in_60',
	...'in_65','in_70','in_75','in_80','in_85','in_90','in_95','in_100''newline$'"

endfor

# -----------------------------------------------------------------------------


# Clean up
select all
Remove
