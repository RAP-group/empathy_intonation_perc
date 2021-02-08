Create Strings as file list... soundFiles ../../exp/stim/wavs/*.wav
select Strings soundFiles
numberOfFiles = Get number of strings

for i to numberOfFiles
	select Strings soundFiles
	soundName$ = Get string... i
	Read from file... ../../exp/stim/wavs/'soundName$'
	Scale peak... 0.99
	Save as WAV file... ../../exp/stim/wavs/'soundName$'
	Remove
endfor