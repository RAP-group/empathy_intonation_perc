form Select data
  comment Output file name:
  sentence file plot.png
  text caption title
  comment Set path to wav audio file and TextGrid:
  sentence wav ../../data/stimuli/sounds/andalusian_match_interrogative-partial-wh_Por-que-abre-el-regalo.wav
  sentence tg ../../data/stimuli/textgrids/andalusian_match_interrogative-partial-wh_Por-que-abre-el-regalo.textgrid
  comment Start and end time to be plotted:
  real start 0
  real end 0
  comment Output file settings:
  positive width 5
  sentence format png
  comment Include f0 track?
  sentence pitch TRUE
  comment f0 minimum and maximum:
  real pitch_min 0
  real pitch_max 500
  comment Hz maximum:
  real hz_max 5000
endform

# Set dimensions ####################################################
# All dimensions are in inches.

# Width and height of the plotting window (not of the whole plot).
win_width = width - (0.7 * 2)
win_height = 2
# Height of the waveform panel.
wav_height = 0.5
# Height of the spectrogram panel.
sp_height = 1.5

# Read files ########################################################

wav = Read from file: wav$

if end == 0
  end = Get end time
endif

# Set plotting window dimensions if f0 will be included.
if pitch$ != "FALSE"
  pitch_height = 0
  sp_height = sp_height
else
  pitch_height = 0
endif

# Set plotting window dimensions if textgrid will be included.
if tg$ != "FALSE"
  tg = Read from file: tg$
  tg_tiers = Get number of tiers
  tg_height = win_height + (0.5 * tg_tiers)
endif

# To objects ########################################################

selectObject: wav
sp = To Spectrogram: 0.005, hz_max, 0.002, 20, "Gaussian"

if pitch$ != "FALSE"
  selectObject: wav
  pitch_obj = To Pitch: 0, pitch_min, pitch_max
  #pitch_obj = To Pitch (cc): 0, pitch_min, 15, "no", 0.03, 0.45, 0.01, 0.35, 0.14, pitch_max
endif

# Set up plotting params ############################################

Erase all

Font size: 8
Times

# Plot ##############################################################

### Spectrogram #####################################################

Select inner viewport: 0, win_width, wav_height, wav_height + sp_height

selectObject: sp
Paint: start, end, 0, 0, 100, "no", 40, 6, 0, "no"
Draw inner box

### f0 track ########################################################

if pitch$ != "FALSE"
  Select inner viewport: 0, win_width, wav_height, wav_height + sp_height

  selectObject: pitch_obj
  Line width: 9
  White
  Draw: start, end, pitch_min, pitch_max, "no"
  Line width: 6
  Blue
  Draw: start, end, pitch_min, pitch_max, "no"
  Line width: 1
  Black
  Axes: start, end, pitch_min, pitch_max
  Marks left: 2, "yes", "yes", "no"
endif

### Spectrogram box #################################################

Select inner viewport: 0, win_width, wav_height, wav_height + sp_height

Draw inner box

### TextGrid ########################################################

if tg$ != "FALSE"
  Select inner viewport: 0, win_width, 0, tg_height
  selectObject: tg
  Draw: start, end, "yes", "yes", "no"
  inner_height = tg_height
else
  inner_height = win_height
endif

### Waveform ########################################################

Select inner viewport: 0, win_width, 0, wav_height

selectObject: wav
Silver
Draw: start, end, 0, 0, "no", "Curve"
Black

### Axes ############################################################

int_min = Get minimum: start, end, "sinc70"
int_max = Get maximum: start, end, "sinc70"

Axes: start, end, int_min, int_max
#One mark right: int_min, "no", "yes", "no", fixed$(int_min, 2)
#One mark right: 0, "yes", "yes", "yes", ""
#One mark right: int_max, "no", "yes", "no", fixed$(int_max, 2)

Select inner viewport: 0, win_width, wav_height, wav_height + sp_height

Axes: start, end, 0, hz_max
#Marks left: 2, "yes", "yes", "no"
Text left: "no", "F0 (Hz)"

Select inner viewport: 0, win_width, 0, inner_height

Draw inner box
Marks bottom every: 1, 1, "no", "no", "no"
Text bottom: "no", caption$

### Select plot and save ###########################################

Select inner viewport: 0.25, win_width - 0.25, 0.25, inner_height - 0.1

if format$ == "png"
  Save as 600-dpi PNG file: file$
elif format$ == "pdf"
  Save as PDF file: file$
else
  writeInfoLine: "Format not recognised."
endif
