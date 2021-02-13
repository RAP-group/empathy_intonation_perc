#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2020.2.10),
    on Sat Feb 13 01:00:25 2021
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""

from __future__ import absolute_import, division

from psychopy import locale_setup
from psychopy import prefs
from psychopy import sound, gui, visual, core, data, event, logging, clock
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle
import os  # handy system and path functions
import sys  # to get file system encoding

from psychopy.hardware import keyboard



# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)

# Store info about the experiment session
psychopyVersion = '2020.2.10'
expName = 'empathy_intonation_perc'  # from the Builder filename that created this script
expInfo = {'id': '', 'Variety of Spanish that is most familiar to you?': ''}
dlg = gui.DlgFromDict(dictionary=expInfo, sortKeys=False, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + u'data/%s_%s_%s' % (expInfo['id'], expName, expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='/Users/casillas/academia/research/in_progress/empathy_intonation_perc/exp/empathy_intonation_perc/empathy_intonation_perc_lastrun.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.DEBUG)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp
frameTolerance = 0.001  # how close to onset before 'same' frame

# Start Code - component code to be run after the window creation

# Setup the Window
win = visual.Window(
    size=[1920, 1200], fullscr=True, screen=0, 
    winType='pyglet', allowGUI=False, allowStencil=False,
    monitor='testMonitor', color=[0,0,0], colorSpace='rgb',
    blendMode='avg', useFBO=True, 
    units='height')
# store frame rate of monitor if we can measure it
expInfo['frameRate'] = win.getActualFrameRate()
if expInfo['frameRate'] != None:
    frameDur = 1.0 / round(expInfo['frameRate'])
else:
    frameDur = 1.0 / 60.0  # could not measure, so guess

# create a default keyboard (e.g. to check for escape)
defaultKeyboard = keyboard.Keyboard()

# Initialize components for Routine "instructions_2afc"
instructions_2afcClock = core.Clock()
text_2afc_instruction = visual.TextStim(win=win, name='text_2afc_instruction',
    text='default text',
    font='Arial',
    pos=(0, 0.1), height=0.05, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
text_2afc_instructions_continue = visual.TextStim(win=win, name='text_2afc_instructions_continue',
    text='default text',
    font='Arial',
    pos=(0, -0.4), height=0.045, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);
key_resp_2afc_instructions = keyboard.Keyboard()

# Initialize components for Routine "practice_2afc"
practice_2afcClock = core.Clock()
sound_2afc_practice_stim = sound.Sound('A', secs=-1, stereo=True, hamming=True,
    name='sound_2afc_practice_stim')
sound_2afc_practice_stim.setVolume(1)
text_2afc_response_yes_practice = visual.TextStim(win=win, name='text_2afc_response_yes_practice',
    text='Yes',
    font='Arial',
    pos=(-0.3, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);
text_2afc_response_no_practice = visual.TextStim(win=win, name='text_2afc_response_no_practice',
    text='No',
    font='Arial',
    pos=(0.3, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);
key_resp_2afc_practice = keyboard.Keyboard()
text_2afc_question_practice = visual.TextStim(win=win, name='text_2afc_question_practice',
    text='Is it a question?',
    font='Arial',
    pos=(0, 0.3), height=0.1, wrapWidth=None, ori=0, 
    color='blue', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-4.0);

# Initialize components for Routine "check_2afc"
check_2afcClock = core.Clock()
text_2afc_check = visual.TextStim(win=win, name='text_2afc_check',
    text='Great! Now we’ll begin for real. \n\nTry to respond as quickly and as accurately as possible after hearing each utterance.\n\n(press the spacebar to start)',
    font='Arial',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
key_resp_2afc_gotit = keyboard.Keyboard()

# Initialize components for Routine "trial_2afc"
trial_2afcClock = core.Clock()
col_name = ['andalusian', 'argentine', 'castilian', 'chilean', 'cuban', 'mexican', 'peruvian', 'puertorican']
sound_2afc_stim_trial = sound.Sound('A', secs=-1, stereo=True, hamming=True,
    name='sound_2afc_stim_trial')
sound_2afc_stim_trial.setVolume(1)
text_2afc_response_yes_trial = visual.TextStim(win=win, name='text_2afc_response_yes_trial',
    text='Yes',
    font='Arial',
    pos=(-0.3, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);
text_2afc_response_no_trial = visual.TextStim(win=win, name='text_2afc_response_no_trial',
    text='No',
    font='Arial',
    pos=(0.3, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-3.0);
key_resp_2afc_trial = keyboard.Keyboard()
text_2afc_question_trial = visual.TextStim(win=win, name='text_2afc_question_trial',
    text='Is this a question?',
    font='Arial',
    pos=(0, 0.3), height=0.1, wrapWidth=None, ori=0, 
    color='blue', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-5.0);

# Initialize components for Routine "instructions_lextale"
instructions_lextaleClock = core.Clock()
text_lextale_instructions = visual.TextStim(win=win, name='text_lextale_instructions',
    text='default text',
    font='Arial',
    pos=(0, 0), height=0.05, wrapWidth=0.8, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
key_resp_lextale_instructions = keyboard.Keyboard()
text_lextale_instructions_continue = visual.TextStim(win=win, name='text_lextale_instructions_continue',
    text='default text',
    font='Arial',
    pos=(0, -0.35), height=0.04, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);

# Initialize components for Routine "practice_lextale"
practice_lextaleClock = core.Clock()
text_lextale_practice_label = visual.TextStim(win=win, name='text_lextale_practice_label',
    text='PRACTICE',
    font='Arial',
    pos=(0, 0.3), height=0.1, wrapWidth=None, ori=0, 
    color='red', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
text_lextale_practice_word = visual.TextStim(win=win, name='text_lextale_practice_word',
    text='default text',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='blue', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);
key_resp_lextale_practice = keyboard.Keyboard()
text_lextale_practice_real_label = visual.TextStim(win=win, name='text_lextale_practice_real_label',
    text='default text',
    font='Arial',
    pos=(-0.3, -0.3), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-3.0);
text_lextale_practice_false_label = visual.TextStim(win=win, name='text_lextale_practice_false_label',
    text='default text',
    font='Arial',
    pos=(0.3, -0.3), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-4.0);

# Initialize components for Routine "practice_feedback_lextale"
practice_feedback_lextaleClock = core.Clock()
text_lextale_practice_feedback = visual.TextStim(win=win, name='text_lextale_practice_feedback',
    text='default text',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);
key_resp_lextale_practice_feedback = keyboard.Keyboard()

# Initialize components for Routine "check_lextale"
check_lextaleClock = core.Clock()
text_lextale_check = visual.TextStim(win=win, name='text_lextale_check',
    text='Great! Let’s start. \n\nPress the spacebar to continue.',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
key_resp_lextale_check = keyboard.Keyboard()

# Initialize components for Routine "trial_lextale"
trial_lextaleClock = core.Clock()
text_lextale_trial_word = visual.TextStim(win=win, name='text_lextale_trial_word',
    text='default text',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='blue', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
key_resp_lextale_trial = keyboard.Keyboard()
text_lextale_trial_real_label = visual.TextStim(win=win, name='text_lextale_trial_real_label',
    text='default text',
    font='Arial',
    pos=(-0.3, -0.3), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);
text_lextale_trial_false_label = visual.TextStim(win=win, name='text_lextale_trial_false_label',
    text='default text',
    font='Arial',
    pos=(0.3, -0.3), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-3.0);

# Initialize components for Routine "instructions_eq"
instructions_eqClock = core.Clock()
key_resp_eq_instructions = keyboard.Keyboard()
text_eq_instructions = visual.TextStim(win=win, name='text_eq_instructions',
    text='Awesome! Now on to the final task. \n\nYou are going to see a series of statements in English. \n\nPlease read each statement carefully and rate how strongly you agree or disagree with it by using your mouse to click on the line. \n\nThere are no right or wrong answers, or trick questions.\n\n(press the spacebar to begin)',
    font='Arial',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "trial_eq"
trial_eqClock = core.Clock()
text_eq_question_trial = visual.TextStim(win=win, name='text_eq_question_trial',
    text='default text',
    font='Arial',
    pos=(0, 0.2), height=0.06, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
slider_eq_trial = visual.Slider(win=win, name='slider_eq_trial',
    size=(0.8, 0.02), pos=(0, -0.2), units=None,
    labels=['strongly agree', 'slightly agree', 'slightly disagree', 'strongly disagree'], ticks=None,
    granularity=1, style=['rating', 'triangleMarker'],
    color='LightGray', font='HelveticaBold',
    flip=False, depth=-1)
key_resp_eq_trial = keyboard.Keyboard()

# Initialize components for Routine "end_exp"
end_expClock = core.Clock()

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# set up handler to look after randomisation of conditions etc
instructions_2afc_loop = data.TrialHandler(nReps=1, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('instructions/2afc_instructions_text.xlsx'),
    seed=None, name='instructions_2afc_loop')
thisExp.addLoop(instructions_2afc_loop)  # add the loop to the experiment
thisInstructions_2afc_loop = instructions_2afc_loop.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisInstructions_2afc_loop.rgb)
if thisInstructions_2afc_loop != None:
    for paramName in thisInstructions_2afc_loop:
        exec('{} = thisInstructions_2afc_loop[paramName]'.format(paramName))

for thisInstructions_2afc_loop in instructions_2afc_loop:
    currentLoop = instructions_2afc_loop
    # abbreviate parameter names if possible (e.g. rgb = thisInstructions_2afc_loop.rgb)
    if thisInstructions_2afc_loop != None:
        for paramName in thisInstructions_2afc_loop:
            exec('{} = thisInstructions_2afc_loop[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "instructions_2afc"-------
    continueRoutine = True
    # update component parameters for each repeat
    text_2afc_instruction.setText(instructions_text)
    text_2afc_instructions_continue.setText(continue_text)
    key_resp_2afc_instructions.keys = []
    key_resp_2afc_instructions.rt = []
    _key_resp_2afc_instructions_allKeys = []
    # keep track of which components have finished
    instructions_2afcComponents = [text_2afc_instruction, text_2afc_instructions_continue, key_resp_2afc_instructions]
    for thisComponent in instructions_2afcComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    instructions_2afcClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "instructions_2afc"-------
    while continueRoutine:
        # get current time
        t = instructions_2afcClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=instructions_2afcClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_2afc_instruction* updates
        if text_2afc_instruction.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_2afc_instruction.frameNStart = frameN  # exact frame index
            text_2afc_instruction.tStart = t  # local t and not account for scr refresh
            text_2afc_instruction.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_2afc_instruction, 'tStartRefresh')  # time at next scr refresh
            text_2afc_instruction.setAutoDraw(True)
        
        # *text_2afc_instructions_continue* updates
        if text_2afc_instructions_continue.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_2afc_instructions_continue.frameNStart = frameN  # exact frame index
            text_2afc_instructions_continue.tStart = t  # local t and not account for scr refresh
            text_2afc_instructions_continue.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_2afc_instructions_continue, 'tStartRefresh')  # time at next scr refresh
            text_2afc_instructions_continue.setAutoDraw(True)
        
        # *key_resp_2afc_instructions* updates
        if key_resp_2afc_instructions.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp_2afc_instructions.frameNStart = frameN  # exact frame index
            key_resp_2afc_instructions.tStart = t  # local t and not account for scr refresh
            key_resp_2afc_instructions.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_2afc_instructions, 'tStartRefresh')  # time at next scr refresh
            key_resp_2afc_instructions.status = STARTED
            # keyboard checking is just starting
            key_resp_2afc_instructions.clock.reset()  # now t=0
            key_resp_2afc_instructions.clearEvents(eventType='keyboard')
        if key_resp_2afc_instructions.status == STARTED:
            theseKeys = key_resp_2afc_instructions.getKeys(keyList=['c', 'space'], waitRelease=False)
            _key_resp_2afc_instructions_allKeys.extend(theseKeys)
            if len(_key_resp_2afc_instructions_allKeys):
                key_resp_2afc_instructions.keys = _key_resp_2afc_instructions_allKeys[-1].name  # just the last key pressed
                key_resp_2afc_instructions.rt = _key_resp_2afc_instructions_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in instructions_2afcComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "instructions_2afc"-------
    for thisComponent in instructions_2afcComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "instructions_2afc" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'instructions_2afc_loop'


# set up handler to look after randomisation of conditions etc
trials_2afc_practice_loop = data.TrialHandler(nReps=1, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('trials/twoafc_practice_trials.xlsx'),
    seed=None, name='trials_2afc_practice_loop')
thisExp.addLoop(trials_2afc_practice_loop)  # add the loop to the experiment
thisTrials_2afc_practice_loop = trials_2afc_practice_loop.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrials_2afc_practice_loop.rgb)
if thisTrials_2afc_practice_loop != None:
    for paramName in thisTrials_2afc_practice_loop:
        exec('{} = thisTrials_2afc_practice_loop[paramName]'.format(paramName))

for thisTrials_2afc_practice_loop in trials_2afc_practice_loop:
    currentLoop = trials_2afc_practice_loop
    # abbreviate parameter names if possible (e.g. rgb = thisTrials_2afc_practice_loop.rgb)
    if thisTrials_2afc_practice_loop != None:
        for paramName in thisTrials_2afc_practice_loop:
            exec('{} = thisTrials_2afc_practice_loop[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "practice_2afc"-------
    continueRoutine = True
    # update component parameters for each repeat
    sound_2afc_practice_stim.setSound(path, hamming=True)
    sound_2afc_practice_stim.setVolume(1, log=False)
    key_resp_2afc_practice.keys = []
    key_resp_2afc_practice.rt = []
    _key_resp_2afc_practice_allKeys = []
    # keep track of which components have finished
    practice_2afcComponents = [sound_2afc_practice_stim, text_2afc_response_yes_practice, text_2afc_response_no_practice, key_resp_2afc_practice, text_2afc_question_practice]
    for thisComponent in practice_2afcComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    practice_2afcClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "practice_2afc"-------
    while continueRoutine:
        # get current time
        t = practice_2afcClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=practice_2afcClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        # start/stop sound_2afc_practice_stim
        if sound_2afc_practice_stim.status == NOT_STARTED and tThisFlip >= 0.500-frameTolerance:
            # keep track of start time/frame for later
            sound_2afc_practice_stim.frameNStart = frameN  # exact frame index
            sound_2afc_practice_stim.tStart = t  # local t and not account for scr refresh
            sound_2afc_practice_stim.tStartRefresh = tThisFlipGlobal  # on global time
            sound_2afc_practice_stim.play(when=win)  # sync with win flip
        
        # *text_2afc_response_yes_practice* updates
        if text_2afc_response_yes_practice.status == NOT_STARTED and tThisFlip >= 0.250-frameTolerance:
            # keep track of start time/frame for later
            text_2afc_response_yes_practice.frameNStart = frameN  # exact frame index
            text_2afc_response_yes_practice.tStart = t  # local t and not account for scr refresh
            text_2afc_response_yes_practice.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_2afc_response_yes_practice, 'tStartRefresh')  # time at next scr refresh
            text_2afc_response_yes_practice.setAutoDraw(True)
        
        # *text_2afc_response_no_practice* updates
        if text_2afc_response_no_practice.status == NOT_STARTED and tThisFlip >= 0.250-frameTolerance:
            # keep track of start time/frame for later
            text_2afc_response_no_practice.frameNStart = frameN  # exact frame index
            text_2afc_response_no_practice.tStart = t  # local t and not account for scr refresh
            text_2afc_response_no_practice.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_2afc_response_no_practice, 'tStartRefresh')  # time at next scr refresh
            text_2afc_response_no_practice.setAutoDraw(True)
        
        # *key_resp_2afc_practice* updates
        waitOnFlip = False
        if key_resp_2afc_practice.status == NOT_STARTED and tThisFlip >= 0.500-frameTolerance:
            # keep track of start time/frame for later
            key_resp_2afc_practice.frameNStart = frameN  # exact frame index
            key_resp_2afc_practice.tStart = t  # local t and not account for scr refresh
            key_resp_2afc_practice.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_2afc_practice, 'tStartRefresh')  # time at next scr refresh
            key_resp_2afc_practice.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_2afc_practice.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_2afc_practice.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_2afc_practice.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_2afc_practice.getKeys(keyList=['1', '0'], waitRelease=False)
            _key_resp_2afc_practice_allKeys.extend(theseKeys)
            if len(_key_resp_2afc_practice_allKeys):
                key_resp_2afc_practice.keys = _key_resp_2afc_practice_allKeys[0].name  # just the first key pressed
                key_resp_2afc_practice.rt = _key_resp_2afc_practice_allKeys[0].rt
                # was this correct?
                if (key_resp_2afc_practice.keys == str(correct_response)) or (key_resp_2afc_practice.keys == correct_response):
                    key_resp_2afc_practice.corr = 1
                else:
                    key_resp_2afc_practice.corr = 0
                # a response ends the routine
                continueRoutine = False
        
        # *text_2afc_question_practice* updates
        if text_2afc_question_practice.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_2afc_question_practice.frameNStart = frameN  # exact frame index
            text_2afc_question_practice.tStart = t  # local t and not account for scr refresh
            text_2afc_question_practice.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_2afc_question_practice, 'tStartRefresh')  # time at next scr refresh
            text_2afc_question_practice.setAutoDraw(True)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in practice_2afcComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "practice_2afc"-------
    for thisComponent in practice_2afcComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    sound_2afc_practice_stim.stop()  # ensure sound has stopped at end of routine
    trials_2afc_practice_loop.addData('sound_2afc_practice_stim.started', sound_2afc_practice_stim.tStartRefresh)
    trials_2afc_practice_loop.addData('sound_2afc_practice_stim.stopped', sound_2afc_practice_stim.tStopRefresh)
    trials_2afc_practice_loop.addData('text_2afc_response_yes_practice.started', text_2afc_response_yes_practice.tStartRefresh)
    trials_2afc_practice_loop.addData('text_2afc_response_yes_practice.stopped', text_2afc_response_yes_practice.tStopRefresh)
    trials_2afc_practice_loop.addData('text_2afc_response_no_practice.started', text_2afc_response_no_practice.tStartRefresh)
    trials_2afc_practice_loop.addData('text_2afc_response_no_practice.stopped', text_2afc_response_no_practice.tStopRefresh)
    # check responses
    if key_resp_2afc_practice.keys in ['', [], None]:  # No response was made
        key_resp_2afc_practice.keys = None
        # was no response the correct answer?!
        if str(correct_response).lower() == 'none':
           key_resp_2afc_practice.corr = 1;  # correct non-response
        else:
           key_resp_2afc_practice.corr = 0;  # failed to respond (incorrectly)
    # store data for trials_2afc_practice_loop (TrialHandler)
    trials_2afc_practice_loop.addData('key_resp_2afc_practice.keys',key_resp_2afc_practice.keys)
    trials_2afc_practice_loop.addData('key_resp_2afc_practice.corr', key_resp_2afc_practice.corr)
    if key_resp_2afc_practice.keys != None:  # we had a response
        trials_2afc_practice_loop.addData('key_resp_2afc_practice.rt', key_resp_2afc_practice.rt)
    trials_2afc_practice_loop.addData('key_resp_2afc_practice.started', key_resp_2afc_practice.tStartRefresh)
    trials_2afc_practice_loop.addData('key_resp_2afc_practice.stopped', key_resp_2afc_practice.tStopRefresh)
    trials_2afc_practice_loop.addData('text_2afc_question_practice.started', text_2afc_question_practice.tStartRefresh)
    trials_2afc_practice_loop.addData('text_2afc_question_practice.stopped', text_2afc_question_practice.tStopRefresh)
    # the Routine "practice_2afc" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'trials_2afc_practice_loop'


# ------Prepare to start Routine "check_2afc"-------
continueRoutine = True
# update component parameters for each repeat
key_resp_2afc_gotit.keys = []
key_resp_2afc_gotit.rt = []
_key_resp_2afc_gotit_allKeys = []
# keep track of which components have finished
check_2afcComponents = [text_2afc_check, key_resp_2afc_gotit]
for thisComponent in check_2afcComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
check_2afcClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "check_2afc"-------
while continueRoutine:
    # get current time
    t = check_2afcClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=check_2afcClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_2afc_check* updates
    if text_2afc_check.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_2afc_check.frameNStart = frameN  # exact frame index
        text_2afc_check.tStart = t  # local t and not account for scr refresh
        text_2afc_check.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_2afc_check, 'tStartRefresh')  # time at next scr refresh
        text_2afc_check.setAutoDraw(True)
    
    # *key_resp_2afc_gotit* updates
    waitOnFlip = False
    if key_resp_2afc_gotit.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_2afc_gotit.frameNStart = frameN  # exact frame index
        key_resp_2afc_gotit.tStart = t  # local t and not account for scr refresh
        key_resp_2afc_gotit.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_2afc_gotit, 'tStartRefresh')  # time at next scr refresh
        key_resp_2afc_gotit.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_2afc_gotit.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_2afc_gotit.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_2afc_gotit.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_2afc_gotit.getKeys(keyList=['c', 'space'], waitRelease=False)
        _key_resp_2afc_gotit_allKeys.extend(theseKeys)
        if len(_key_resp_2afc_gotit_allKeys):
            key_resp_2afc_gotit.keys = _key_resp_2afc_gotit_allKeys[-1].name  # just the last key pressed
            key_resp_2afc_gotit.rt = _key_resp_2afc_gotit_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in check_2afcComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "check_2afc"-------
for thisComponent in check_2afcComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('text_2afc_check.started', text_2afc_check.tStartRefresh)
thisExp.addData('text_2afc_check.stopped', text_2afc_check.tStopRefresh)
# the Routine "check_2afc" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
trials_2afc_loop = data.TrialHandler(nReps=1, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('trials/twoafc_trials.xlsx'),
    seed=None, name='trials_2afc_loop')
thisExp.addLoop(trials_2afc_loop)  # add the loop to the experiment
thisTrials_2afc_loop = trials_2afc_loop.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrials_2afc_loop.rgb)
if thisTrials_2afc_loop != None:
    for paramName in thisTrials_2afc_loop:
        exec('{} = thisTrials_2afc_loop[paramName]'.format(paramName))

for thisTrials_2afc_loop in trials_2afc_loop:
    currentLoop = trials_2afc_loop
    # abbreviate parameter names if possible (e.g. rgb = thisTrials_2afc_loop.rgb)
    if thisTrials_2afc_loop != None:
        for paramName in thisTrials_2afc_loop:
            exec('{} = thisTrials_2afc_loop[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "trial_2afc"-------
    continueRoutine = True
    # update component parameters for each repeat
    the_col = np.random.choice(col_name, p=[0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125])
    sound_2afc_stim_trial.setSound(eval(the_col), hamming=True)
    sound_2afc_stim_trial.setVolume(1, log=False)
    key_resp_2afc_trial.keys = []
    key_resp_2afc_trial.rt = []
    _key_resp_2afc_trial_allKeys = []
    # keep track of which components have finished
    trial_2afcComponents = [sound_2afc_stim_trial, text_2afc_response_yes_trial, text_2afc_response_no_trial, key_resp_2afc_trial, text_2afc_question_trial]
    for thisComponent in trial_2afcComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    trial_2afcClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "trial_2afc"-------
    while continueRoutine:
        # get current time
        t = trial_2afcClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=trial_2afcClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        # start/stop sound_2afc_stim_trial
        if sound_2afc_stim_trial.status == NOT_STARTED and tThisFlip >= 0.500-frameTolerance:
            # keep track of start time/frame for later
            sound_2afc_stim_trial.frameNStart = frameN  # exact frame index
            sound_2afc_stim_trial.tStart = t  # local t and not account for scr refresh
            sound_2afc_stim_trial.tStartRefresh = tThisFlipGlobal  # on global time
            sound_2afc_stim_trial.play(when=win)  # sync with win flip
        
        # *text_2afc_response_yes_trial* updates
        if text_2afc_response_yes_trial.status == NOT_STARTED and tThisFlip >= 0.250-frameTolerance:
            # keep track of start time/frame for later
            text_2afc_response_yes_trial.frameNStart = frameN  # exact frame index
            text_2afc_response_yes_trial.tStart = t  # local t and not account for scr refresh
            text_2afc_response_yes_trial.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_2afc_response_yes_trial, 'tStartRefresh')  # time at next scr refresh
            text_2afc_response_yes_trial.setAutoDraw(True)
        
        # *text_2afc_response_no_trial* updates
        if text_2afc_response_no_trial.status == NOT_STARTED and tThisFlip >= 0.250-frameTolerance:
            # keep track of start time/frame for later
            text_2afc_response_no_trial.frameNStart = frameN  # exact frame index
            text_2afc_response_no_trial.tStart = t  # local t and not account for scr refresh
            text_2afc_response_no_trial.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_2afc_response_no_trial, 'tStartRefresh')  # time at next scr refresh
            text_2afc_response_no_trial.setAutoDraw(True)
        
        # *key_resp_2afc_trial* updates
        waitOnFlip = False
        if key_resp_2afc_trial.status == NOT_STARTED and tThisFlip >= 0.500-frameTolerance:
            # keep track of start time/frame for later
            key_resp_2afc_trial.frameNStart = frameN  # exact frame index
            key_resp_2afc_trial.tStart = t  # local t and not account for scr refresh
            key_resp_2afc_trial.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_2afc_trial, 'tStartRefresh')  # time at next scr refresh
            key_resp_2afc_trial.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_2afc_trial.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_2afc_trial.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_2afc_trial.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_2afc_trial.getKeys(keyList=['1', '0'], waitRelease=False)
            _key_resp_2afc_trial_allKeys.extend(theseKeys)
            if len(_key_resp_2afc_trial_allKeys):
                key_resp_2afc_trial.keys = _key_resp_2afc_trial_allKeys[0].name  # just the first key pressed
                key_resp_2afc_trial.rt = _key_resp_2afc_trial_allKeys[0].rt
                # was this correct?
                if (key_resp_2afc_trial.keys == str(correct_response)) or (key_resp_2afc_trial.keys == correct_response):
                    key_resp_2afc_trial.corr = 1
                else:
                    key_resp_2afc_trial.corr = 0
                # a response ends the routine
                continueRoutine = False
        
        # *text_2afc_question_trial* updates
        if text_2afc_question_trial.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_2afc_question_trial.frameNStart = frameN  # exact frame index
            text_2afc_question_trial.tStart = t  # local t and not account for scr refresh
            text_2afc_question_trial.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_2afc_question_trial, 'tStartRefresh')  # time at next scr refresh
            text_2afc_question_trial.setAutoDraw(True)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in trial_2afcComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "trial_2afc"-------
    for thisComponent in trial_2afcComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials_2afc_loop.addData('the_col', the_col)
    sound_2afc_stim_trial.stop()  # ensure sound has stopped at end of routine
    trials_2afc_loop.addData('sound_2afc_stim_trial.started', sound_2afc_stim_trial.tStartRefresh)
    trials_2afc_loop.addData('sound_2afc_stim_trial.stopped', sound_2afc_stim_trial.tStopRefresh)
    trials_2afc_loop.addData('text_2afc_response_yes_trial.started', text_2afc_response_yes_trial.tStartRefresh)
    trials_2afc_loop.addData('text_2afc_response_yes_trial.stopped', text_2afc_response_yes_trial.tStopRefresh)
    trials_2afc_loop.addData('text_2afc_response_no_trial.started', text_2afc_response_no_trial.tStartRefresh)
    trials_2afc_loop.addData('text_2afc_response_no_trial.stopped', text_2afc_response_no_trial.tStopRefresh)
    # check responses
    if key_resp_2afc_trial.keys in ['', [], None]:  # No response was made
        key_resp_2afc_trial.keys = None
        # was no response the correct answer?!
        if str(correct_response).lower() == 'none':
           key_resp_2afc_trial.corr = 1;  # correct non-response
        else:
           key_resp_2afc_trial.corr = 0;  # failed to respond (incorrectly)
    # store data for trials_2afc_loop (TrialHandler)
    trials_2afc_loop.addData('key_resp_2afc_trial.keys',key_resp_2afc_trial.keys)
    trials_2afc_loop.addData('key_resp_2afc_trial.corr', key_resp_2afc_trial.corr)
    if key_resp_2afc_trial.keys != None:  # we had a response
        trials_2afc_loop.addData('key_resp_2afc_trial.rt', key_resp_2afc_trial.rt)
    trials_2afc_loop.addData('key_resp_2afc_trial.started', key_resp_2afc_trial.tStartRefresh)
    trials_2afc_loop.addData('key_resp_2afc_trial.stopped', key_resp_2afc_trial.tStopRefresh)
    trials_2afc_loop.addData('text_2afc_question_trial.started', text_2afc_question_trial.tStartRefresh)
    trials_2afc_loop.addData('text_2afc_question_trial.stopped', text_2afc_question_trial.tStopRefresh)
    # the Routine "trial_2afc" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'trials_2afc_loop'


# set up handler to look after randomisation of conditions etc
instructions_lextale_loop = data.TrialHandler(nReps=1, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('instructions/lextale_instructions_text.xlsx'),
    seed=None, name='instructions_lextale_loop')
thisExp.addLoop(instructions_lextale_loop)  # add the loop to the experiment
thisInstructions_lextale_loop = instructions_lextale_loop.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisInstructions_lextale_loop.rgb)
if thisInstructions_lextale_loop != None:
    for paramName in thisInstructions_lextale_loop:
        exec('{} = thisInstructions_lextale_loop[paramName]'.format(paramName))

for thisInstructions_lextale_loop in instructions_lextale_loop:
    currentLoop = instructions_lextale_loop
    # abbreviate parameter names if possible (e.g. rgb = thisInstructions_lextale_loop.rgb)
    if thisInstructions_lextale_loop != None:
        for paramName in thisInstructions_lextale_loop:
            exec('{} = thisInstructions_lextale_loop[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "instructions_lextale"-------
    continueRoutine = True
    # update component parameters for each repeat
    text_lextale_instructions.setText(instructions_text)
    key_resp_lextale_instructions.keys = []
    key_resp_lextale_instructions.rt = []
    _key_resp_lextale_instructions_allKeys = []
    text_lextale_instructions_continue.setText(continue_text)
    # keep track of which components have finished
    instructions_lextaleComponents = [text_lextale_instructions, key_resp_lextale_instructions, text_lextale_instructions_continue]
    for thisComponent in instructions_lextaleComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    instructions_lextaleClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "instructions_lextale"-------
    while continueRoutine:
        # get current time
        t = instructions_lextaleClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=instructions_lextaleClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_lextale_instructions* updates
        if text_lextale_instructions.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_lextale_instructions.frameNStart = frameN  # exact frame index
            text_lextale_instructions.tStart = t  # local t and not account for scr refresh
            text_lextale_instructions.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_lextale_instructions, 'tStartRefresh')  # time at next scr refresh
            text_lextale_instructions.setAutoDraw(True)
        
        # *key_resp_lextale_instructions* updates
        waitOnFlip = False
        if key_resp_lextale_instructions.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp_lextale_instructions.frameNStart = frameN  # exact frame index
            key_resp_lextale_instructions.tStart = t  # local t and not account for scr refresh
            key_resp_lextale_instructions.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_lextale_instructions, 'tStartRefresh')  # time at next scr refresh
            key_resp_lextale_instructions.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_lextale_instructions.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_lextale_instructions.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_lextale_instructions.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_lextale_instructions.getKeys(keyList=['space'], waitRelease=False)
            _key_resp_lextale_instructions_allKeys.extend(theseKeys)
            if len(_key_resp_lextale_instructions_allKeys):
                key_resp_lextale_instructions.keys = _key_resp_lextale_instructions_allKeys[-1].name  # just the last key pressed
                key_resp_lextale_instructions.rt = _key_resp_lextale_instructions_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # *text_lextale_instructions_continue* updates
        if text_lextale_instructions_continue.status == NOT_STARTED and tThisFlip >= 2.5-frameTolerance:
            # keep track of start time/frame for later
            text_lextale_instructions_continue.frameNStart = frameN  # exact frame index
            text_lextale_instructions_continue.tStart = t  # local t and not account for scr refresh
            text_lextale_instructions_continue.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_lextale_instructions_continue, 'tStartRefresh')  # time at next scr refresh
            text_lextale_instructions_continue.setAutoDraw(True)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in instructions_lextaleComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "instructions_lextale"-------
    for thisComponent in instructions_lextaleComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "instructions_lextale" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'instructions_lextale_loop'


# set up handler to look after randomisation of conditions etc
trials_lextale_practice_loop = data.TrialHandler(nReps=1, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('trials/lextale_practice_trials.csv'),
    seed=None, name='trials_lextale_practice_loop')
thisExp.addLoop(trials_lextale_practice_loop)  # add the loop to the experiment
thisTrials_lextale_practice_loop = trials_lextale_practice_loop.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrials_lextale_practice_loop.rgb)
if thisTrials_lextale_practice_loop != None:
    for paramName in thisTrials_lextale_practice_loop:
        exec('{} = thisTrials_lextale_practice_loop[paramName]'.format(paramName))

for thisTrials_lextale_practice_loop in trials_lextale_practice_loop:
    currentLoop = trials_lextale_practice_loop
    # abbreviate parameter names if possible (e.g. rgb = thisTrials_lextale_practice_loop.rgb)
    if thisTrials_lextale_practice_loop != None:
        for paramName in thisTrials_lextale_practice_loop:
            exec('{} = thisTrials_lextale_practice_loop[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "practice_lextale"-------
    continueRoutine = True
    # update component parameters for each repeat
    text_lextale_practice_word.setText(word)
    key_resp_lextale_practice.keys = []
    key_resp_lextale_practice.rt = []
    _key_resp_lextale_practice_allKeys = []
    text_lextale_practice_real_label.setText(prac_button_real)
    text_lextale_practice_false_label.setText(prac_button_false)
    # keep track of which components have finished
    practice_lextaleComponents = [text_lextale_practice_label, text_lextale_practice_word, key_resp_lextale_practice, text_lextale_practice_real_label, text_lextale_practice_false_label]
    for thisComponent in practice_lextaleComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    practice_lextaleClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "practice_lextale"-------
    while continueRoutine:
        # get current time
        t = practice_lextaleClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=practice_lextaleClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_lextale_practice_label* updates
        if text_lextale_practice_label.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_lextale_practice_label.frameNStart = frameN  # exact frame index
            text_lextale_practice_label.tStart = t  # local t and not account for scr refresh
            text_lextale_practice_label.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_lextale_practice_label, 'tStartRefresh')  # time at next scr refresh
            text_lextale_practice_label.setAutoDraw(True)
        
        # *text_lextale_practice_word* updates
        if text_lextale_practice_word.status == NOT_STARTED and tThisFlip >= 0.5-frameTolerance:
            # keep track of start time/frame for later
            text_lextale_practice_word.frameNStart = frameN  # exact frame index
            text_lextale_practice_word.tStart = t  # local t and not account for scr refresh
            text_lextale_practice_word.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_lextale_practice_word, 'tStartRefresh')  # time at next scr refresh
            text_lextale_practice_word.setAutoDraw(True)
        
        # *key_resp_lextale_practice* updates
        waitOnFlip = False
        if key_resp_lextale_practice.status == NOT_STARTED and tThisFlip >= 0.5-frameTolerance:
            # keep track of start time/frame for later
            key_resp_lextale_practice.frameNStart = frameN  # exact frame index
            key_resp_lextale_practice.tStart = t  # local t and not account for scr refresh
            key_resp_lextale_practice.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_lextale_practice, 'tStartRefresh')  # time at next scr refresh
            key_resp_lextale_practice.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_lextale_practice.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_lextale_practice.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_lextale_practice.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_lextale_practice.getKeys(keyList=['1', '0'], waitRelease=False)
            _key_resp_lextale_practice_allKeys.extend(theseKeys)
            if len(_key_resp_lextale_practice_allKeys):
                key_resp_lextale_practice.keys = _key_resp_lextale_practice_allKeys[0].name  # just the first key pressed
                key_resp_lextale_practice.rt = _key_resp_lextale_practice_allKeys[0].rt
                # was this correct?
                if (key_resp_lextale_practice.keys == str(correct_response)) or (key_resp_lextale_practice.keys == correct_response):
                    key_resp_lextale_practice.corr = 1
                else:
                    key_resp_lextale_practice.corr = 0
                # a response ends the routine
                continueRoutine = False
        
        # *text_lextale_practice_real_label* updates
        if text_lextale_practice_real_label.status == NOT_STARTED and tThisFlip >= 0.25-frameTolerance:
            # keep track of start time/frame for later
            text_lextale_practice_real_label.frameNStart = frameN  # exact frame index
            text_lextale_practice_real_label.tStart = t  # local t and not account for scr refresh
            text_lextale_practice_real_label.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_lextale_practice_real_label, 'tStartRefresh')  # time at next scr refresh
            text_lextale_practice_real_label.setAutoDraw(True)
        
        # *text_lextale_practice_false_label* updates
        if text_lextale_practice_false_label.status == NOT_STARTED and tThisFlip >= 0.25-frameTolerance:
            # keep track of start time/frame for later
            text_lextale_practice_false_label.frameNStart = frameN  # exact frame index
            text_lextale_practice_false_label.tStart = t  # local t and not account for scr refresh
            text_lextale_practice_false_label.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_lextale_practice_false_label, 'tStartRefresh')  # time at next scr refresh
            text_lextale_practice_false_label.setAutoDraw(True)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in practice_lextaleComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "practice_lextale"-------
    for thisComponent in practice_lextaleComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if key_resp_lextale_practice.keys in ['', [], None]:  # No response was made
        key_resp_lextale_practice.keys = None
        # was no response the correct answer?!
        if str(correct_response).lower() == 'none':
           key_resp_lextale_practice.corr = 1;  # correct non-response
        else:
           key_resp_lextale_practice.corr = 0;  # failed to respond (incorrectly)
    # store data for trials_lextale_practice_loop (TrialHandler)
    trials_lextale_practice_loop.addData('key_resp_lextale_practice.keys',key_resp_lextale_practice.keys)
    trials_lextale_practice_loop.addData('key_resp_lextale_practice.corr', key_resp_lextale_practice.corr)
    if key_resp_lextale_practice.keys != None:  # we had a response
        trials_lextale_practice_loop.addData('key_resp_lextale_practice.rt', key_resp_lextale_practice.rt)
    # the Routine "practice_lextale" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "practice_feedback_lextale"-------
    continueRoutine = True
    routineTimer.add(1.000000)
    # update component parameters for each repeat
    if key_resp_lextale_practice.corr == 1:
        msg = "Correct! That's a real word."
    else:
        msg = "Ooo! That's not a real word."
    text_lextale_practice_feedback.setText(msg)
    key_resp_lextale_practice_feedback.keys = []
    key_resp_lextale_practice_feedback.rt = []
    _key_resp_lextale_practice_feedback_allKeys = []
    # keep track of which components have finished
    practice_feedback_lextaleComponents = [text_lextale_practice_feedback, key_resp_lextale_practice_feedback]
    for thisComponent in practice_feedback_lextaleComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    practice_feedback_lextaleClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "practice_feedback_lextale"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = practice_feedback_lextaleClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=practice_feedback_lextaleClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_lextale_practice_feedback* updates
        if text_lextale_practice_feedback.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_lextale_practice_feedback.frameNStart = frameN  # exact frame index
            text_lextale_practice_feedback.tStart = t  # local t and not account for scr refresh
            text_lextale_practice_feedback.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_lextale_practice_feedback, 'tStartRefresh')  # time at next scr refresh
            text_lextale_practice_feedback.setAutoDraw(True)
        if text_lextale_practice_feedback.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > text_lextale_practice_feedback.tStartRefresh + 1.0-frameTolerance:
                # keep track of stop time/frame for later
                text_lextale_practice_feedback.tStop = t  # not accounting for scr refresh
                text_lextale_practice_feedback.frameNStop = frameN  # exact frame index
                win.timeOnFlip(text_lextale_practice_feedback, 'tStopRefresh')  # time at next scr refresh
                text_lextale_practice_feedback.setAutoDraw(False)
        
        # *key_resp_lextale_practice_feedback* updates
        waitOnFlip = False
        if key_resp_lextale_practice_feedback.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp_lextale_practice_feedback.frameNStart = frameN  # exact frame index
            key_resp_lextale_practice_feedback.tStart = t  # local t and not account for scr refresh
            key_resp_lextale_practice_feedback.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_lextale_practice_feedback, 'tStartRefresh')  # time at next scr refresh
            key_resp_lextale_practice_feedback.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_lextale_practice_feedback.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_lextale_practice_feedback.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_lextale_practice_feedback.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > key_resp_lextale_practice_feedback.tStartRefresh + 1.0-frameTolerance:
                # keep track of stop time/frame for later
                key_resp_lextale_practice_feedback.tStop = t  # not accounting for scr refresh
                key_resp_lextale_practice_feedback.frameNStop = frameN  # exact frame index
                win.timeOnFlip(key_resp_lextale_practice_feedback, 'tStopRefresh')  # time at next scr refresh
                key_resp_lextale_practice_feedback.status = FINISHED
        if key_resp_lextale_practice_feedback.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_lextale_practice_feedback.getKeys(keyList=['space'], waitRelease=False)
            _key_resp_lextale_practice_feedback_allKeys.extend(theseKeys)
            if len(_key_resp_lextale_practice_feedback_allKeys):
                key_resp_lextale_practice_feedback.keys = _key_resp_lextale_practice_feedback_allKeys[-1].name  # just the last key pressed
                key_resp_lextale_practice_feedback.rt = _key_resp_lextale_practice_feedback_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in practice_feedback_lextaleComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "practice_feedback_lextale"-------
    for thisComponent in practice_feedback_lextaleComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    thisExp.nextEntry()
    
# completed 1 repeats of 'trials_lextale_practice_loop'


# ------Prepare to start Routine "check_lextale"-------
continueRoutine = True
# update component parameters for each repeat
key_resp_lextale_check.keys = []
key_resp_lextale_check.rt = []
_key_resp_lextale_check_allKeys = []
# keep track of which components have finished
check_lextaleComponents = [text_lextale_check, key_resp_lextale_check]
for thisComponent in check_lextaleComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
check_lextaleClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "check_lextale"-------
while continueRoutine:
    # get current time
    t = check_lextaleClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=check_lextaleClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_lextale_check* updates
    if text_lextale_check.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_lextale_check.frameNStart = frameN  # exact frame index
        text_lextale_check.tStart = t  # local t and not account for scr refresh
        text_lextale_check.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_lextale_check, 'tStartRefresh')  # time at next scr refresh
        text_lextale_check.setAutoDraw(True)
    
    # *key_resp_lextale_check* updates
    waitOnFlip = False
    if key_resp_lextale_check.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_lextale_check.frameNStart = frameN  # exact frame index
        key_resp_lextale_check.tStart = t  # local t and not account for scr refresh
        key_resp_lextale_check.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_lextale_check, 'tStartRefresh')  # time at next scr refresh
        key_resp_lextale_check.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_lextale_check.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_lextale_check.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_lextale_check.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_lextale_check.getKeys(keyList=['space'], waitRelease=False)
        _key_resp_lextale_check_allKeys.extend(theseKeys)
        if len(_key_resp_lextale_check_allKeys):
            key_resp_lextale_check.keys = _key_resp_lextale_check_allKeys[-1].name  # just the last key pressed
            key_resp_lextale_check.rt = _key_resp_lextale_check_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in check_lextaleComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "check_lextale"-------
for thisComponent in check_lextaleComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# the Routine "check_lextale" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
trials_lextale_loop = data.TrialHandler(nReps=1, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('trials/lextale_trials.xlsx'),
    seed=None, name='trials_lextale_loop')
thisExp.addLoop(trials_lextale_loop)  # add the loop to the experiment
thisTrials_lextale_loop = trials_lextale_loop.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrials_lextale_loop.rgb)
if thisTrials_lextale_loop != None:
    for paramName in thisTrials_lextale_loop:
        exec('{} = thisTrials_lextale_loop[paramName]'.format(paramName))

for thisTrials_lextale_loop in trials_lextale_loop:
    currentLoop = trials_lextale_loop
    # abbreviate parameter names if possible (e.g. rgb = thisTrials_lextale_loop.rgb)
    if thisTrials_lextale_loop != None:
        for paramName in thisTrials_lextale_loop:
            exec('{} = thisTrials_lextale_loop[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "trial_lextale"-------
    continueRoutine = True
    # update component parameters for each repeat
    text_lextale_trial_word.setText(word)
    key_resp_lextale_trial.keys = []
    key_resp_lextale_trial.rt = []
    _key_resp_lextale_trial_allKeys = []
    text_lextale_trial_real_label.setText(button_real)
    text_lextale_trial_false_label.setText(button_false)
    # keep track of which components have finished
    trial_lextaleComponents = [text_lextale_trial_word, key_resp_lextale_trial, text_lextale_trial_real_label, text_lextale_trial_false_label]
    for thisComponent in trial_lextaleComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    trial_lextaleClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "trial_lextale"-------
    while continueRoutine:
        # get current time
        t = trial_lextaleClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=trial_lextaleClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_lextale_trial_word* updates
        if text_lextale_trial_word.status == NOT_STARTED and tThisFlip >= 0.5-frameTolerance:
            # keep track of start time/frame for later
            text_lextale_trial_word.frameNStart = frameN  # exact frame index
            text_lextale_trial_word.tStart = t  # local t and not account for scr refresh
            text_lextale_trial_word.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_lextale_trial_word, 'tStartRefresh')  # time at next scr refresh
            text_lextale_trial_word.setAutoDraw(True)
        
        # *key_resp_lextale_trial* updates
        waitOnFlip = False
        if key_resp_lextale_trial.status == NOT_STARTED and tThisFlip >= 0.5-frameTolerance:
            # keep track of start time/frame for later
            key_resp_lextale_trial.frameNStart = frameN  # exact frame index
            key_resp_lextale_trial.tStart = t  # local t and not account for scr refresh
            key_resp_lextale_trial.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_lextale_trial, 'tStartRefresh')  # time at next scr refresh
            key_resp_lextale_trial.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_lextale_trial.clock.reset)  # t=0 on next screen flip
        if key_resp_lextale_trial.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_lextale_trial.getKeys(keyList=['1', '0'], waitRelease=False)
            _key_resp_lextale_trial_allKeys.extend(theseKeys)
            if len(_key_resp_lextale_trial_allKeys):
                key_resp_lextale_trial.keys = _key_resp_lextale_trial_allKeys[0].name  # just the first key pressed
                key_resp_lextale_trial.rt = _key_resp_lextale_trial_allKeys[0].rt
                # was this correct?
                if (key_resp_lextale_trial.keys == str(correct_response)) or (key_resp_lextale_trial.keys == correct_response):
                    key_resp_lextale_trial.corr = 1
                else:
                    key_resp_lextale_trial.corr = 0
                # a response ends the routine
                continueRoutine = False
        
        # *text_lextale_trial_real_label* updates
        if text_lextale_trial_real_label.status == NOT_STARTED and tThisFlip >= 0.25-frameTolerance:
            # keep track of start time/frame for later
            text_lextale_trial_real_label.frameNStart = frameN  # exact frame index
            text_lextale_trial_real_label.tStart = t  # local t and not account for scr refresh
            text_lextale_trial_real_label.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_lextale_trial_real_label, 'tStartRefresh')  # time at next scr refresh
            text_lextale_trial_real_label.setAutoDraw(True)
        
        # *text_lextale_trial_false_label* updates
        if text_lextale_trial_false_label.status == NOT_STARTED and tThisFlip >= 0.25-frameTolerance:
            # keep track of start time/frame for later
            text_lextale_trial_false_label.frameNStart = frameN  # exact frame index
            text_lextale_trial_false_label.tStart = t  # local t and not account for scr refresh
            text_lextale_trial_false_label.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_lextale_trial_false_label, 'tStartRefresh')  # time at next scr refresh
            text_lextale_trial_false_label.setAutoDraw(True)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in trial_lextaleComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "trial_lextale"-------
    for thisComponent in trial_lextaleComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if key_resp_lextale_trial.keys in ['', [], None]:  # No response was made
        key_resp_lextale_trial.keys = None
        # was no response the correct answer?!
        if str(correct_response).lower() == 'none':
           key_resp_lextale_trial.corr = 1;  # correct non-response
        else:
           key_resp_lextale_trial.corr = 0;  # failed to respond (incorrectly)
    # store data for trials_lextale_loop (TrialHandler)
    trials_lextale_loop.addData('key_resp_lextale_trial.keys',key_resp_lextale_trial.keys)
    trials_lextale_loop.addData('key_resp_lextale_trial.corr', key_resp_lextale_trial.corr)
    if key_resp_lextale_trial.keys != None:  # we had a response
        trials_lextale_loop.addData('key_resp_lextale_trial.rt', key_resp_lextale_trial.rt)
    # the Routine "trial_lextale" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'trials_lextale_loop'


# ------Prepare to start Routine "instructions_eq"-------
continueRoutine = True
# update component parameters for each repeat
key_resp_eq_instructions.keys = []
key_resp_eq_instructions.rt = []
_key_resp_eq_instructions_allKeys = []
# keep track of which components have finished
instructions_eqComponents = [key_resp_eq_instructions, text_eq_instructions]
for thisComponent in instructions_eqComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
instructions_eqClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "instructions_eq"-------
while continueRoutine:
    # get current time
    t = instructions_eqClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=instructions_eqClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *key_resp_eq_instructions* updates
    waitOnFlip = False
    if key_resp_eq_instructions.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_eq_instructions.frameNStart = frameN  # exact frame index
        key_resp_eq_instructions.tStart = t  # local t and not account for scr refresh
        key_resp_eq_instructions.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_eq_instructions, 'tStartRefresh')  # time at next scr refresh
        key_resp_eq_instructions.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_eq_instructions.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_eq_instructions.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_eq_instructions.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_eq_instructions.getKeys(keyList=['space'], waitRelease=False)
        _key_resp_eq_instructions_allKeys.extend(theseKeys)
        if len(_key_resp_eq_instructions_allKeys):
            key_resp_eq_instructions.keys = _key_resp_eq_instructions_allKeys[-1].name  # just the last key pressed
            key_resp_eq_instructions.rt = _key_resp_eq_instructions_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # *text_eq_instructions* updates
    if text_eq_instructions.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_eq_instructions.frameNStart = frameN  # exact frame index
        text_eq_instructions.tStart = t  # local t and not account for scr refresh
        text_eq_instructions.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_eq_instructions, 'tStartRefresh')  # time at next scr refresh
        text_eq_instructions.setAutoDraw(True)
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in instructions_eqComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "instructions_eq"-------
for thisComponent in instructions_eqComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# the Routine "instructions_eq" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
trials_eq_loop = data.TrialHandler(nReps=1, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('trials/eq_trials.csv'),
    seed=None, name='trials_eq_loop')
thisExp.addLoop(trials_eq_loop)  # add the loop to the experiment
thisTrials_eq_loop = trials_eq_loop.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrials_eq_loop.rgb)
if thisTrials_eq_loop != None:
    for paramName in thisTrials_eq_loop:
        exec('{} = thisTrials_eq_loop[paramName]'.format(paramName))

for thisTrials_eq_loop in trials_eq_loop:
    currentLoop = trials_eq_loop
    # abbreviate parameter names if possible (e.g. rgb = thisTrials_eq_loop.rgb)
    if thisTrials_eq_loop != None:
        for paramName in thisTrials_eq_loop:
            exec('{} = thisTrials_eq_loop[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "trial_eq"-------
    continueRoutine = True
    # update component parameters for each repeat
    text_eq_question_trial.setText(item)
    slider_eq_trial.reset()
    key_resp_eq_trial.keys = []
    key_resp_eq_trial.rt = []
    _key_resp_eq_trial_allKeys = []
    # keep track of which components have finished
    trial_eqComponents = [text_eq_question_trial, slider_eq_trial, key_resp_eq_trial]
    for thisComponent in trial_eqComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    trial_eqClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "trial_eq"-------
    while continueRoutine:
        # get current time
        t = trial_eqClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=trial_eqClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_eq_question_trial* updates
        if text_eq_question_trial.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_eq_question_trial.frameNStart = frameN  # exact frame index
            text_eq_question_trial.tStart = t  # local t and not account for scr refresh
            text_eq_question_trial.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_eq_question_trial, 'tStartRefresh')  # time at next scr refresh
            text_eq_question_trial.setAutoDraw(True)
        
        # *slider_eq_trial* updates
        if slider_eq_trial.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            slider_eq_trial.frameNStart = frameN  # exact frame index
            slider_eq_trial.tStart = t  # local t and not account for scr refresh
            slider_eq_trial.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(slider_eq_trial, 'tStartRefresh')  # time at next scr refresh
            slider_eq_trial.setAutoDraw(True)
        
        # Check slider_eq_trial for response to end routine
        if slider_eq_trial.getRating() is not None and slider_eq_trial.status == STARTED:
            continueRoutine = False
        
        # *key_resp_eq_trial* updates
        waitOnFlip = False
        if key_resp_eq_trial.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp_eq_trial.frameNStart = frameN  # exact frame index
            key_resp_eq_trial.tStart = t  # local t and not account for scr refresh
            key_resp_eq_trial.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_eq_trial, 'tStartRefresh')  # time at next scr refresh
            key_resp_eq_trial.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_eq_trial.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_eq_trial.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_eq_trial.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_eq_trial.getKeys(keyList=['return'], waitRelease=False)
            _key_resp_eq_trial_allKeys.extend(theseKeys)
            if len(_key_resp_eq_trial_allKeys):
                key_resp_eq_trial.keys = _key_resp_eq_trial_allKeys[-1].name  # just the last key pressed
                key_resp_eq_trial.rt = _key_resp_eq_trial_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in trial_eqComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "trial_eq"-------
    for thisComponent in trial_eqComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials_eq_loop.addData('slider_eq_trial.response', slider_eq_trial.getRating())
    # the Routine "trial_eq" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'trials_eq_loop'


# ------Prepare to start Routine "end_exp"-------
continueRoutine = True
# update component parameters for each repeat
# keep track of which components have finished
end_expComponents = []
for thisComponent in end_expComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
end_expClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "end_exp"-------
while continueRoutine:
    # get current time
    t = end_expClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=end_expClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in end_expComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "end_exp"-------
for thisComponent in end_expComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# the Routine "end_exp" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# Flip one final time so any remaining win.callOnFlip() 
# and win.timeOnFlip() tasks get executed before quitting
win.flip()

# these shouldn't be strictly necessary (should auto-save)
thisExp.saveAsWideText(filename+'.csv', delim='auto')
thisExp.saveAsPickle(filename)
logging.flush()
# make sure everything is closed down
thisExp.abort()  # or data files will save again on exit
win.close()
core.quit()
