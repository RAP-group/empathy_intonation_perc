#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2020.2.10),
    on Sat Feb 13 01:13:20 2021
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
expName = 'empathy_intonation_perc_sp'  # from the Builder filename that created this script
expInfo = {'id': '', 'La variedad del español que mejor conozcas': 'ej. cubano'}
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
    originPath='/Users/casillas/academia/research/in_progress/empathy_intonation_perc/exp/empathy_intonation_perc_sp/empathy_intonation_perc_sp_lastrun.py',
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
text_2afc_instructions = visual.TextStim(win=win, name='text_2afc_instructions',
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
    text='Sí',
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
    text='Es una pregunta?',
    font='Arial',
    pos=(0, 0.3), height=0.1, wrapWidth=None, ori=0, 
    color='blue', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-4.0);

# Initialize components for Routine "check_2afc"
check_2afcClock = core.Clock()
text_2afc_check = visual.TextStim(win=win, name='text_2afc_check',
    text='Bien! Ahora vamos a empezar. \n\nIntenta responder lo más rápido posible sin cometer errores después de escuchar cada enunciado. \n\n(presiona la barra espaciadora para comenzar)',
    font='Arial',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
key_resp_2afc_gotit = keyboard.Keyboard()

# Initialize components for Routine "trial_2afc"
trial_2afcClock = core.Clock()
col_name = ['andalusian', 'argentine', 'castilian', 'chilean', 'cuban', 'mexican', 'peruvian', 'puertorican']

sound_stim_trial = sound.Sound('A', secs=-1, stereo=True, hamming=True,
    name='sound_stim_trial')
sound_stim_trial.setVolume(1)
text_response_yes_trial = visual.TextStim(win=win, name='text_response_yes_trial',
    text='Sí',
    font='Arial',
    pos=(-0.3, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);
text_response_no_trial = visual.TextStim(win=win, name='text_response_no_trial',
    text='No',
    font='Arial',
    pos=(0.3, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-3.0);
key_resp_2afc_trial = keyboard.Keyboard()
text_question_trial = visual.TextStim(win=win, name='text_question_trial',
    text='Es una pregunta?',
    font='Arial',
    pos=(0, 0.3), height=0.1, wrapWidth=None, ori=0, 
    color='blue', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-5.0);

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# set up handler to look after randomisation of conditions etc
instructions_2afc_loop = data.TrialHandler(nReps=1, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('instructions/2afc_instructions_sp_text.xlsx'),
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
    text_2afc_instructions.setText(instructions_text)
    text_2afc_instructions_continue.setText(continue_text)
    key_resp_2afc_instructions.keys = []
    key_resp_2afc_instructions.rt = []
    _key_resp_2afc_instructions_allKeys = []
    # keep track of which components have finished
    instructions_2afcComponents = [text_2afc_instructions, text_2afc_instructions_continue, key_resp_2afc_instructions]
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
        
        # *text_2afc_instructions* updates
        if text_2afc_instructions.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_2afc_instructions.frameNStart = frameN  # exact frame index
            text_2afc_instructions.tStart = t  # local t and not account for scr refresh
            text_2afc_instructions.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_2afc_instructions, 'tStartRefresh')  # time at next scr refresh
            text_2afc_instructions.setAutoDraw(True)
        
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
    
    sound_stim_trial.setSound(eval(the_col), hamming=True)
    sound_stim_trial.setVolume(1, log=False)
    key_resp_2afc_trial.keys = []
    key_resp_2afc_trial.rt = []
    _key_resp_2afc_trial_allKeys = []
    # keep track of which components have finished
    trial_2afcComponents = [sound_stim_trial, text_response_yes_trial, text_response_no_trial, key_resp_2afc_trial, text_question_trial]
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
        # start/stop sound_stim_trial
        if sound_stim_trial.status == NOT_STARTED and tThisFlip >= 0.500-frameTolerance:
            # keep track of start time/frame for later
            sound_stim_trial.frameNStart = frameN  # exact frame index
            sound_stim_trial.tStart = t  # local t and not account for scr refresh
            sound_stim_trial.tStartRefresh = tThisFlipGlobal  # on global time
            sound_stim_trial.play(when=win)  # sync with win flip
        
        # *text_response_yes_trial* updates
        if text_response_yes_trial.status == NOT_STARTED and tThisFlip >= 0.250-frameTolerance:
            # keep track of start time/frame for later
            text_response_yes_trial.frameNStart = frameN  # exact frame index
            text_response_yes_trial.tStart = t  # local t and not account for scr refresh
            text_response_yes_trial.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_response_yes_trial, 'tStartRefresh')  # time at next scr refresh
            text_response_yes_trial.setAutoDraw(True)
        
        # *text_response_no_trial* updates
        if text_response_no_trial.status == NOT_STARTED and tThisFlip >= 0.250-frameTolerance:
            # keep track of start time/frame for later
            text_response_no_trial.frameNStart = frameN  # exact frame index
            text_response_no_trial.tStart = t  # local t and not account for scr refresh
            text_response_no_trial.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_response_no_trial, 'tStartRefresh')  # time at next scr refresh
            text_response_no_trial.setAutoDraw(True)
        
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
        
        # *text_question_trial* updates
        if text_question_trial.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_question_trial.frameNStart = frameN  # exact frame index
            text_question_trial.tStart = t  # local t and not account for scr refresh
            text_question_trial.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_question_trial, 'tStartRefresh')  # time at next scr refresh
            text_question_trial.setAutoDraw(True)
        
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
    
    sound_stim_trial.stop()  # ensure sound has stopped at end of routine
    trials_2afc_loop.addData('sound_stim_trial.started', sound_stim_trial.tStartRefresh)
    trials_2afc_loop.addData('sound_stim_trial.stopped', sound_stim_trial.tStopRefresh)
    trials_2afc_loop.addData('text_response_yes_trial.started', text_response_yes_trial.tStartRefresh)
    trials_2afc_loop.addData('text_response_yes_trial.stopped', text_response_yes_trial.tStopRefresh)
    trials_2afc_loop.addData('text_response_no_trial.started', text_response_no_trial.tStartRefresh)
    trials_2afc_loop.addData('text_response_no_trial.stopped', text_response_no_trial.tStopRefresh)
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
    trials_2afc_loop.addData('text_question_trial.started', text_question_trial.tStartRefresh)
    trials_2afc_loop.addData('text_question_trial.stopped', text_question_trial.tStopRefresh)
    # the Routine "trial_2afc" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'trials_2afc_loop'


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
