#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2020.2.4),
    on Thu Oct 15 23:24:12 2020
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
psychopyVersion = '2020.2.4'
expName = 'empathy_intonation_perc'  # from the Builder filename that created this script
expInfo = {'participant': '', 'session': '001'}
dlg = gui.DlgFromDict(dictionary=expInfo, sort_keys=False, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='/Users/casillas/academia/research/in_progress/empathy_intonation_perc/exp/empathy_intonation_perc_lastrun.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp
frameTolerance = 0.001  # how close to onset before 'same' frame

# Start Code - component code to be run before the window creation

# Setup the Window
win = visual.Window(
    size=(1024, 768), fullscr=True, screen=0, 
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

# Initialize components for Routine "trial"
trialClock = core.Clock()
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
    flip=False)
key_resp_eq_trial = keyboard.Keyboard()

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# set up handler to look after randomisation of conditions etc
eq_trials = data.TrialHandler(nReps=1, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('trials/eq_trials.csv'),
    seed=None, name='eq_trials')
thisExp.addLoop(eq_trials)  # add the loop to the experiment
thisEq_trial = eq_trials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisEq_trial.rgb)
if thisEq_trial != None:
    for paramName in thisEq_trial:
        exec('{} = thisEq_trial[paramName]'.format(paramName))

for thisEq_trial in eq_trials:
    currentLoop = eq_trials
    # abbreviate parameter names if possible (e.g. rgb = thisEq_trial.rgb)
    if thisEq_trial != None:
        for paramName in thisEq_trial:
            exec('{} = thisEq_trial[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "trial"-------
    continueRoutine = True
    # update component parameters for each repeat
    text_eq_question_trial.setText(item)
    slider_eq_trial.reset()
    key_resp_eq_trial.keys = []
    key_resp_eq_trial.rt = []
    _key_resp_eq_trial_allKeys = []
    # keep track of which components have finished
    trialComponents = [text_eq_question_trial, slider_eq_trial, key_resp_eq_trial]
    for thisComponent in trialComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    trialClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "trial"-------
    while continueRoutine:
        # get current time
        t = trialClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=trialClock)
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
        for thisComponent in trialComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "trial"-------
    for thisComponent in trialComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    eq_trials.addData('text_eq_question_trial.started', text_eq_question_trial.tStartRefresh)
    eq_trials.addData('text_eq_question_trial.stopped', text_eq_question_trial.tStopRefresh)
    eq_trials.addData('slider_eq_trial.response', slider_eq_trial.getRating())
    # the Routine "trial" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'eq_trials'


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
