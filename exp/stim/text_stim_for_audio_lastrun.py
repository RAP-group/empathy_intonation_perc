#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2020.2.5),
    on Mon Nov 16 11:58:43 2020
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
psychopyVersion = '2020.2.5'
expName = 'text_stim_for_audio'  # from the Builder filename that created this script
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
    originPath='/Users/casillas/academia/research/in_progress/empathy_intonation_perc/exp/stim/text_stim_for_audio_lastrun.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.DEBUG)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp
frameTolerance = 0.001  # how close to onset before 'same' frame

# Start Code - component code to be run before the window creation

# Setup the Window
win = visual.Window(
    size=[1050, 750], fullscr=False, screen=0, 
    winType='pyglet', allowGUI=True, allowStencil=False,
    monitor='testMonitor', color=[1,1,1], colorSpace='rgb',
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

# Initialize components for Routine "stim"
stimClock = core.Clock()
text_stim_stim_n = visual.TextStim(win=win, name='text_stim_stim_n',
    text='default text',
    font='Arial',
    pos=(-0.67, -0.47), height=0.03, wrapWidth=None, ori=0, 
    color='grey', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
text_stim_sentence_type = visual.TextStim(win=win, name='text_stim_sentence_type',
    text='default text',
    font='Arial',
    pos=(-0.57, -0.47), height=0.03, wrapWidth=None, ori=0, 
    color='grey', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);
text_stim_condition = visual.TextStim(win=win, name='text_stim_condition',
    text='default text',
    font='Arial',
    pos=(0.0, -0.47), height=0.03, wrapWidth=None, ori=0, 
    color='grey', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);
text_stim_prompt = visual.TextStim(win=win, name='text_stim_prompt',
    text='default text',
    font='Arial',
    pos=(0, 0.3), height=0.07, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-3.0);
text_stim_sentence = visual.TextStim(win=win, name='text_stim_sentence',
    text='default text',
    font='Arial',
    pos=(0, 0), height=0.09, wrapWidth=None, ori=0, 
    color='black', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-4.0);
key_resp_stim = keyboard.Keyboard()

# Initialize components for Routine "stim"
stimClock = core.Clock()
text_stim_stim_n = visual.TextStim(win=win, name='text_stim_stim_n',
    text='default text',
    font='Arial',
    pos=(-0.67, -0.47), height=0.03, wrapWidth=None, ori=0, 
    color='grey', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
text_stim_sentence_type = visual.TextStim(win=win, name='text_stim_sentence_type',
    text='default text',
    font='Arial',
    pos=(-0.57, -0.47), height=0.03, wrapWidth=None, ori=0, 
    color='grey', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);
text_stim_condition = visual.TextStim(win=win, name='text_stim_condition',
    text='default text',
    font='Arial',
    pos=(0.0, -0.47), height=0.03, wrapWidth=None, ori=0, 
    color='grey', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);
text_stim_prompt = visual.TextStim(win=win, name='text_stim_prompt',
    text='default text',
    font='Arial',
    pos=(0, 0.3), height=0.07, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-3.0);
text_stim_sentence = visual.TextStim(win=win, name='text_stim_sentence',
    text='default text',
    font='Arial',
    pos=(0, 0), height=0.09, wrapWidth=None, ori=0, 
    color='black', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-4.0);
key_resp_stim = keyboard.Keyboard()

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# set up handler to look after randomisation of conditions etc
stim_loop = data.TrialHandler(nReps=1, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('trials/stim_list.csv'),
    seed=None, name='stim_loop')
thisExp.addLoop(stim_loop)  # add the loop to the experiment
thisStim_loop = stim_loop.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisStim_loop.rgb)
if thisStim_loop != None:
    for paramName in thisStim_loop:
        exec('{} = thisStim_loop[paramName]'.format(paramName))

for thisStim_loop in stim_loop:
    currentLoop = stim_loop
    # abbreviate parameter names if possible (e.g. rgb = thisStim_loop.rgb)
    if thisStim_loop != None:
        for paramName in thisStim_loop:
            exec('{} = thisStim_loop[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "stim"-------
    continueRoutine = True
    # update component parameters for each repeat
    text_stim_stim_n.setText(stim_n)
    text_stim_sentence_type.setText(sentence_type)
    text_stim_condition.setText(condition)
    text_stim_prompt.setColor(color, colorSpace='rgb')
    text_stim_prompt.setText(prompt)
    text_stim_sentence.setText(sentence)
    key_resp_stim.keys = []
    key_resp_stim.rt = []
    _key_resp_stim_allKeys = []
    # keep track of which components have finished
    stimComponents = [text_stim_stim_n, text_stim_sentence_type, text_stim_condition, text_stim_prompt, text_stim_sentence, key_resp_stim]
    for thisComponent in stimComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    stimClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "stim"-------
    while continueRoutine:
        # get current time
        t = stimClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=stimClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_stim_stim_n* updates
        if text_stim_stim_n.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_stim_stim_n.frameNStart = frameN  # exact frame index
            text_stim_stim_n.tStart = t  # local t and not account for scr refresh
            text_stim_stim_n.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_stim_stim_n, 'tStartRefresh')  # time at next scr refresh
            text_stim_stim_n.setAutoDraw(True)
        
        # *text_stim_sentence_type* updates
        if text_stim_sentence_type.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_stim_sentence_type.frameNStart = frameN  # exact frame index
            text_stim_sentence_type.tStart = t  # local t and not account for scr refresh
            text_stim_sentence_type.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_stim_sentence_type, 'tStartRefresh')  # time at next scr refresh
            text_stim_sentence_type.setAutoDraw(True)
        
        # *text_stim_condition* updates
        if text_stim_condition.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_stim_condition.frameNStart = frameN  # exact frame index
            text_stim_condition.tStart = t  # local t and not account for scr refresh
            text_stim_condition.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_stim_condition, 'tStartRefresh')  # time at next scr refresh
            text_stim_condition.setAutoDraw(True)
        
        # *text_stim_prompt* updates
        if text_stim_prompt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_stim_prompt.frameNStart = frameN  # exact frame index
            text_stim_prompt.tStart = t  # local t and not account for scr refresh
            text_stim_prompt.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_stim_prompt, 'tStartRefresh')  # time at next scr refresh
            text_stim_prompt.setAutoDraw(True)
        
        # *text_stim_sentence* updates
        if text_stim_sentence.status == NOT_STARTED and tThisFlip >= 0.05-frameTolerance:
            # keep track of start time/frame for later
            text_stim_sentence.frameNStart = frameN  # exact frame index
            text_stim_sentence.tStart = t  # local t and not account for scr refresh
            text_stim_sentence.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_stim_sentence, 'tStartRefresh')  # time at next scr refresh
            text_stim_sentence.setAutoDraw(True)
        
        # *key_resp_stim* updates
        waitOnFlip = False
        if key_resp_stim.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp_stim.frameNStart = frameN  # exact frame index
            key_resp_stim.tStart = t  # local t and not account for scr refresh
            key_resp_stim.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_stim, 'tStartRefresh')  # time at next scr refresh
            key_resp_stim.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_stim.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_stim.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_stim.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_stim.getKeys(keyList=['y', 'n', 'left', 'right', 'space'], waitRelease=False)
            _key_resp_stim_allKeys.extend(theseKeys)
            if len(_key_resp_stim_allKeys):
                key_resp_stim.keys = _key_resp_stim_allKeys[-1].name  # just the last key pressed
                key_resp_stim.rt = _key_resp_stim_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in stimComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "stim"-------
    for thisComponent in stimComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    stim_loop.addData('text_stim_stim_n.started', text_stim_stim_n.tStartRefresh)
    stim_loop.addData('text_stim_stim_n.stopped', text_stim_stim_n.tStopRefresh)
    stim_loop.addData('text_stim_sentence_type.started', text_stim_sentence_type.tStartRefresh)
    stim_loop.addData('text_stim_sentence_type.stopped', text_stim_sentence_type.tStopRefresh)
    stim_loop.addData('text_stim_condition.started', text_stim_condition.tStartRefresh)
    stim_loop.addData('text_stim_condition.stopped', text_stim_condition.tStopRefresh)
    stim_loop.addData('text_stim_prompt.started', text_stim_prompt.tStartRefresh)
    stim_loop.addData('text_stim_prompt.stopped', text_stim_prompt.tStopRefresh)
    stim_loop.addData('text_stim_sentence.started', text_stim_sentence.tStartRefresh)
    stim_loop.addData('text_stim_sentence.stopped', text_stim_sentence.tStopRefresh)
    # the Routine "stim" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
# completed 1 repeats of 'stim_loop'


# set up handler to look after randomisation of conditions etc
fillers_loop = data.TrialHandler(nReps=1, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('trials/filler_list.csv'),
    seed=None, name='fillers_loop')
thisExp.addLoop(fillers_loop)  # add the loop to the experiment
thisFillers_loop = fillers_loop.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisFillers_loop.rgb)
if thisFillers_loop != None:
    for paramName in thisFillers_loop:
        exec('{} = thisFillers_loop[paramName]'.format(paramName))

for thisFillers_loop in fillers_loop:
    currentLoop = fillers_loop
    # abbreviate parameter names if possible (e.g. rgb = thisFillers_loop.rgb)
    if thisFillers_loop != None:
        for paramName in thisFillers_loop:
            exec('{} = thisFillers_loop[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "stim"-------
    continueRoutine = True
    # update component parameters for each repeat
    text_stim_stim_n.setText(stim_n)
    text_stim_sentence_type.setText(sentence_type)
    text_stim_condition.setText(condition)
    text_stim_prompt.setColor(color, colorSpace='rgb')
    text_stim_prompt.setText(prompt)
    text_stim_sentence.setText(sentence)
    key_resp_stim.keys = []
    key_resp_stim.rt = []
    _key_resp_stim_allKeys = []
    # keep track of which components have finished
    stimComponents = [text_stim_stim_n, text_stim_sentence_type, text_stim_condition, text_stim_prompt, text_stim_sentence, key_resp_stim]
    for thisComponent in stimComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    stimClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "stim"-------
    while continueRoutine:
        # get current time
        t = stimClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=stimClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_stim_stim_n* updates
        if text_stim_stim_n.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_stim_stim_n.frameNStart = frameN  # exact frame index
            text_stim_stim_n.tStart = t  # local t and not account for scr refresh
            text_stim_stim_n.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_stim_stim_n, 'tStartRefresh')  # time at next scr refresh
            text_stim_stim_n.setAutoDraw(True)
        
        # *text_stim_sentence_type* updates
        if text_stim_sentence_type.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_stim_sentence_type.frameNStart = frameN  # exact frame index
            text_stim_sentence_type.tStart = t  # local t and not account for scr refresh
            text_stim_sentence_type.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_stim_sentence_type, 'tStartRefresh')  # time at next scr refresh
            text_stim_sentence_type.setAutoDraw(True)
        
        # *text_stim_condition* updates
        if text_stim_condition.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_stim_condition.frameNStart = frameN  # exact frame index
            text_stim_condition.tStart = t  # local t and not account for scr refresh
            text_stim_condition.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_stim_condition, 'tStartRefresh')  # time at next scr refresh
            text_stim_condition.setAutoDraw(True)
        
        # *text_stim_prompt* updates
        if text_stim_prompt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_stim_prompt.frameNStart = frameN  # exact frame index
            text_stim_prompt.tStart = t  # local t and not account for scr refresh
            text_stim_prompt.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_stim_prompt, 'tStartRefresh')  # time at next scr refresh
            text_stim_prompt.setAutoDraw(True)
        
        # *text_stim_sentence* updates
        if text_stim_sentence.status == NOT_STARTED and tThisFlip >= 0.05-frameTolerance:
            # keep track of start time/frame for later
            text_stim_sentence.frameNStart = frameN  # exact frame index
            text_stim_sentence.tStart = t  # local t and not account for scr refresh
            text_stim_sentence.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_stim_sentence, 'tStartRefresh')  # time at next scr refresh
            text_stim_sentence.setAutoDraw(True)
        
        # *key_resp_stim* updates
        waitOnFlip = False
        if key_resp_stim.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp_stim.frameNStart = frameN  # exact frame index
            key_resp_stim.tStart = t  # local t and not account for scr refresh
            key_resp_stim.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_stim, 'tStartRefresh')  # time at next scr refresh
            key_resp_stim.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_stim.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_stim.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_stim.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_stim.getKeys(keyList=['y', 'n', 'left', 'right', 'space'], waitRelease=False)
            _key_resp_stim_allKeys.extend(theseKeys)
            if len(_key_resp_stim_allKeys):
                key_resp_stim.keys = _key_resp_stim_allKeys[-1].name  # just the last key pressed
                key_resp_stim.rt = _key_resp_stim_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in stimComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "stim"-------
    for thisComponent in stimComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    fillers_loop.addData('text_stim_stim_n.started', text_stim_stim_n.tStartRefresh)
    fillers_loop.addData('text_stim_stim_n.stopped', text_stim_stim_n.tStopRefresh)
    fillers_loop.addData('text_stim_sentence_type.started', text_stim_sentence_type.tStartRefresh)
    fillers_loop.addData('text_stim_sentence_type.stopped', text_stim_sentence_type.tStopRefresh)
    fillers_loop.addData('text_stim_condition.started', text_stim_condition.tStartRefresh)
    fillers_loop.addData('text_stim_condition.stopped', text_stim_condition.tStopRefresh)
    fillers_loop.addData('text_stim_prompt.started', text_stim_prompt.tStartRefresh)
    fillers_loop.addData('text_stim_prompt.stopped', text_stim_prompt.tStopRefresh)
    fillers_loop.addData('text_stim_sentence.started', text_stim_sentence.tStartRefresh)
    fillers_loop.addData('text_stim_sentence.stopped', text_stim_sentence.tStopRefresh)
    # the Routine "stim" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'fillers_loop'


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
