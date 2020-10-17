#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2020.2.4),
    on Fri Oct 16 16:10:34 2020
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

# Initialize components for Routine "lextale_instructions"
lextale_instructionsClock = core.Clock()
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

# Initialize components for Routine "lextale_practice"
lextale_practiceClock = core.Clock()
text_lextale_practice_label = visual.TextStim(win=win, name='text_lextale_practice_label',
    text='PRÁCTICA',
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

# Initialize components for Routine "lextale_practice_feedback"
lextale_practice_feedbackClock = core.Clock()
text_lextale_practice_feedback = visual.TextStim(win=win, name='text_lextale_practice_feedback',
    text='default text',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);
key_resp_lextale_practice_feedback = keyboard.Keyboard()

# Initialize components for Routine "lextale_check"
lextale_checkClock = core.Clock()
text_lextale_check = visual.TextStim(win=win, name='text_lextale_check',
    text='¿Preguntas?\n\nPresiona la barra espaciadora para comenzar.',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
key_resp_lextale_check = keyboard.Keyboard()

# Initialize components for Routine "lextale_trial"
lextale_trialClock = core.Clock()
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

# Initialize components for Routine "eq_instructions"
eq_instructionsClock = core.Clock()
key_resp_eq_instructions = keyboard.Keyboard()
text_eq_instructions = visual.TextStim(win=win, name='text_eq_instructions',
    text='instructions here',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "eq_trial"
eq_trialClock = core.Clock()
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
lextale_instructions_loop = data.TrialHandler(nReps=1, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('instructions/lextale_instructions_text.csv'),
    seed=None, name='lextale_instructions_loop')
thisExp.addLoop(lextale_instructions_loop)  # add the loop to the experiment
thisLextale_instructions_loop = lextale_instructions_loop.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisLextale_instructions_loop.rgb)
if thisLextale_instructions_loop != None:
    for paramName in thisLextale_instructions_loop:
        exec('{} = thisLextale_instructions_loop[paramName]'.format(paramName))

for thisLextale_instructions_loop in lextale_instructions_loop:
    currentLoop = lextale_instructions_loop
    # abbreviate parameter names if possible (e.g. rgb = thisLextale_instructions_loop.rgb)
    if thisLextale_instructions_loop != None:
        for paramName in thisLextale_instructions_loop:
            exec('{} = thisLextale_instructions_loop[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "lextale_instructions"-------
    continueRoutine = True
    # update component parameters for each repeat
    text_lextale_instructions.setText(instructions_text)
    key_resp_lextale_instructions.keys = []
    key_resp_lextale_instructions.rt = []
    _key_resp_lextale_instructions_allKeys = []
    text_lextale_instructions_continue.setText(continue_text)
    # keep track of which components have finished
    lextale_instructionsComponents = [text_lextale_instructions, key_resp_lextale_instructions, text_lextale_instructions_continue]
    for thisComponent in lextale_instructionsComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    lextale_instructionsClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "lextale_instructions"-------
    while continueRoutine:
        # get current time
        t = lextale_instructionsClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=lextale_instructionsClock)
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
        for thisComponent in lextale_instructionsComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "lextale_instructions"-------
    for thisComponent in lextale_instructionsComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "lextale_instructions" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'lextale_instructions_loop'


# set up handler to look after randomisation of conditions etc
lextale_practice_trials = data.TrialHandler(nReps=1, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('trials/lextale_practice_trials.csv'),
    seed=None, name='lextale_practice_trials')
thisExp.addLoop(lextale_practice_trials)  # add the loop to the experiment
thisLextale_practice_trial = lextale_practice_trials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisLextale_practice_trial.rgb)
if thisLextale_practice_trial != None:
    for paramName in thisLextale_practice_trial:
        exec('{} = thisLextale_practice_trial[paramName]'.format(paramName))

for thisLextale_practice_trial in lextale_practice_trials:
    currentLoop = lextale_practice_trials
    # abbreviate parameter names if possible (e.g. rgb = thisLextale_practice_trial.rgb)
    if thisLextale_practice_trial != None:
        for paramName in thisLextale_practice_trial:
            exec('{} = thisLextale_practice_trial[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "lextale_practice"-------
    continueRoutine = True
    # update component parameters for each repeat
    text_lextale_practice_word.setText(word)
    key_resp_lextale_practice.keys = []
    key_resp_lextale_practice.rt = []
    _key_resp_lextale_practice_allKeys = []
    text_lextale_practice_real_label.setText(prac_button_real)
    text_lextale_practice_false_label.setText(prac_button_false)
    # keep track of which components have finished
    lextale_practiceComponents = [text_lextale_practice_label, text_lextale_practice_word, key_resp_lextale_practice, text_lextale_practice_real_label, text_lextale_practice_false_label]
    for thisComponent in lextale_practiceComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    lextale_practiceClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "lextale_practice"-------
    while continueRoutine:
        # get current time
        t = lextale_practiceClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=lextale_practiceClock)
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
        for thisComponent in lextale_practiceComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "lextale_practice"-------
    for thisComponent in lextale_practiceComponents:
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
    # store data for lextale_practice_trials (TrialHandler)
    lextale_practice_trials.addData('key_resp_lextale_practice.keys',key_resp_lextale_practice.keys)
    lextale_practice_trials.addData('key_resp_lextale_practice.corr', key_resp_lextale_practice.corr)
    if key_resp_lextale_practice.keys != None:  # we had a response
        lextale_practice_trials.addData('key_resp_lextale_practice.rt', key_resp_lextale_practice.rt)
    # the Routine "lextale_practice" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "lextale_practice_feedback"-------
    continueRoutine = True
    routineTimer.add(1.000000)
    # update component parameters for each repeat
    if key_resp_lextale_practice.corr == 1:
        msg = "Correcto!"
    else:
        msg = "Ooo! Te has equivocado!"
    text_lextale_practice_feedback.setText(msg)
    key_resp_lextale_practice_feedback.keys = []
    key_resp_lextale_practice_feedback.rt = []
    _key_resp_lextale_practice_feedback_allKeys = []
    # keep track of which components have finished
    lextale_practice_feedbackComponents = [text_lextale_practice_feedback, key_resp_lextale_practice_feedback]
    for thisComponent in lextale_practice_feedbackComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    lextale_practice_feedbackClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "lextale_practice_feedback"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = lextale_practice_feedbackClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=lextale_practice_feedbackClock)
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
        for thisComponent in lextale_practice_feedbackComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "lextale_practice_feedback"-------
    for thisComponent in lextale_practice_feedbackComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    thisExp.nextEntry()
    
# completed 1 repeats of 'lextale_practice_trials'


# ------Prepare to start Routine "lextale_check"-------
continueRoutine = True
# update component parameters for each repeat
key_resp_lextale_check.keys = []
key_resp_lextale_check.rt = []
_key_resp_lextale_check_allKeys = []
# keep track of which components have finished
lextale_checkComponents = [text_lextale_check, key_resp_lextale_check]
for thisComponent in lextale_checkComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
lextale_checkClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "lextale_check"-------
while continueRoutine:
    # get current time
    t = lextale_checkClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=lextale_checkClock)
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
    for thisComponent in lextale_checkComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "lextale_check"-------
for thisComponent in lextale_checkComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# the Routine "lextale_check" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
lextail_trials = data.TrialHandler(nReps=1, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('trials/lextale_trials.csv'),
    seed=None, name='lextail_trials')
thisExp.addLoop(lextail_trials)  # add the loop to the experiment
thisLextail_trial = lextail_trials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisLextail_trial.rgb)
if thisLextail_trial != None:
    for paramName in thisLextail_trial:
        exec('{} = thisLextail_trial[paramName]'.format(paramName))

for thisLextail_trial in lextail_trials:
    currentLoop = lextail_trials
    # abbreviate parameter names if possible (e.g. rgb = thisLextail_trial.rgb)
    if thisLextail_trial != None:
        for paramName in thisLextail_trial:
            exec('{} = thisLextail_trial[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "lextale_trial"-------
    continueRoutine = True
    # update component parameters for each repeat
    text_lextale_trial_word.setText(word)
    key_resp_lextale_trial.keys = []
    key_resp_lextale_trial.rt = []
    _key_resp_lextale_trial_allKeys = []
    text_lextale_trial_real_label.setText(button_real)
    text_lextale_trial_false_label.setText(button_false)
    # keep track of which components have finished
    lextale_trialComponents = [text_lextale_trial_word, key_resp_lextale_trial, text_lextale_trial_real_label, text_lextale_trial_false_label]
    for thisComponent in lextale_trialComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    lextale_trialClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "lextale_trial"-------
    while continueRoutine:
        # get current time
        t = lextale_trialClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=lextale_trialClock)
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
        for thisComponent in lextale_trialComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "lextale_trial"-------
    for thisComponent in lextale_trialComponents:
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
    # store data for lextail_trials (TrialHandler)
    lextail_trials.addData('key_resp_lextale_trial.keys',key_resp_lextale_trial.keys)
    lextail_trials.addData('key_resp_lextale_trial.corr', key_resp_lextale_trial.corr)
    if key_resp_lextale_trial.keys != None:  # we had a response
        lextail_trials.addData('key_resp_lextale_trial.rt', key_resp_lextale_trial.rt)
    # the Routine "lextale_trial" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'lextail_trials'


# ------Prepare to start Routine "eq_instructions"-------
continueRoutine = True
# update component parameters for each repeat
key_resp_eq_instructions.keys = []
key_resp_eq_instructions.rt = []
_key_resp_eq_instructions_allKeys = []
# keep track of which components have finished
eq_instructionsComponents = [key_resp_eq_instructions, text_eq_instructions]
for thisComponent in eq_instructionsComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
eq_instructionsClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "eq_instructions"-------
while continueRoutine:
    # get current time
    t = eq_instructionsClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=eq_instructionsClock)
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
    for thisComponent in eq_instructionsComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "eq_instructions"-------
for thisComponent in eq_instructionsComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# the Routine "eq_instructions" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

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
    
    # ------Prepare to start Routine "eq_trial"-------
    continueRoutine = True
    # update component parameters for each repeat
    text_eq_question_trial.setText(item)
    slider_eq_trial.reset()
    key_resp_eq_trial.keys = []
    key_resp_eq_trial.rt = []
    _key_resp_eq_trial_allKeys = []
    # keep track of which components have finished
    eq_trialComponents = [text_eq_question_trial, slider_eq_trial, key_resp_eq_trial]
    for thisComponent in eq_trialComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    eq_trialClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "eq_trial"-------
    while continueRoutine:
        # get current time
        t = eq_trialClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=eq_trialClock)
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
        for thisComponent in eq_trialComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "eq_trial"-------
    for thisComponent in eq_trialComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    eq_trials.addData('slider_eq_trial.response', slider_eq_trial.getRating())
    # the Routine "eq_trial" was not non-slip safe, so reset the non-slip timer
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
