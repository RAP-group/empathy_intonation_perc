# Build empathy quotient trials -----------------------------------------------
#
# Last update: 2020/10/15
#
# - Use items from Baron-Cohen & Wheelwright (2004)
# - Create vars for critical trials, fillers
# - Include indicator vars for 'agree' scoring items and 'disagree' scoring 
#   items
# - Save as .csv and .xlsx in exp/trials dir
#
# -----------------------------------------------------------------------------


# Source libraries ------------------------------------------------------------

source(here::here("scripts", "r","01_helpers.R"))

# -----------------------------------------------------------------------------


# Build trials data frame -----------------------------------------------------

# Create vector of all items
items <- c(
  "I can easily tell if someone else wants to enter a conversation.",
  "I prefer animals to humans.",
  "I try to keep up with the current trends and fashions.",
  "I find it difficult to explain to others things that I understand easily, 
  when they don’t understand it first time.",
  "I dream most nights.",
  "I really enjoy caring for other people.",
  "I try to solve my own problems rather than discussing them with others.",
  "I find it hard to know what to do in a social situation.",
  "I am at my best first thing in the morning.",
  "People often tell me that I went too far in driving my point home in a 
  discussion.",
  "It doesn’t bother me too much if I am late meeting a friend.",
  "Friendships and relationships are just too difficult, so I tend not to 
  bother with them.",
  "I would never break a law, no matter how minor.",
  "I often find it difficult to judge if something is rude or polite.",
  "In a conversation, I tend to focus on my own thoughts rather than on what 
  my listener might be thinking.",
  "I prefer practical jokes to verbal humor.",
  "I live life for today rather than the future.",
  "When I was a child, I enjoyed cutting up worms to see what would happen.",
  "I can pick up quickly if someone says one thing but means another.",
  "I tend to have very strong opinions about morality.",
  "It is hard for me to see why some things upset people so much.",
  "I find it easy to put myself in somebody else’s shoes.",
  "I think that good manners are the most important thing a parent can teach 
  their child.",
  "I like to do things on the spur of the moment.",
  "I am good at predicting how someone will feel.",
  "I am quick to spot when someone in a group is feeling awkward or 
  uncomfortable.",
  "If I say something that someone else is offended by, I think that that’s 
  their problem, not mine.",
  "If anyone asked me if I liked their haircut, I would reply truthfully, even 
  if I didn’t like it.",
  "I can’t always see why someone should have felt offended by a remark.",
  "People often tell me that I am very unpredictable.",
  "I enjoy being the center of attention at any social gathering.",
  "Seeing people cry doesn’t really upset me.",
  "I enjoy having discussions about politics.",
  "I am very blunt, which some people take to be rudeness, even though this is 
  unintentional.",
  "I don’t tend to find social situations confusing.",
  "Other people tell me I am good at understanding how they are feeling and 
  what they are thinking.",
  "When I talk to people, I tend to talk about their experiences rather than 
  my own.",
  "It upsets me to see an animal in pain.",
  "I am able to make decisions without being influenced by people’s feelings.",
  "I can’t relax until I have done everything I had planned to do that day.",
  "I can easily tell if someone else is interested or bored with what I am 
  saying.",
  "I get upset if I see people suffering on news programmes.",
  "Friends usually talk to me about their problems as they say that I am very 
  understanding.",
  "I can sense if I am intruding, even if the other person doesn’t tell me.",
  "I often start new hobbies but quickly become bored with them and move on to 
  something else.",
  "People sometimes tell me that I have gone too far with teasing.",
  "I would be too nervous to go on a big rollercoaster.",
  "Other people, often say that I am insensitive, though I don’t always see 
  why.",
  "If I see a stranger in a group, I think that it is up to them to make an 
  effort to join in.",
  "I usually stay emotionally detached when watching a film.",
  "I like to be very organized in day-to-day life and often make lists of the 
  chores I have to do.",
  "I can tune into how someone else feels rapidly and intuitively.",
  "I don’t like to take risks.",
  "I can easily work out what another person might want to talk about.",
  "I can tell if someone is masking their true emotion.",
  "Before making a decision I always weigh up the pros and cons.",
  "I don’t consciously work out the rules of social situations.",
  "I am good at predicting what someone will do.",
  "I tend to get emotionally involved with a friend’s problems.",
  "I can usually appreciate the other person’s viewpoint, even if I don’t 
  agree with it."
)

# Vector of all item numbers
all_items <- 1:60

# 'agree' scoring items
# - “strongly agree” responses scored 2 points 
# - “slightly agree” responses scored 1 point 
agree_score <- c(1, 6, 19, 22, 25, 26, 35, 36, 37, 38, 41, 42, 43, 44, 52, 54, 
  55, 57, 58, 59, 60)

# 'disagree' scoring items
# - “strongly disagree” responses scored 2 points
# - “slightly disagree” responses scored 1 point
disagree_score <- c(4, 8, 10, 11, 12, 14, 15, 18, 21, 27, 28, 29, 32, 34, 39, 
  46, 48, 49, 50)

# Vector of test items
test_items <- c(agree_score, disagree_score)

# Vector of logicals indicating if test item (TRUE) or filler (FALSE)
is_test <- all_items %in% test_items

# Subset all items to get vector of filler items
filler_items <- all_items[!is_test]

# Check to see if filler items appear in test items (should all be false)
filler_test <- filler_items %in% test_items

# Build data frame
eq_trials <- tibble(
  item_num = all_items, 
  item     = items
  ) %>% 
  mutate(
    is_test_item      = if_else(item_num %in% test_items, 1, 0), 
    is_distractor     = if_else(item_num %in% filler_items, 1, 0), 
    is_agree_score    = if_else(item_num %in% agree_score, 1, 0), 
    is_disagree_score = if_else(item_num %in% disagree_score, 1, 0)) 

write_csv(eq_trials, here("exp", "trials", "eq_trials.csv"))
write_xlsx(eq_trials, here("exp", "trials", "eq_trials.xlsx"), 
  format_headers = F)

# -----------------------------------------------------------------------------
