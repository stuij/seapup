(in-package :pup)

(defparameter *viewpoint* '(("I" . "you") ("you" . "me") ("me" . "you") ("am" . "are") ("yourself" . "myself") ("your" . "my") ("my" . "your")))



(defparameter *eliza-rules*
  `(((%or
      ((%* %x) help (%* %y))
      ((%* %x) menu (%* %y)))
     "Oh dear, you're stuck!
No worries. Here's what you can do:<br/>
- Type 'help' for help (what you just did).<br/>
- Type 'blog' to see the blog.<br/>
- Type random stuff to have a conversation with me, sea pup. I'm a barrel of information.
")

    (((%* %x) blog year (%* %y))
     ,#'(lambda (bindings)
          (declare (ignorable bindings))
          (print-blog-year-posts bindings)))
    
    (((%* %x) blog years (%* %y))
     ,#'(lambda (bindings)
          (declare (ignorable bindings))
          (print-just-years)))
    
    (((%* %x) blog post (%* %y))
     ,#'(lambda (bindings)
          (blog-post bindings)))
    
    (((%* %x) blog (%* %y))
     ,#'(lambda (bindings)
          (blog bindings)))
    
    ((%or
      ((%* %x) fuck (%* %y))
      ((%* %x) shit (%* %y)))
     "That's not very polite.")
    (((%* %x) moof (%* %y))
     "I only take moof from the Moofmaster!"
     "%y may be to moof as moof is to %x, but I moof to my own groove, unless you're the Moofmaster.")    
    (((%* %x) seen (%* %y))
     "I've seen %y, and a whole lot more. My eyes are worn from the seeing.
      I prefer to close my eyes these days. A terrible habit for sure.")
    (((%* %x) hello (%* %y))
     "how do you do.  please state your problem")
    (((%* %x) computer (%* %y))
     "do computers worry you"
     "what do you think about machines"
     "why do you mention computers"
     "what do you think machines have to do with your problem")
    (((%* %x) name (%* %y))
     "i am not interested in names")
    (((%* %x) sorry (%* %y))
     "please don't apologize"
     "apologies are not necessary"
     "what feelings do you have when you apologize")
    (((%* %x) i remember (%* %y))
     "do you often think of %y"
     "does thinking of %y bring anything else to mind"
     "what else do you remember?"
     "why do you recall %y right now"
     "what in this present situation reminds you of %y"
     "what is the connection between me and %y")
    (((%* %x) do you remember (%* %y))
     "did you think i would forget %y"
     "why do you think i should recall %y"
     "what about %y"
     "you mentioned %y")
    (((%* %x) if (%* %y))
     "do you really think it is likely that %y"
     "do you wish that %y"
     "what do you think about %y"
     "really -- if %y")
    (((%* %x) i dreamt (%* %y))
     "really -- %y"
     "have you ever fantasized %y while you were awake"
     "have you dreamt %y before")
    (((%* %x) i dreamed (%* %y))
     "really -- %y"
     "have you ever fantasized %y while you were awake"
     "have you dreamed %y before")
    (((%* %x) dream (%* %y))
     "what does this dream suggest to you"
     "do you dream often"
     "what persons appear in your dreams"
     "don't you believe that dream has to do with your problem")
    (((%* %x) my mother (%* %y))
     "who else is in your family %y"
     "tell me more about your family")
    (((%* %x) my father (%* %y))
     "your father"
     "does he influence you strongly"
     "what else comes to mind when you think of your father")
    (((%* %x) i want (%* %y))
     "what would it mean if you got %y"
     "why do you want %y"
     "suppose you got %y soon")
    (((%* %x) i am glad (%* %y))
     "how have i helped you to be %y"
     "what makes you happy just now"
     "can you explain why you are suddenly %y")
    (((%* %x) i am happy (%* %y))
     "how have i helped you to be %y"
     "what makes you glad just now"
     "can you explain why you are suddenly %y")
    (((%* %x) i am sad (%* %y))
     "i am sorry to hear you are depressed"
     "i'm sure it's not pleasant to be sad")
    (((%* %x) i am unhappy (%* %y))
     "i am sorry to hear you are depressed"
     "i'm sure it's not pleasant to be unhappy")
    (((%* %x) are like (%* %y))
     "what resemblence do you see between %x and %y")
    (((%* %x) is like (%* %y))
     "in what way is it that %x is like %y"
     "what resemblence do you see"
     "could there really be some connection"
     "how")
    (((%* %x) alike (%* %y))
     "in what way"
     "what similarities are there")
    (((%* %x) same (%* %y))
     "what other connections do you see")
    (((%* %x) i was (%* %y))
     "were you really"
     "perhaps i already knew you were %y"
     "why do you tell me you were %y now")
    (((%* %x) was i (%* %y))
     "what if you were %y"
     "do you think you were %y"
     "what would it mean if you were %y")
    (((%* %x) i am (%* %y))
     "in what way are you %y"
     "do you want to be %y")
    (((%* %x) am i (%* %y))
     "do you believe you are %y"
     "would you want to be %y"
     "you wish i would tell you you are %y"
     "what would it mean if you were %y")
    (((%* %x) am (%* %y))
     "why do you say "am""
     "i don't understand that")
    (((%* %x) are you (%* %y))
     "why are you interested in whether i am %y or not"
     "would you prefer it if i weren't %y"
     "perhaps i am %y in your fantasies")
    (((%* %x) you are (%* %y))
     "what makes you think i am %y")
    (((%* %x) because (%* %y))
     "is that the real reason"
     "what other reason might there be"
     "does that reason seem to explain anything else")
    (((%* %x) were you (%* %y))
     "perhaps i was %y"
     "what do you think"
     "what if i had been %y")
    (((%* %x) i can't (%* %y))
     "maybe you could %y now"
     "what if you could %y")
    (((%* x) i feel (%* %y))
     "Do you often feel %y?")
    (((%* %x) i felt (%* %y))
     "what other feelings do you have")
    (((%* %x) i (%* %y) you (%* %z))
     "perhaps in your fantasies we %y each other")
    (((%* %x) why don't you (%* %y))
     "should you %y yourself"
     "do you believe i don't %y"
     "perhaps i will %y in good time")
    (((%* %x) yes (%* %y))
     "you seem quite positive"
     "you are sure"
     "i understand")
    (((%* %x) no (%* %y))
     "why not"
     "you are being a bit negative"
     "are you saying 'no' just to be negative")
    (((%* %x) someone (%* %y))
     "can you be more specific")
    (((%* %x) everyone (%* %y))
     "surely not everyone"
     "can you think of anyone in particular"
     "who for example"
     "you are thinking of a special person")
    (((%* %x) always (%* %y))
     "can you think of a specific example"
     "when"
     "what incident are you thinking of"
     "really -- always")
    (((%* %x) what (%* %y))
     "why do you ask"
     "does that question interest you"
     "what is it you really want to know"
     "what do you think"
     "what comes to your mind when you ask that")
    (((%* %x) perhaps (%* %y))
     "you do not seem quite certain")
    (((%* %x) are (%* %y))
     "do you think they might not be %y"
     "possibly they are %y.")
    (((%* %x))
     "I'm a sea-dragon, not an eel-cat!"
     "very interesting"
     "i am not sure i understand you fully"
     "what does that suggest to you"
     "please continue"
     "go on"
     "do you feel strongly about discussing such things")))

