(in-package :pup)

(defparameter *viewpoint* '(("I" . "you") ("you" . "me") ("me" . "you") ("am" . "are") ("yourself" . "myself") ("your" . "my") ("my" . "your")))

(defparameter *eliza-rules*
  `(((%or
      ((%* %x) help (%* %y))
      ((%* %x) menu (%* %y)))
     "<br/>
Oh dear, you're stuck!
No worries. Here's what you can do:<br/>
- Type 'help' for help (what you just did).<br/>
- Type 'blog' to see the blog.<br/>
- Type random stuff to have a conversation with me, sea pup. I'm a barrel of information.
")
    
    (((%* %x) blog (%* %y))
     ,#'(lambda (bindings responses)
          (declare (ignorable bindings responses))
          (blog)))
    
    (((%* %x) moof (%* %y))
     "I only take moof from the Moofmaster!"
     "%y may be to moof as moof is to %x, but I moof to my own groove, unless you're the Moofmaster.")
    
    (((%* %x) seen (%* %y))
     "I've seen %y, and a whole lot more. My eyes are worn from the seeing.
      I prefer to close my eyes these days. A terrible habit for sure.")
    
    (((%* %x) hello (%* %y))
     "How do you do?")
    
    ((%or
      ((%* %x) fuck (%* %y))
      ((%* %x) shit (%* %y)))
     "That's not very polite.")

    (((%* %x))
     "I'm a sea-dragon, not an eel-cat.")))
