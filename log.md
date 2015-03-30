3/19/2015:
  - Read the full buffered input rather than one character at a time.
    - Collapse multiple searches or multiple backspaces into single actions,
      e.g: this: [Search "d", Search "o", Search "g", DropChars 1, DropChars 1]
           becomes: [Search "dog", DropChars 2].
           Assuming you manage to enter that all before we pull from the buffer.
    - Maybe we should delay a few ms between buffer reads?
      - Could use hWaitForInput.

    Concerns:  Not sure memoization is worth the code complexity after this
               change.  Then again using a state monad might decrease the 
               complexity, and the performance cost is basically nil so I'm
               leaving it in for now.

               Bug: Realized I made a mistake when removing choicesToShow
                    from writeSelection.  It now uses the match count, which
                    could potentially be much larger than choicesToShow,
                    pushing the selection to the bottom of the screen.
