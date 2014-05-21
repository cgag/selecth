This is a port of Gary Bernhardt's [Selecta](https://github.com/garybernhardt/selecta) from Ruby to Haskell. Selecta a fuzzy text selector in the vein of CtrlP or Command-T, but is an editor independant unix tool that can filter anything from stdin.  See [Selecta's README](https://github.com/garybernhardt/selecta/blob/master/README.md) for keybindings, suggested uses, the scoring algorithm, and other info.

## Installation
  If you're on a 64 bit processor, you can download the binary from [http://curtis.io/downloads/selecth.html](http://curtis.io/downloads/selecth.html).   For Haskell users, it should be available on Hackage once I get upload permissions.

  Then just place it on your path and you're good to go. See Selecta's readme for more on how to do this if you don't know how. 

### Differences from Selecta
  - Selecth calls "stty sane" to restore your terminal from raw mode, rather than
    setting it to its previous state.  If you keep your terminal in an insane mode
    this could be a problem.  I don't imagine it actually will be, but fixing this is on my TODO list.
  - It's a native executable with no Ruby dependency. 
  - It uses relative positioning rather than always placing your cursor at the bottom of the window after use.
  - Selecth is faster (3-6x), particularly for searching thousands of lines
    - Haskell's single threaded performance is solid, but it will score searches of more than 1000 lines in parallel.
    - Checkout this beautiful [report](http://curtis.io/final-report.html) generated by Bryan O'Sullivan amazing [Criterion](https://hackage.haskell.org/package/criterion).  
    - Selecta's times for the same benchmarks on my machine:
      - non-matching: 0.003015629
      - matching exactly: 0.028580932
      - matching broken up: 0.029262051
      - overlapping matches: 0.087831839
      - words, non-matching: 0.192346901
      - words, matching: 0.20814989
  

