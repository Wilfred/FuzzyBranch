fuzzy branch is a tool for checking out a copy of a branch, using
fuzzy string matching. 

Inspired by Emacs' ido-mode, but written in Haskell.

## Installation

    $ cabal install --user
    
This will install an executable `git-fuzzy` to `~/.cabal/bin`. Copy
this to somewhere on your `$PATH`.

You then want to set up a git alias for this command. I like 
`git co`. Add to your `~/.gitconfig`:

    [alias]
        co = !git-fuzzy
        
Now, rather than writing:

    $ git checkout develop
    
You can just write:

    $ git co dev

