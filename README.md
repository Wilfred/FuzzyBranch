[![Build Status](https://travis-ci.org/Wilfred/FuzzyBranch.svg?branch=master)](https://travis-ci.org/Wilfred/FuzzyBranch)

Bored of writing

```
$ git checkout feature/really-long-branch-name
```

? git-fuzzy saves precious keystrokes. Just type enough of the branch
name to uniquely identify it:

```
$ git co real
```

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

