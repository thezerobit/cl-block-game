# Cl-Block-Game

A mostly-functional falling block game in Common Lisp. See it in action
on [youtube](http://www.youtube.com/watch?v=mY6tuSI-sgk).

## Usage

In game folder, start lisp repl:

    (ql:quickload :fset)
    (ql:quickload :lispbuilder-sdl)
    (require "asdf")
    (asdf:load-system :cl-block-game)
    (in-package #:cl-block-game)
    (game-loop)

* left and right and down arrows move piece
* up arrow rotates piece
* space bar drops piece
* 'r' reverses game time
* 'p' pauses game (use 'f' to go forward again)
* 'f' game moves forward through time
* 'n' starts a brand new game

## Installation

## TODO:

* Next Piece

## Author

* Stephen A. Goss (steveth45@gmail.com)

## Copyright

Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)

# License

Licensed under the Modified BSD License. See LICENSE.txt.

