Program Name: stupidAnt.lisp
Team: AKJ
Group names: Amy Nguyen-Dang, Kiren Syed, Jason Lieu
Contact Info: amyndang@csu.fullerton.edu, kirensyed@csu.fullerton.edu, jasonlieu@csu.fullerton.edu
Class number: CPSC 481

Intro:
This is Ant Colony Optimization. In this project, we will build a ACO swarm that will start at one corner of a 2D grid and attempt to find the shortest path possible to reach the opposite corner. 

External Requirements:
none

Build, Installation, and Setup:
For Windows:s Install the CLISP compiler by downloading: https://sourceforge.net/projects/clisp/files/latest/download

For MAC: Install homebrew and then use in a terminal brew install clisp

Build the .lisp file by using the command in a terminal: clisp <filename>.lisp

Usage:
Through the command line, change the directory to the source folder "AntColonyOptimization", and run the command "clisp stupidAnt.lisp".

Extra Features:
no extra features.

Bugs:

Notes:
* returning uses a backtrack algorithm.
* limit on tabu is removed.