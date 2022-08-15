# pi-calc-lts. 

An implementation of the pi-calculus using the nom package [[3]](#3) for name-handling.
Part of the thesis: Designing and assessing a pi-calculus evaluator in Haskell using the nom package [[1]](#1). 

### Installation
Assuming [Stack](https://www.haskellstack.org/) is installed.

    git clone git@github:ainac99/pi-calc-lts
    cd pi-calc-lts
    stack install

### Usage

Once installed, you can define your processes in app/Main.hs.

    stack build
    stack exec pi-calc-lts-exe

Processes are evaluated using the labelled transition system presented in [[2]](#2) and printed to the 
command-line.



## References
<a id="1">[1]</a> 
Centelles Tarrés, A. (2022). 
Designing and assessing a pi-calculus evaluator in Haskell using the nom package. 
MSc thesis, Heriot-Watt University.

<a id="2">[2]</a> 
Gabbay, M.J. (2003). 
The pi-calculus in FM. 
Thirty Five Years of Automatic Mathematics, 28, 247-269.

<a id="3">[3]</a> 
Gabbay, M.J. (2020). 
nom: name-binding & alpha-equivalence (version 0.1.0.2).
Available at: https://hackage.haskell.org/package/nom

