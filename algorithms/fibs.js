"use strict";

// fibs : Number -> Array Number
function fibs (n) {
    let array = [];
    let go = function (a, b, n) {
        if (n <= 0) {
            return;
        }
        array.push(a);
        return go (b, (a+b), n-1);
    }
    go (0, 1, n);
    return array;
}

// main : IO ()
function main () {
    let num = 100;
    let nFibs = fibs (num);
    console.log("The first %j fibonacci numbers are %s", num, nFibs.toString());
}

main();
