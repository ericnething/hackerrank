"use strict";
const R = require("ramda");

// getPairs : a -> Array a -> Array (Array a)
function getPairs(a, bs) {
    return R.map (b => [a,b]) (bs);
}
// arrayWithoutIndex : Number -> Array a -> Array a
function arrayWithoutIndex(index, array) {
    const left  = R.slice(0,index, array);
    const right = R.slice(index+1, Infinity, array);
    return R.concat (left) (right);
}

// getAllPairs : Array a -> Array (Array a)
function getAllPairs(array) {
    let result = []
    for (let i = 0; i < array.length; ++i) {
        let pairs = getPairs(array[i], arrayWithoutIndex(i, array));
        result = R.concat (result) (pairs);
    }
    return result;
}

// getPairEqualTo : Number -> Array (Array Number) -> Array Number
function getPairEqualTo(k, array) {
    for ( let i = 0; i < array.length; ++i ) {
        if (R.sum(array[i]) === k) {
            // found it
            return array[i];
        }
    }
    // not found
    return null;
}

function main() {
    let k = 4;
    let array = [1,4,5,3,2];
    let result = R.compose (R.curry (getPairEqualTo) (k), getAllPairs) (array);
    console.log("The values at index %j,%j ([%s]) sum to %j",
                array.indexOf(result[0]),
                array.indexOf(result[1]),
                result,
                k
               );
}

main();
