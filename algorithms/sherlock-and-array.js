'use strict'
const _ = require("lodash")

function sum(arr) {
    return _.reduce( arr, (acc, a) => a + acc, 0)
}

function sherlock(arr) {
    for (let i=0; i < arr.length; ++i) {
        let left  = _.slice(arr, 0, i);
        let right = _.slice(arr, i+1, arr.length);
        console.log("%j: [%s] [%s]", i, left.toString(), right.toString());
        if ( sum(left) === sum(right) ) {
            return i;
        }
    }
    return null;
}

function main(arr) {
    console.log("Array: [%s]", arr.toString());
    let result = sherlock(arr);
    if (result != null) {
        console.log("YES");
    }
    else {
        console.log("NO");
    }
}

main([1,2,3]);
main([3,3,4,3,2,1]);
