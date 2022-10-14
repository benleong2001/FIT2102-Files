/**
 * Surname     | Firstname        | email                       | Contribution% | Any issues?
 * ==========================================================================================
 * Leong       | Benjamin Tjen Ho | bleo0009@student.monash.edu | 20%           |
 * Kim         | Tae Jun          | tkim0018@student.monash.edu | 20%           |
 * Lim         | Hao Wei          | hlim0042@student.monash.edu | 20%           |
 * Lee         | Sing Yuan        | slee0163@student.monash.edu | 20%           |
 * Chen        | Darren Hong Weng | dche0077@student.monash.edu | 20%           |
 *
 * Please do not hesitate to contact your tutors if there are
 * issues that you cannot resolve within the group.
 *
 * Complete Worksheet 4 by entering code in the places marked below...
 *
 * For full instructions and tests open the file worksheetChecklist.html
 * in Chrome browser.  Keep it open side-by-side with your editor window.
 * You will edit this file (main.ts), save it, build it, and reload the
 * browser window to run the test.
 */
/**
 * Replace references to IMPLEMENT_THIS with your code!
 */
const IMPLEMENT_THIS = undefined;
// Implement the function:
// function initSequence<T>(
//   transform: (value: T) => T
// ): (initialValue: T) => LazySequence<T> {
//   return function _next(v: T): LazySequence<T> {
//     return {
//       value: v,
//       next: () => _next(transform(v)),
//     };
//   };
// }
const initSequence = (transform) => function _next(v) {
    return { value: v, 
        next: () => _next(transform(v)) };
};
/**
 *  Exercise 2 - map, filter, take, reduce
 */
const map = (func, seq) => ({
    value: func(seq.value),
    next: () => map(func, seq.next()),
});
// function filter<T>(
//   func: (v: T) => boolean,
//   seq: LazySequence<T>
// ): LazySequence<T> {
//   return func(seq.value)
//     ? {
//         value: seq.value,
//         next: () => filter(func, seq.next()),
//       }
//     : filter(func, seq.next());
// }
const filter = (func, seq) => func(seq.value)
    ? { value: seq.value, next: () => filter(func, seq.next()) }
    : filter(func, seq.next());
/**
 * Creates a sequence of finite length (terminated by undefined) from a longer or infinite sequence.
 * Take returns a sequence that contains the specified number of elements of the sequence, and then 'undefined'.
 * That is, the next attribute of the last element in the returned sequence, will be a function that returns 'undefined'.
 *
 * @param n number of elements to return before returning undefined
 * @param seq the sequence
 */
const take = (n, seq) => n <= 0
    ? undefined
    : {
        value: seq.value,
        next: () => take(n - 1, seq.next()),
    };
/**
 * We have to cast the type here due to the limitations of the TypeScript type system.
 * If you have to type cast something, make sure to justify it in the comments.
 */
/**
 * reduce a finite sequence to a value using the specified aggregation function
 * @param func aggregation function
 * @param seq either a sequence or undefined if we have reached the end of the sequence
 * @param start starting value of the reduction past as first parameter to first call of func
 */
function reduce(func, seq, start) {
    return seq ? reduce(func, seq.next(), func(start, seq.value)) : start;
}
function reduceRight(f, seq, start) {
    return seq ? f(reduceRight(f, seq.next(), start), seq.value) : start;
}
/**
 *  Exercise 3 - Reduce Practice
 */
function maxNumber(lazyList) {
    // ******** YOUR CODE HERE ********
    // Use __only__ reduce on the
    // lazyList passed in. The lazyList
    // will terminate so don't use `take`
    // inside this function body.
    return reduce((max, elem) => (max > elem ? max : elem), lazyList, -Infinity);
}
function lengthOfSequence(lazyList) {
    // ******** YOUR CODE HERE ********
    // Again only use reduce and don't
    // use `take` inside this function.
    return reduce((count, _) => ++count, lazyList, 0);
}
function toArray(seq) {
    // ******** YOUR CODE HERE ********
    // Again only use reduce and don't
    // use `take` inside this function.
    return reduce((arr, elem) => [...arr, elem], seq, []);
}
/**
 *  Exercise 4 - Lazy Pi Approximations
 */
function exercise4Solution(seriesLength) {
    // Your solution using lazy lists.
    // Use `take` to only take the right amount of the infinite list.
    return reduce((acc, term) => acc + term, take(seriesLength, initSequence((num) => -num / (Math.abs(2 * num) + 1))(1)), 0);
}
//# sourceMappingURL=sequences.js.map

const someObj = [1, 2, "hello"]
const someVar = (function (){return console.log(...someObj, "hello")})()
console.log(someVar)