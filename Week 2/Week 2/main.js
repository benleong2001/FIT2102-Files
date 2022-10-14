// Surname     | Firstname        | email                       | Contribution% | Any issues?
// ==========================================================================================
// Leong       | Benjamin Tjen Ho | bleo0009@student.monash.edu | 20%           |
// Kim         | Tae Jun          | tkim0018@student.monash.edu | 20%           |
// Lim         | Hao Wei          | hlim0042@student.monash.edu | 20%           |
// Lee         | Sing Yuan        | slee0163@student.monash.edu | 20%           |
// Chen        | Darren Hong Weng | dche0077@student.monash.edu | 20%           |
//
// complete Worksheet 2 by entering code in the places marked below...
//
// For full instructions and tests open the file worksheetChecklist.html
// in Chrome browser.  Keep it open side-by-side with your editor window.
// You will edit this file (main.js), save it, and reload the
// browser window to run the test.

/**
 * Exercise 1
 */
// Initialising an object with properties:
// - aProperty
// - anotherProperty
const myObj = {
  aProperty: "hello world",
  anotherProperty: 2102
}

/**
 * Exercise 2
 */
// The main function takes in parameter f
// It then returns a function that receives a parameter x
// Which then returns another function to receive a parameter y
// Which then finally returns the output of function f with input x and y

// i.e.
// function(f)
// - function(x) 
// - - function(y)
// - - - return f(x, y)
const operationOnTwoNumbers = (f) => (x) => (y) => f(x, y);

/**
 * Exercise 3
 */
// Going through the array and for each function (element) in the array, 
// the function is called
const callEach = (array) => array.forEach((fun) => fun());

/**
 * Exercise 4
 */
// i) Mapping a basic summing function to all elements in the array parameter
const addN = (n, array) => array.map(operationOnTwoNumbers((x, y) => x + y)(n));

// ii) Filtering the array by checking if the element is even or not
const getEvens = (array) => array.filter((n) => !(n % 2));

// iii) First filtering out all 0s by checking their boolean value
// Then reducing the array by mapping a basic multiplying function to the remaining elements
const multiplyArray = (array) => array.filter(Boolean).reduce((x, y) => x * y);

/**
 * Exercise 5
 */
// Concatenating all values from n-1 to 0
// and stopping once n reaches 0
const range = (n) => (n ? range(n - 1).concat(n - 1) : []);
function range1(n, x=0, list=[]){
  if (n === 0){
    return list
  } else if (x === n){
    return list
  } else {
    let newlist = list.concat(x)
    // console.log(newlist)
    x = x + 1
    console.log(x)
    return range1(n, x, newlist) 
  }
}

// console.log(range1(100))

/**
 * Exercise 6
 */
// Going through an array of numbers from 0 to 999
// and filtering only multiples of 3 and 5
// then reducing the array by summing all the multiples
const Euler1 = () =>
  range(1000)
    .filter((n) => !(n % 3) || !(n % 5))
    .reduce((x, y) => x + y);

/**
 * Exercise 7
 */
// Infinite Series Calculator Function
const infinite_series_calculator =
  (accumulate) => (predicate) => (transform) => (n) =>
    range(n).filter(predicate).map(transform).reduce(accumulate);

/**
 * Exercise 8
 */
// Calculating each Pi term function
const calculatePiTerm = (n) => (4 * n ** 2) / (4 * n ** 2 - 1);

// Function to check if the value is 0 or not
const skipZero = (x) => Boolean(x);

// Basic multiplying function
const productAccumulate = (x, y) => x * y;

// Function to calculate an approximation of pi
const calculatePi = (n) =>
  2 *
  infinite_series_calculator(productAccumulate)(skipZero)(calculatePiTerm)(n);

// Extra: Finding the minimum value of n 
// for a difference between approximation and actual value of pi < 0.01
const findPiLimit = (n = 2) =>
  Math.abs(calculatePi(n) - Math.PI) < 0.01 ? n : findPiLimit(n + 1);
const nPiLimit = findPiLimit();

// Getting an accurate value of pi
const pi = calculatePi(nPiLimit);

/**
 * Exercise 9
 */
// Factorial function
const factorial = (n) => (n ? n * factorial(n - 1) : 1);

// Calculating each E term function
const calculateETerm = (n) => (2 * (n + 1)) / factorial(2 * n + 1);

// Basic summing function
const sumAccumulate = (x, y) => x + y;

// Function to always return true
const alwaysTrue = () => true;

// Sum Series Calculator function
const sum_series_calculator = (transform) => (n) =>
  range(n).map(transform).reduce(sumAccumulate);

// Function to calculate an approximation of e
const calculateE = (n) => sum_series_calculator(calculateETerm)(n);

// Extra: Finding the minimum value of n 
// for a difference between approximation and actual value of e < 0.01
const findELimit = (n = 2) =>
  Math.abs(calculateE(n) - Math.E) < 0.01 ? n : findELimit(n + 1);
const nELimit = findELimit();

// Getting an accurate value of e
const e = calculateE(nELimit);

/**
 * Exercise 10
 */
// Calculating each Sin term function
const calculateSinTerm = (x) => (n) =>
  ((-1) ** n * x ** (2 * n + 1)) / factorial(2 * n + 1);

// Calculating an approximation of sin(x)
const calculateSin = (n) => (x) =>
  sum_series_calculator(calculateSinTerm(x))(n);

// Using approximatino of up to 700 terms
const sin = (x) => calculateSin(700)(x);
