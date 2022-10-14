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
 * Complete the worksheet by entering code in the places marked below...
 *
 * For full instructions and tests open the file worksheetChecklist.html
 * in Chrome browser.  Keep it open side-by-side with your editor window.
 * You will edit this file, save it, compile it, and reload the
 * browser window to run the test.
 */
// Stub value to indicate an implementation
const IMPLEMENT_THIS = undefined;
/*****************************************************************
 * Exercise 1
 */
// Just adding the types of the parameters and the output
function addStuff(a, b) {
    return a + b;
}
function numberToString(input) {
    return JSON.stringify(input);
}
const arr = [0];
console.log(arr.slice(1));
/**
 * Takes a string and adds "padding" to the left.
 * If 'padding' is a string, then 'padding' is appended to the left side.
 * If 'padding' is a number, then that number of spaces is added to the left side.
 */
function padLeft(value, padding) {
    if (typeof padding === "number") {
        return Array(padding + 1).join(" ") + value;
    }
    if (typeof padding === "string") {
        return padding + value;
    }
    throw new Error(`Expected string or number, got '${padding}'.`);
}
padLeft("Hello world", 4); // returns "    Hello world"
// What's the type of arg0 and arg1?
function curry(f) {
    return function (x) {
        return function (y) {
            return f(x, y);
        };
    };
}
/**
 * cons "constructs" a list node, if no second argument is specified it is the last node in the list
 */
function cons(head, rest) {
    return (selector) => selector(head, rest);
}
/**
 * head selector, returns the first element in the list
 * @param list is a Cons (note, not an empty ConsList)
 */
function head(list) {
    if (!list)
        throw new TypeError("list is null");
    return list((head, rest) => head);
}
/**
 * rest selector, everything but the head
 * @param list is a Cons (note, not an empty ConsList)
 */
function rest(list) {
    if (!list)
        throw new TypeError("list is null");
    return list((head, rest) => rest);
}
/**
 * Use this as an example for other functions!
 * @param f Function to use for each element
 * @param list Cons list
 */
function forEach(f, list) {
    if (list) {
        f(head(list));
        forEach(f, rest(list));
    }
}
/**
 * Implement this function! Also, complete this documentation (see forEach).
 */
/**
 * @param f Function to use for each element
 * @param l Cons list input
 * @returns Cons list after Function f has been applied to elements in Cons list input l
 */
function map(f, l) {
    // If input l is null then we just return null
    // else, we combine f(head) with map(rest) 
    //    using cons() function to combine them
    // and then return that^
    return l ? cons(f(head(l)), map(f, rest(l))) : null;
}
/*****************************************************************
 * Exercise 3
 */
/**
 * Converting an array into its Cons List equivalent
 * @param arr Array input
 * @returns Cons list equivalent of the array input
 */
// When using cons(), we are adding elements to the head
// So if we want to create a ConsList from an array, 
//  we need to add elements from right to left
// so, we use reduceRight() on the input array
// let arr = [0, 1, 2] 
// 1st call = cons(2, null) = [2]
// 2nd call = cons(1, 1st call) = [1, 2]
// 3rd call = cons(0, 2nd call) = [0, 1, 2]
const fromArray = (arr) => arr.reduceRight((acc, elem) => cons(elem, acc), null);
/**
 * Reducing a list from left to right using an accumulating function
 * @param f Function to reduce the input cons list
 * @param init Initial value when reducing the list
 * @param l Input cons list
 * @returns The accumulated value after reducing the elements of the input cons list
 */
// Using recursion to accumulate all the elements using function f
// The base case is when the list is empty and so we return the initial value
// Note: When recursively calling, the new initial value is 
//      the accumulated value from the previous call
//      i.e., f(init, head(l))
const reduce = (f, init, l) => l ? reduce(f, f(init, head(l)), rest(l)) : init;
/**
 * Reducing a list from right to left using an accumulating function
 * @param f Function to reduce the input cons list.
 * @param init Initial value when reducing the input cons list.
 * @param l Input cons list.
 * @returns The accumulated value after reducing the input cons list from right to left.
 */
// Using reduce() but on the reverse of l
const reduceRight = (f, init, l) => reduce(f, init, reverse(l));
/**
 * Creates a new Cons List with all valid elements from the input Cons List
 * @param f Function that returns a boolean stating if an element is valid
 * @param l Input Cons List to be filtered
 * @returns Filtered Cons List
 */
// Checking if f(head) is true
// If it is, then we include it into the output list
// else, we just continue to filter the rest i.e., filter(rest)
const filter = (f, l) => l
    ? f(head(l))
        ? cons(head(l), filter(f, rest(l)))
        : filter(f, rest(l))
    : null;
/**
 * Concatenates 2 Cons Lists together
 * @param l1 First Cons List to be concatenated
 * @param l2 Second Cons List to be concatenated
 * @returns Concatenated Cons List
 */
// A bit of a long process
// But we use cons() to create our list
// We do this by looping through the entirety of l1
// and then l2
const concat = (l1, l2) => l1
    ? cons(head(l1), concat(rest(l1), l2))
    : l2
        ? cons(head(l2), concat(l1, rest(l2)))
        : null;
/**
 * Reverses a Cons List
 * @param l Input Cons List to be reversed
 * @returns Reversed Cons List
 */
// concat() the reverse of rest and the current head
// [0, 1, 2]
// reverse([0, 1, 2]) = concat(reverse([1, 2]), [0]) = concat([2, 1], [0]) = [2, 1, 0]
// reverse([1, 2]) = concat(reverse([2]), [1]) = concat([2], [1]) = [2, 1]
// reverse([2]) = concat(reverse([]), [2]) = concat(null, [2]) = [2]
const reverse = (l) => l ? concat(reverse(rest(l)), cons(head(l), null)) : null;
// Example use of reduce
function countLetters(stringArray) {
    const list = fromArray(stringArray);
    return reduce((len, s) => len + s.length, 0, list);
}
console.log(countLetters(["Hello", "there!"]));
/*****************************************************************
 * Exercise 4
 *
 * Tip: Use the functions in exercise 3!
 */
/**
 * A linked list backed by a ConsList
 */
class List {
    constructor(list) {
        if (list instanceof Array) {
            // IMPLEMENT THIS. What goes here ??
            // Converting Array to Cons List using fromArray()
            list = fromArray(list);
            this.head = list !== null && list !== void 0 ? list : null;
        }
        else {
            // nullish coalescing operator
            // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Nullish_coalescing_operator
            this.head = list !== null && list !== void 0 ? list : null;
        }
    }
    /**
     * create an array containing all the elements of this List
     */
    toArray() {
        // Getting type errors here?
        // Make sure your type annotation for reduce()
        // in Exercise 3 is correct!
        return reduce((a, t) => a.concat(t), [], this.head);
    }
    // Add methods here:
    // Basically, for all of these methods, we are just using the functions from exercise 3
    /**
     * Map function of the List<T> class
     * @param f Function to map the elements in the list to
     * @returns A new list with the mapped values
     */
    map(f) {
        return new List(map(f, this.head));
    }
    /**
     * forEach function of the List<T> class
     * @param f Function to apply to each element of the list
     * @returns The original list
     */
    forEach(f) {
        forEach(f, this.head);
        // We need this line as for this implementation, we need to return the original list
        return this;
    }
    /**
     * Filter function of the List<T> class
     * @param f Filtering function
     * @returns New list with the elements after filtering
     */
    filter(f) {
        return new List(filter(f, this.head));
    }
    /**
     * Reduce function of the List<T> class
     * @param f Accumulating function
     * @param init Initial value of the accumulator
     * @returns The value after accumulating the entire list
     */
    reduce(f, init) {
        return reduce(f, init, this.head);
    }
    /**
     * Concat function of the List<T> class
     * @param l A list to concatenate the original list with
     * @returns The lists concatenated
     */
    concat(l) {
        return new List(concat(this.head, l.head));
    }
    // Only used for debugging
    // Not part of the tutorial exercises
    getHead() {
        return this.head;
    }
}
console.log((new List(cons(undefined, null))).toArray());
/*****************************************************************
 * Exercise 5
 */
/**
 * Converting a string into a list (line)
 * @param s A string value
 * @returns A list in the format [0, string_input_value]
 */
// If you don't understand, feel free to ask in group/dm
const line = (s) => [0, s];
/**
 * Converts a list (line) into a List<T> object equivalent
 * @param line A list (line) to be converted
 * @returns A List<T> object equivalent to the input l
 */
// Creating a List<T> instance using the line input
const lineToList = (line) => new List([line]);
/*****************************************************************
 * Exercise 6
 */
// function nest(
//   indent: number,
//   layout: List<[number, string]>
// ): List<[number, string]>{
//   return layout.map((elem: [number, string]) => [elem[0] + indent, elem[1]])
// };
/**
 * A nest function to 'indent' the lines in layout by indent
 * @param indent The amount of indent to nest the lines by
 * @param layout A List<T> of lists
 * @returns The List<T> of lists after indenting by indent amount
 */
// layout here is a List<T> of lists/lines
// so we can use the map function and in this map function
//    we increase the <number> part of the line by indent value
const nest = (indent, layout) => layout.map((elem) => [elem[0] + indent, elem[1]]);
class BinaryTreeNode {
    constructor(data, leftChild, rightChild) {
        this.data = data;
        this.leftChild = leftChild;
        this.rightChild = rightChild;
    }
}
// example tree:
const myTree = new BinaryTreeNode(1, new BinaryTreeNode(2, new BinaryTreeNode(3)), new BinaryTreeNode(4));
// *** uncomment the following code once you have implemented List and nest function (above) ***
function prettyPrintBinaryTree(node) {
    if (!node) {
        return new List([]);
    }
    const thisLine = lineToList(line(node.data.toString())), leftLines = prettyPrintBinaryTree(node.leftChild), rightLines = prettyPrintBinaryTree(node.rightChild);
    return thisLine.concat(nest(1, leftLines.concat(rightLines)));
}
const output = prettyPrintBinaryTree(myTree)
    .map((aLine) => new Array(aLine[0] + 1).join("-") + aLine[1])
    .reduce((a, b) => a + "\n" + b, "")
    .trim();
console.log(output);
/*****************************************************************
 * Exercise 7: Implement prettyPrintNaryTree, which takes a NaryTree as input
 * and returns a list of the type expected by your nest function
 */
class NaryTree {
    constructor(data, children = new List(undefined)) {
        this.data = data;
        this.children = children;
    }
}
// Example tree for you to print:
const naryTree = new NaryTree(1, new List([
    new NaryTree(2),
    new NaryTree(3, new List([new NaryTree(4)])),
    new NaryTree(5),
]));
// Implement: function prettyPrintNaryTree(...)
/**
 * Converting a NaryTree which may have more than 2 children into a List<T> of lists
 * @param node The root of a NaryTree
 * @returns The List<T> of lists equivalent of input node
 */
// For each NaryTree, there are 2 parts
//    The root/head (its .data property)
//    It's children
// So what we are doing here is we take the head and convert into a string
//    then using lineToList(line()) to convert it into a List<T> object
// With this List<T>, we concat() it with its children
// The .children property is basically a List<T> of NaryTrees
//    So for each of the NaryTrees, we call back this method and it repeats again for each child
//    We also nest the children so that each child has a <number> value 1 greater than its parent
function prettyPrintNaryTree(node) {
    return node
        ? lineToList(line(node.data.toString())).concat(nest(1, node.children.reduce((a, b) => a.concat(prettyPrintNaryTree(b)), new List([]))))
        : new List([]);
}
// *** uncomment the following code once you have implemented prettyPrintNaryTree (above) ***
const outputNaryTree = prettyPrintNaryTree(naryTree)
    .map((aLine) => new Array(aLine[0] + 1).join("-") + aLine[1])
    .reduce((a, b) => a + "\n" + b, "")
    .trim();
console.log(outputNaryTree);
const jsonPrettyToDoc = (json) => {
    if (Array.isArray(json)) {
        // Handle the Array case.
        return lineToList(line("[")) // [0, '[']
            .concat(nest(1, json
            .join(" , ")
            .split(" ")
            .reduce((a, b) => a.concat(jsonPrettyToDoc(b)), new List([]))))
            .concat(lineToList(line("]"))); // [0, ']']
    }
    else if (typeof json === "object" && json !== null) {
        // Handle the object case.
        // Hint: use Object.keys(json) to get a list of
        // keys that the object has.
        return lineToList(line("{"))
            .concat(nest(1, new List(Object.keys(json)).reduce((acc, x) => x != ","
            ? acc
                .concat(jsonPrettyToDoc(x + ":"))
                .concat(jsonPrettyToDoc(json[x]))
            : acc.concat(jsonPrettyToDoc(x)), new List([]))))
            .concat(lineToList(line("}")));
    }
    else if (typeof json === "string") {
        // Handle string case.
        return lineToList(line(json));
    }
    else if (typeof json === "number") {
        // Handle number
        return lineToList(line(json.toString()));
    }
    else if (typeof json === "boolean") {
        // Handle the boolean case
        return lineToList(line(json.toString()));
    }
    else if (json === null) {
        // Handle the null case
        return lineToList(line("null"));
    }
    // Default case to fall back on.
    return new List([]);
};
// *** uncomment the following code once you are ready to test your implemented jsonPrettyToDoc ***
const json = {
    unit: "FIT2102",
    year: 2021,
    semester: "S2",
    active: true,
    assessments: {
        week1: null,
        week2: "Tutorial 1 Exercise",
        week3: "Tutorial 2 Exercise",
    },
    languages: ["Javascript", "Typescript", "Haskell", "Minizinc"],
};
function lineIndented(aLine) {
    return new Array(aLine[0] + 1).join("    ") + aLine[1];
}
function appendLine(acc, nextLine) {
    return nextLine.slice(-1) === ","
        ? acc + nextLine.trim()
        : acc.slice(-1) === ":"
            ? acc + " " + nextLine.trim()
            : acc + "\n" + nextLine;
}
console.log(jsonPrettyToDoc(json).map(lineIndented).reduce(appendLine, "").trim());
// *** This is what it should look like in the console ***
// {
//     unit: FIT2102,
//     year: 2021,
//     semester: S2,
//     active: true,
//     assessments: {
//         week1: null,
//         week2: Tutorial 1 Exercise,
//         week3: Tutorial 2 Exercise
//     },
//     languages: [
//         Javascript,
//         Typescript,
//         Haskell,
//         Minizinc
//     ]
// }
//# sourceMappingURL=main.js.map