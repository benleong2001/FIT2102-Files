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
 * Complete Worksheet 4/5 by entering code in the places marked below...
 *
 * For full instructions and tests open the file observableexamples.html
 * in Chrome browser.  Keep it open side-by-side with your editor window.
 * You will edit this file (observableexamples.ts), save it, build it, and reload the
 * browser window to run the test.
 */

import { interval, fromEvent} from "rxjs";
import { map, filter, take, mergeMap, takeUntil, scan } from "rxjs/operators";

// Simple demonstration
// ===========================================================================================
// ===========================================================================================
/**
 * an example of traditional event driven programming style - this is what we are
 * replacing with observable.
 * The following adds a listener for the mouse event
 * handler, sets p and adds or removes a highlight depending on x position
 */
function mousePosEvents() {
  const pos = document.getElementById("pos")!;

  document.addEventListener("mousemove", ({ clientX, clientY }) => {
    const p = clientX + ", " + clientY;
    pos.innerHTML = p;
    if (clientX > 400) {
      pos.classList.add("highlight");
    } else {
      pos.classList.remove("highlight");
    }
  });
}

/**
 * constructs an Observable event stream with three branches:
 *   Observable<x,y>
 *    |- set <p>
 *    |- add highlight
 *    |- remove highlight
 */
function mousePosObservable() {
  const pos = document.getElementById("pos")!,
    o = fromEvent<MouseEvent>(document, "mousemove").pipe(
      map(({ clientX, clientY }) => ({ x: clientX, y: clientY }))
    );

  o.pipe(map(({ x, y }) => `${x},${y}`)).subscribe(
    (s: string) => (pos.innerHTML = s)
  );

  o.pipe(filter(({ x }) => x > 400)).subscribe((_) =>
    pos.classList.add("highlight")
  );

  o.pipe(filter(({ x }) => x <= 400)).subscribe(({ x, y }) => {
    pos.classList.remove("highlight");
  });


}

// Exercise 5
// ===========================================================================================
// ===========================================================================================
function piApproximation() {
  // a simple, seedable, pseudo-random number generator
  class RNG {
    // LCG using GCC's constants
    m = 0x80000000; // 2**31
    a = 1103515245;
    c = 12345;
    state: number;
    constructor(seed: number) {
      this.state = seed ? seed : Math.floor(Math.random() * (this.m - 1));
    }
    nextInt() {
      this.state = (this.a * this.state + this.c) % this.m;
      return this.state;
    }
    nextFloat() {
      // returns in range [0,1]
      return this.nextInt() / (this.m - 1);
    }
  }

  const resultInPage = document.getElementById("value_piApproximation"),
    canvas = document.getElementById("piApproximationVis");

  if (!resultInPage || !canvas) {
    console.log("Not on the observableexamples.html page");
    return;
  }

  // Some handy types for passing data around
  type Colour = "red" | "green";
  type Dot = { x: number; y: number; colour?: Colour };
  interface Data {
    point?: Dot;
    insideCount: number;
    totalCount: number;
  }

  // an instance of the Random Number Generator with a specific seed
  const rng = new RNG(20);
  // return a random number in the range [-1,1]
  const nextRandom = () => rng.nextFloat() * 2 - 1;
  // you'll need the circleDiameter to scale the dots to fit the canvas
  const circleRadius = Number(canvas.getAttribute("width")) / 2;
  // test if a point is inside a unit circle
  const inCircle = ({ x, y }: Dot) => x ** 2 + y ** 2 <= 1;
  // you'll also need to set innerText with the pi approximation
  resultInPage.innerText =
    "...Update this text to show the Pi approximation...";

  const piApproximationData: Data = {
    insideCount: 0,
    totalCount: 0,
  };
  // Your code starts here!
  // =========================================================================================
  function createDot(d: Dot) {
    /* what parameters do we need to plot dots at different locations and in red or green? Use the types above! */
    if (!canvas) throw "Couldn't get canvas element!";
    const dot = document.createElementNS(canvas.namespaceURI, "circle");

    // Set circle properties
    dot.setAttribute("cx", String(150 * (d.x + 1)));
    dot.setAttribute("cy", String(150 * (d.y + 1)));
    dot.setAttribute("r", "5");
    dot.setAttribute("fill", d.colour)
    // Add the dot to the canvas
    canvas.appendChild(dot);
  }
  // createDot()
  // A stream of random numbers
  const randomNumberStream = interval(1).pipe(
    map(nextRandom),
    scan(({ x, y, colour }, num) => ({ x: y, y: num, colour: (inCircle({ x: y, y: num }) ? "green" : "red") as Colour }),
      <Dot>{ x: 0, y: nextRandom(), colour: "red" }),
  )

  randomNumberStream.subscribe((dot) => {
    createDot(dot);

    piApproximationData.insideCount = piApproximationData.insideCount + Number(inCircle(dot))
    piApproximationData.totalCount = piApproximationData.totalCount + 1

    resultInPage.innerText =
      "\n" + piApproximationData.insideCount +
      " green points inside the circle.\n" +
      (piApproximationData.totalCount - piApproximationData.insideCount) +
      " red points outside the circle.\n" +
      piApproximationData.totalCount +
      " total points.\n\n" +
      "π ≈ Points inside circle / Total points\n= 4 * " +
      piApproximationData.insideCount +
      "/" +
      piApproximationData.totalCount +
      "\n= " +
      (4 * piApproximationData.insideCount) / piApproximationData.totalCount;
  });
}

// Exercise 6
// ===========================================================================================
// ===========================================================================================
/**
 * animates an SVG rectangle, passing a continuation to the built-in HTML5 setInterval function.
 * a rectangle smoothly moves to the right for 1 second.
 */
function animatedRectTimer() {
  // get the svg canvas element
  const svg = document.getElementById("animatedRect")!;
  // create the rect
  const rect = document.createElementNS(svg.namespaceURI, "rect");
  Object.entries({
    x: 100,
    y: 70,
    width: 120,
    height: 80,
    fill: "#95B3D7",
  }).forEach(([key, val]) => rect.setAttribute(key, String(val)));
  svg.appendChild(rect);

  const animate = setInterval(
    () => rect.setAttribute("x", String(1 + Number(rect.getAttribute("x")))),
    10
  );
  const timer = setInterval(() => {
    clearInterval(animate);
    clearInterval(timer);
  }, 1000);
}

/**
 * Demonstrates the interval method
 * You want to choose an interval so the rectangle animates smoothly
 * It terminates after 1 second (1000 milliseconds)
 */
function animatedRect() {
  // Your code starts here!
  // =========================================================================================
  // get the svg canvas element
  const svg = document.getElementById("animatedRect")!;
  // create the rect
  const rect = document.createElementNS(svg.namespaceURI, "rect");
  Object.entries({
    x: 100,
    y: 70,
    width: 120,
    height: 80,
    fill: "#95B3D7",
  }).forEach(([key, val]) => rect.setAttribute(key, String(val)));
  svg.appendChild(rect);

  // Stream of numbers every 10 ms
  interval(10)
    // Convert the stream of numbers to a stream of 1s
    .pipe(map(_ => 1), take(100))
    .subscribe((val) =>
      // Add the x coordinate of the rectangle by 1 (from the stream) 
      rect.setAttribute("x", String(val + Number(rect.getAttribute("x"))))
    );
}

// Exercise 7
// ===========================================================================================
// ===========================================================================================
/**
 * Create and control a rectangle using the keyboard! Use only one subscribe call and not the interval method
 * If statements
 */
function keyboardControl() {
  // get the svg canvas element
  const svg = document.getElementById("moveableRect")!;


  // Your code starts here!
  // =========================================================================================
  // create the rect

  type Direction = "vertical" | "horizontal"

  const rect = document.createElementNS(svg.namespaceURI, "rect");
  Object.entries({
    x: 100,
    y: 70,
    width: 120,
    height: 80,
    fill: "#95B3D7",
  }).forEach(([key, val]) => rect.setAttribute(key, String(val)));
  svg.appendChild(rect);

  // Used to animate the rectangle moving
  const animate = (axis: string) => (val: number) => {
    const point: number = Number(rect.getAttribute(axis)) + val
    const comparison: number = -rect.getAttribute(axis === 'x' ? "width" : "height")
    const size: number = axis === 'x' ? svg.getBoundingClientRect().width : svg.getBoundingClientRect().height
    rect.setAttribute(axis, point < comparison ? String(point + size - comparison) : point > size ? String(point - size + comparison) : String(point))
  }
  // Creating a stream of when a key is pressed
  const keyDown$ = fromEvent<KeyboardEvent>(document, 'keydown'),
    keyUp$ = fromEvent<KeyboardEvent>(document, 'keyup')

  // Modularised function to create a stream of 1s or -1s depending on the WASD key pressed
  const keyWASD = (dir: Direction) => keyDown$.pipe(
    filter(({ key }) => dir === "vertical" ? key === 'w' || key === 's' : key === 'a' || key === 'd'),
    filter(({ repeat }) => !repeat)).pipe(
      mergeMap(keyPress => interval(10).pipe(
        takeUntil(keyUp$.pipe(
          filter(({ key }) => key === keyPress.key)
        )), map(_ => keyPress)
      )), map((keyPress) => dir === "vertical" ?
        keyPress.key === 'w' ? -1 : 1
        : keyPress.key === 'a' ? -1 : 1)
    )

  // Initialising 2 streams, 1 for vertical movement, 1 for horizontal
  const verticalPress$ = keyWASD("vertical"),
    horizontalPress$ = keyWASD("horizontal")

  // Converting the stream of numbers to movement
  verticalPress$.subscribe(animate('y'))
  horizontalPress$.subscribe(animate('x'))
}

// Running the code
// ===========================================================================================
// ===========================================================================================
document.addEventListener("DOMContentLoaded", function (event) {
  piApproximation();

  // compare mousePosEvents and mousePosObservable for equivalent implementations
  // of mouse handling with events and then with Observable, respectively.
  //mousePosEvents();
  mousePosObservable();

  // animatedRectTimer();
  animatedRect();
  // replace the above call with the following once you have implemented it:
  //animatedRect()
  keyboardControl();
  
});
